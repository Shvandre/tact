import { Address, Cell, toNano } from "@ton/core";
import { CompilerContext } from "../context";
import { idTextErr, throwInternalCompilerError } from "../errors";
import {
    AstBinaryOperation,
    AstBoolean,
    AstExpression,
    AstFieldAccess,
    AstFunctionDef,
    AstId,
    AstMethodCall,
    AstNull,
    AstNumber,
    AstOpBinary,
    AstOpUnary,
    AstStatementAugmentedAssign,
    AstStatementLet,
    AstStaticCall,
    AstString,
    AstStructInstance,
    AstUnaryOperation,
    eqNames,
    idText,
    isSelfId,
    SrcInfo,
} from "../grammar/ast";
import {
    getStaticConstant,
    getStaticFunction,
    getType,
    hasStaticConstant,
    hasStaticFunction,
} from "../types/resolveDescriptors";
import { getExpType } from "../types/resolveExpression";
import { CommentValue, showValue, StructValue, Value } from "../types/types";
import { InterpreterSemantics } from "./types";
import { sha256_sync } from "@ton/crypto";
import { enabledMasterchain } from "../config/features";
import { dummySrcInfo } from "../grammar/grammar";
import {
    divFloor,
    modFloor,
    throwErrorConstEval,
    throwNonFatalErrorConstEval,
} from "./util";

// TVM integers are signed 257-bit integers
const minTvmInt: bigint = -(2n ** 256n);
const maxTvmInt: bigint = 2n ** 256n - 1n;

// Range allowed in repeat statements
const minRepeatStatement: bigint = -(2n ** 256n); // Note it is the same as minimum for TVM
const maxRepeatStatement: bigint = 2n ** 31n - 1n;

class ReturnSignal extends Error {
    private value?: Value;

    constructor(value?: Value) {
        super();
        this.value = value;
    }

    public getValue(): Value | undefined {
        return this.value;
    }
}

type InterpreterConfig = {
    // Options that tune the interpreter's behavior.

    // Maximum number of iterations inside a loop before a time out is issued.
    // This option only applies to: do...until and while loops
    maxLoopIterations: bigint;
};

const WILDCARD_NAME: string = "_";

/* An Environment consists of a map of variable names to their values 
(which have the generic type V), and a reference to the (optional) parent 
environment. In other words, an Environment acts as a node in the linked list
representing the environments stack.

The elements in the map are called "bindings".
*/
type Environment<V> = { values: Map<string, V>; parent?: Environment<V> };

/*
An environment stack is a linked list of Environment nodes. 

The type of the values stored in the environments is represented by the 
generic type V.
*/
class EnvironmentStack<V> {
    private currentEnv: Environment<V>;

    constructor() {
        this.currentEnv = { values: new Map() };
    }

    private findBindingMap(name: string): Map<string, V> | undefined {
        let env: Environment<V> | undefined = this.currentEnv;
        while (env !== undefined) {
            if (env.values.has(name)) {
                return env.values;
            } else {
                env = env.parent;
            }
        }
        return undefined;
    }

    /*
    Sets a binding for "name" in the **current** environment of the stack.
    If a binding for "name" already exists in the current environment, it 
    overwrites the binding with the provided value.
    As a special case, name "_" is ignored.

    Note that this method does not check if binding "name" already exists in 
    a parent environment.
    This means that if binding "name" already exists in a parent environment, 
    it will be shadowed by the provided value in the current environment.
    This shadowing behavior is useful for modelling recursive function calls.
    For example, consider the recursive implementation of factorial 
    (for simplification purposes, it returns 1 for the factorial of 
    negative numbers):

    1  fun factorial(a: Int): Int {
    2  if (a <= 1) {
    3     return 1;
    4  } else {
    5     return a * factorial(a - 1);
    6  }

    Just before factorial(4) finishes its execution, the environment stack will
    look as follows (the arrows point to their parent environment):

    a = 4 <------- a = 3 <-------- a = 2 <------- a = 1

    Note how each child environment shadows variable a, because each
    recursive call to factorial at line 5 creates a child
    environment with a new binding for a.

    When factorial(1) = 1 finishes execution, the environment at the top
    of the stack is popped:
    
    a = 4 <------- a = 3 <-------- a = 2

    and execution resumes at line 5 in the environment where a = 2,
    so that the return at line 5 is 2 * 1 = 2.

    This in turn causes the stack to pop the environment at the top:

    a = 4 <------- a = 3

    so that the return at line 5 (now in the environment a = 3) will 
    produce 3 * 2 = 6, and so on.
    */
    public setNewBinding(name: string, val: V) {
        if (name !== WILDCARD_NAME) {
            this.currentEnv.values.set(name, val);
        }
    }

    /*
    Searches the binding "name" in the stack, starting at the current
    environment and moving towards the parent environments. 
    If it finds the binding, it updates its value
    to "val". If it does not find "name", the stack is unchanged.
    As a special case, name "_" is always ignored.
    */
    public updateBinding(name: string, val: V) {
        if (name !== WILDCARD_NAME) {
            const bindings = this.findBindingMap(name);
            if (bindings !== undefined) {
                bindings.set(name, val);
            }
        }
    }

    /*
    Searches the binding "name" in the stack, starting at the current
    environment and moving towards the parent environments. 
    If it finds "name", it returns its value.
    If it does not find "name", it returns undefined.
    As a special case, name "_" always returns undefined.
    */
    public getBinding(name: string): V | undefined {
        if (name === WILDCARD_NAME) {
            return undefined;
        }
        const bindings = this.findBindingMap(name);
        if (bindings !== undefined) {
            return bindings.get(name);
        } else {
            return undefined;
        }
    }

    public selfInEnvironment(): boolean {
        return this.findBindingMap("self") !== undefined;
    }

    /*
    Executes "code" in a fresh environment that is placed at the top
    of the environment stack. The fresh environment is initialized
    with the bindings in "initialBindings". Once "code" finishes
    execution, the new environment is automatically popped from 
    the stack. 
    
    This method is useful for starting a new local variables scope, 
    like in a function call.
    */
    public executeInNewEnvironment<T>(
        code: () => T,
        initialBindings: { names: string[]; values: V[] } = {
            names: [],
            values: [],
        },
    ): T {
        const names = initialBindings.names;
        const values = initialBindings.values;

        const oldEnv = this.currentEnv;
        this.currentEnv = { values: new Map(), parent: oldEnv };

        names.forEach((name, index) => {
            this.setNewBinding(name, values[index]!);
        }, this);

        try {
            return code();
        } finally {
            this.currentEnv = oldEnv;
        }
    }
}

const defaultInterpreterConfig: InterpreterConfig = {
    // We set the default max number of loop iterations
    // to the maximum number allowed for repeat loops
    maxLoopIterations: maxRepeatStatement,
};

/*
The standard semantics for the Tact interpreter. See Interpreter class
for ways of instantiating interpreters with different semantics.

The constructor receives an optional CompilerContext which includes 
all external declarations that the interpreter will use during interpretation.
If no CompilerContext is provided, the semantics will use an empty 
CompilerContext.

**IMPORTANT**: if a custom CompilerContext is provided, it should be the 
CompilerContext provided by the typechecker. 

The reason for requiring a CompilerContext is that the interpreter should work 
in the use case where the interpreter only knows part of the code.
For example, consider the following code (I marked with brackets [ ] the places 
where the interpreter gets called during expression simplification in the 
compilation phase):

const C: Int = [1];

contract TestContract {

   get fun test(): Int {
      return [C + 1];
   }
}

When the interpreter gets called inside the brackets, it does not know what 
other code is surrounding those brackets, because the interpreter did not execute the 
code outside the brackets. Hence, it relies on the typechecker to receive the 
CompilerContext that includes the declarations in the code 
(the constant C for example).

Since the interpreter relies on the typechecker, this semantics assume that the 
interpreter will only be called on AST trees
that are already valid Tact programs.

Internally, the semantics use a stack of environments to keep track of
variables at different scopes. Each environment in the stack contains a map
that binds a variable name to its corresponding value.

In the standard semantics, statements do not produce results, as such I use type "undefined"
as the generic type for statement's results.
*/
export class StandardSemantics extends InterpreterSemantics<Value, undefined> {
    private envStack: EnvironmentStack<Value>;
    private context: CompilerContext;
    private config: InterpreterConfig;

    constructor(
        context: CompilerContext = new CompilerContext(),
        config: InterpreterConfig = defaultInterpreterConfig,
    ) {
        super();
        this.envStack = new EnvironmentStack();
        this.context = context;
        this.config = config;
    }

    public lookupBinding(name: AstId): Value {
        if (hasStaticConstant(this.context, idText(name))) {
            const constant = getStaticConstant(this.context, idText(name));
            if (constant.value !== undefined) {
                return constant.value;
            } else {
                throwErrorConstEval(
                    `cannot evaluate declared constant ${idText(name)} as it does not have a body`,
                    name.loc,
                );
            }
        }
        const variableBinding = this.envStack.getBinding(idText(name));
        if (variableBinding !== undefined) {
            return variableBinding;
        }
        throwNonFatalErrorConstEval("cannot evaluate a variable", name.loc);
    }

    public evalBuiltinOnSelf(
        ast: AstMethodCall,
        self: Value,
        _argValues: Value[],
    ): Value | undefined {
        switch (idText(ast.method)) {
            case "asComment": {
                ensureMethodArity(0, ast.args, ast.loc);
                const comment = ensureString(self, ast.self.loc);
                return new CommentValue(comment);
            }
            default:
                return undefined;
        }
    }

    public evalCallOnSelf(
        ast: AstMethodCall,
        _self: Value,
        _argValues: Value[],
    ): Value {
        throwNonFatalErrorConstEval(
            `calls of ${idTextErr(ast.method)} are not supported at this moment`,
            ast.loc,
        );
    }

    public evalNull(_ast: AstNull): Value {
        return null;
    }

    public evalBoolean(ast: AstBoolean): Value {
        return ast.value;
    }

    public evalInteger(ast: AstNumber): Value {
        return ensureInt(ast.value, ast.loc);
    }

    public evalString(ast: AstString): Value {
        return ensureString(
            interpretEscapeSequences(ast.value, ast.loc),
            ast.loc,
        );
    }

    public evalUnaryOp(ast: AstOpUnary, operandEvaluator: () => Value): Value {
        // Tact grammar does not have negative integer literals,
        // so in order to avoid errors for `-115792089237316195423570985008687907853269984665640564039457584007913129639936`
        // which is `-(2**256)` we need to have a special case for it

        if (ast.operand.kind === "number" && ast.op === "-") {
            // emulating negative integer literals
            return ensureInt(-ast.operand.value, ast.loc);
        }

        return evalUnaryOp(
            ast.op,
            operandEvaluator(),
            ast.operand.loc,
            ast.loc,
        );
    }

    public evalBinaryOp(
        ast: AstOpBinary,
        leftValue: Value,
        rightEvaluator: () => Value,
    ): Value {
        return evalBinaryOp(
            ast.op,
            leftValue,
            rightEvaluator(), // There is actually a bug in the current implementation of the semantics:
            // boolean operators do not short-circuit in this implementation
            // The evalBinaryOp function should receive the rightEvaluator, instead of executing
            // rightEvaluator and pass the result as a parameter.
            // This implies we need to change the the signature of the evalBinaryOp function
            // (the one outside the class).
            ast.left.loc,
            ast.right.loc,
            ast.loc,
        );
    }

    public evalBinaryOpInAugmentedAssign(
        ast: AstStatementAugmentedAssign,
        leftValue: Value,
        rightEvaluator: () => Value,
    ): Value {
        return evalBinaryOp(
            ast.op,
            leftValue,
            rightEvaluator(), // There is actually a bug in the current implementation of the semantics:
            // boolean operators do not short-circuit in this implementation
            // The evalBinaryOp function should receive the rightEvaluator continuation, instead of executing
            // rightEvaluator and pass the result as a parameter.
            // This implies we need to change the the signature of the evalBinaryOp function
            // (the one outside the class).
            ast.path.loc,
            ast.expression.loc,
            ast.loc,
        );
    }

    public toBoolean(value: Value, src: SrcInfo): boolean {
        return ensureBoolean(value, src);
    }

    public evalStructInstance(
        ast: AstStructInstance,
        initializerEvaluators: (() => Value)[],
    ): Value {
        if (ast.args.length !== initializerEvaluators.length) {
            throwInternalCompilerError(
                "Number of arguments in ast must match the number of argument evaluators.",
            );
        }

        const structTy = getType(this.context, ast.type);

        // initialize the resulting struct value with
        // the default values for fields with initializers
        // or null for uninitialized optional fields
        const resultWithDefaultFields: StructValue = structTy.fields.reduce(
            (resObj, field) => {
                if (field.default !== undefined) {
                    resObj[field.name] = field.default;
                } else {
                    if (field.type.kind === "ref" && field.type.optional) {
                        resObj[field.name] = null;
                    }
                }
                return resObj;
            },
            { $tactStruct: idText(ast.type) } as StructValue,
        );

        // this will override default fields set above
        return ast.args.reduce((resObj, fieldWithInit, index) => {
            resObj[idText(fieldWithInit.field)] =
                initializerEvaluators[index]!();
            return resObj;
        }, resultWithDefaultFields);
    }

    public evalFieldAccess(
        ast: AstFieldAccess,
        aggregateEvaluator: () => Value,
    ): Value {
        // special case for contract/trait constant accesses via `self.constant`
        // interpret "self" as a contract/trait access only if "self"
        // is not already assigned in the environment (this would mean
        // we are executing inside an extends function)
        if (
            ast.aggregate.kind === "id" &&
            isSelfId(ast.aggregate) &&
            !this.envStack.selfInEnvironment()
        ) {
            const selfTypeRef = getExpType(this.context, ast.aggregate);
            if (selfTypeRef.kind === "ref") {
                const contractTypeDescription = getType(
                    this.context,
                    selfTypeRef.name,
                );
                const foundContractConst =
                    contractTypeDescription.constants.find((constId) =>
                        eqNames(ast.field, constId.name),
                    );
                if (foundContractConst === undefined) {
                    // not a constant, e.g. `self.storageVariable`
                    throwNonFatalErrorConstEval(
                        "cannot evaluate non-constant self field access",
                        ast.aggregate.loc,
                    );
                }
                if (foundContractConst.value !== undefined) {
                    return foundContractConst.value;
                } else {
                    throwErrorConstEval(
                        `cannot evaluate declared contract/trait constant ${idTextErr(ast.field)} as it does not have a body`,
                        ast.field.loc,
                    );
                }
            }
        }
        const valStruct = aggregateEvaluator();
        if (
            valStruct === null ||
            typeof valStruct !== "object" ||
            !("$tactStruct" in valStruct)
        ) {
            throwErrorConstEval(
                `constant struct expected, but got ${showValue(valStruct)}`,
                ast.aggregate.loc,
            );
        }
        if (idText(ast.field) in valStruct) {
            return valStruct[idText(ast.field)]!;
        } else {
            // this cannot happen in a well-typed program
            throwInternalCompilerError(
                `struct field ${idTextErr(ast.field)} is missing`,
                ast.aggregate.loc,
            );
        }
    }

    public evalBuiltin(
        ast: AstStaticCall,
        argValues: Value[],
    ): Value | undefined {
        switch (idText(ast.function)) {
            case "ton": {
                ensureFunArity(1, ast.args, ast.loc);
                const tons = ensureString(argValues[0]!, ast.args[0]!.loc);
                try {
                    return ensureInt(
                        BigInt(toNano(tons).toString(10)),
                        ast.loc,
                    );
                } catch (e) {
                    if (e instanceof Error && e.message === "Invalid number") {
                        throwErrorConstEval(
                            `invalid ${idTextErr(ast.function)} argument`,
                            ast.loc,
                        );
                    }
                    throw e;
                }
            }
            case "pow": {
                ensureFunArity(2, ast.args, ast.loc);
                const valBase = ensureInt(argValues[0]!, ast.args[0]!.loc);
                const valExp = ensureInt(argValues[1]!, ast.args[1]!.loc);
                if (valExp < 0n) {
                    throwErrorConstEval(
                        `${idTextErr(ast.function)} builtin called with negative exponent ${valExp}`,
                        ast.loc,
                    );
                }
                try {
                    return ensureInt(valBase ** valExp, ast.loc);
                } catch (e) {
                    if (e instanceof RangeError) {
                        // even TS bigint type cannot hold it
                        throwErrorConstEval(
                            "integer does not fit into TVM Int type",
                            ast.loc,
                        );
                    }
                    throw e;
                }
            }
            case "pow2": {
                ensureFunArity(1, ast.args, ast.loc);
                const valExponent = ensureInt(argValues[0]!, ast.args[0]!.loc);
                if (valExponent < 0n) {
                    throwErrorConstEval(
                        `${idTextErr(ast.function)} builtin called with negative exponent ${valExponent}`,
                        ast.loc,
                    );
                }
                try {
                    return ensureInt(2n ** valExponent, ast.loc);
                } catch (e) {
                    if (e instanceof RangeError) {
                        // even TS bigint type cannot hold it
                        throwErrorConstEval(
                            "integer does not fit into TVM Int type",
                            ast.loc,
                        );
                    }
                    throw e;
                }
            }
            case "sha256": {
                ensureFunArity(1, ast.args, ast.loc);
                const str = ensureString(argValues[0]!, ast.args[0]!.loc);
                const dataSize = Buffer.from(str).length;
                if (dataSize > 128) {
                    throwErrorConstEval(
                        `data is too large for sha256 hash, expected up to 128 bytes, got ${dataSize}`,
                        ast.loc,
                    );
                }
                return BigInt("0x" + sha256_sync(str).toString("hex"));
            }
            case "emptyMap": {
                ensureFunArity(0, ast.args, ast.loc);
                return null;
            }
            case "cell":
                {
                    ensureFunArity(1, ast.args, ast.loc);
                    const str = ensureString(argValues[0]!, ast.args[0]!.loc);
                    try {
                        return Cell.fromBase64(str);
                    } catch (_) {
                        throwErrorConstEval(
                            `invalid base64 encoding for a cell: ${str}`,
                            ast.loc,
                        );
                    }
                }
                break;
            case "address":
                {
                    ensureFunArity(1, ast.args, ast.loc);
                    const str = ensureString(argValues[0]!, ast.args[0]!.loc);
                    try {
                        const address = Address.parse(str);
                        if (
                            address.workChain !== 0 &&
                            address.workChain !== -1
                        ) {
                            throwErrorConstEval(
                                `${str} is invalid address`,
                                ast.loc,
                            );
                        }
                        if (
                            !enabledMasterchain(this.context) &&
                            address.workChain !== 0
                        ) {
                            throwErrorConstEval(
                                `address ${str} is from masterchain which is not enabled for this contract`,
                                ast.loc,
                            );
                        }
                        return address;
                    } catch (_) {
                        throwErrorConstEval(
                            `invalid address encoding: ${str}`,
                            ast.loc,
                        );
                    }
                }
                break;
            case "newAddress": {
                ensureFunArity(2, ast.args, ast.loc);
                const wc = ensureInt(argValues[0]!, ast.args[0]!.loc);
                const addr = Buffer.from(
                    ensureInt(argValues[1]!, ast.args[1]!.loc)
                        .toString(16)
                        .padStart(64, "0"),
                    "hex",
                );
                if (wc !== 0n && wc !== -1n) {
                    throwErrorConstEval(
                        `expected workchain of an address to be equal 0 or -1, received: ${wc}`,
                        ast.loc,
                    );
                }
                if (!enabledMasterchain(this.context) && wc !== 0n) {
                    throwErrorConstEval(
                        `${wc}:${addr.toString("hex")} address is from masterchain which is not enabled for this contract`,
                        ast.loc,
                    );
                }
                return new Address(Number(wc), addr);
            }
            default:
                return undefined;
        }
    }

    public lookupFunction(ast: AstStaticCall): AstFunctionDef {
        if (hasStaticFunction(this.context, idText(ast.function))) {
            const functionDescription = getStaticFunction(
                this.context,
                idText(ast.function),
            );
            switch (functionDescription.ast.kind) {
                case "function_def":
                    // Currently, no attribute is supported
                    if (functionDescription.ast.attributes.length > 0) {
                        throwNonFatalErrorConstEval(
                            "calls to functions with attributes are currently not supported",
                            ast.loc,
                        );
                    }
                    return functionDescription.ast;

                case "function_decl":
                    throwNonFatalErrorConstEval(
                        `${idTextErr(ast.function)} cannot be interpreted because it does not have a body`,
                        ast.loc,
                    );
                    break;
                case "native_function_decl":
                    throwNonFatalErrorConstEval(
                        "native function calls are currently not supported",
                        ast.loc,
                    );
                    break;
            }
        } else {
            throwNonFatalErrorConstEval(
                `function ${idTextErr(ast.function)} is not declared`,
                ast.loc,
            );
        }
    }

    public evalStaticCall(
        _ast: AstStaticCall,
        _functionDef: AstFunctionDef,
        functionBodyEvaluator: () => undefined,
        args: { names: string[]; values: Value[] },
    ): Value {
        // Call function inside a new environment
        return this.envStack.executeInNewEnvironment(
            () => {
                // Interpret all the statements
                try {
                    functionBodyEvaluator();
                    // At this point, the function did not execute a return.
                    // Execution continues after the catch.
                } catch (e) {
                    if (e instanceof ReturnSignal) {
                        const val = e.getValue();
                        if (val !== undefined) {
                            return val;
                        }
                        // The function executed a return without a value.
                        // Execution continues after the catch.
                    } else {
                        throw e;
                    }
                }
                // If execution reaches this point, it means that
                // the function had no return statement or executed a return
                // without a value. In summary, the function does not return a value.
                // We rely on the typechecker so that the function is called as a statement.
                // Hence, we can return a dummy null, since the null will be discarded anyway.
                return null;
            },
            { names: args.names, values: args.values },
        );
    }

    public storeNewBinding(ast: AstStatementLet, exprValue: Value): undefined {
        if (hasStaticConstant(this.context, idText(ast.name))) {
            // Attempt of shadowing a constant in a let declaration
            throwInternalCompilerError(
                `declaration of ${idText(ast.name)} shadows a constant with the same name`,
                ast.loc,
            );
        }

        this.envStack.setNewBinding(idText(ast.name), exprValue);
    }

    public updateBinding(id: AstId, exprValue: Value): undefined {
        this.envStack.updateBinding(idText(id), exprValue);
    }

    public runInNewEnvironment(
        statementsEvaluator: () => undefined,
    ): undefined {
        this.envStack.executeInNewEnvironment(statementsEvaluator);
    }

    public joinStatementResults(
        _first: undefined,
        _second: undefined,
    ): undefined {
        // Do nothing: statements do not return values, so there is nothing to join.
    }

    public emptyStatementResult(): undefined {
        // Do nothing
    }

    public toStatementResult(_val: Value): undefined {
        // Do nothing: values returned from expressions are ignored when the expression is executed as statement.
    }

    public toInteger(value: Value, src: SrcInfo): bigint {
        return ensureInt(value, src);
    }

    public evalReturn(val?: Value): undefined {
        throw new ReturnSignal(val);
    }

    public runOneIteration(
        iterationNumber: bigint,
        src: SrcInfo,
        iterationEvaluator: () => undefined,
    ): undefined {
        iterationEvaluator();
        if (iterationNumber >= this.config.maxLoopIterations) {
            throwNonFatalErrorConstEval("loop timeout reached", src);
        }
    }

    public toRepeatInteger(value: Value, src: SrcInfo): bigint {
        return ensureRepeatInt(value, src);
    }
}

export function ensureInt(val: Value, source: SrcInfo): bigint {
    if (typeof val !== "bigint") {
        throwErrorConstEval(
            `integer expected, but got '${showValue(val)}'`,
            source,
        );
    }
    if (minTvmInt <= val && val <= maxTvmInt) {
        return val;
    } else {
        throwErrorConstEval(
            `integer '${showValue(val)}' does not fit into TVM Int type`,
            source,
        );
    }
}

function ensureRepeatInt(val: Value, source: SrcInfo): bigint {
    if (typeof val !== "bigint") {
        throwErrorConstEval(
            `integer expected, but got '${showValue(val)}'`,
            source,
        );
    }
    if (minRepeatStatement <= val && val <= maxRepeatStatement) {
        return val;
    } else {
        throwErrorConstEval(
            `repeat argument must be a number between -2^256 (inclusive) and 2^31 - 1 (inclusive)`,
            source,
        );
    }
}

function ensureBoolean(val: Value, source: SrcInfo): boolean {
    if (typeof val !== "boolean") {
        throwErrorConstEval(
            `boolean expected, but got '${showValue(val)}'`,
            source,
        );
    }
    return val;
}

function ensureString(val: Value, source: SrcInfo): string {
    if (typeof val !== "string") {
        throwErrorConstEval(
            `string expected, but got '${showValue(val)}'`,
            source,
        );
    }
    return val;
}

function ensureFunArity(arity: number, args: AstExpression[], source: SrcInfo) {
    if (args.length !== arity) {
        throwErrorConstEval(
            `function expects ${arity} argument(s), but got ${args.length}`,
            source,
        );
    }
}

function ensureMethodArity(
    arity: number,
    args: AstExpression[],
    source: SrcInfo,
) {
    if (args.length !== arity) {
        throwErrorConstEval(
            `method expects ${arity} argument(s), but got ${args.length}`,
            source,
        );
    }
}

export function evalUnaryOp(
    op: AstUnaryOperation,
    valOperand: Value,
    operandLoc: SrcInfo = dummySrcInfo,
    source: SrcInfo = dummySrcInfo,
): Value {
    switch (op) {
        case "+":
            return ensureInt(valOperand, operandLoc);
        case "-":
            return ensureInt(-ensureInt(valOperand, operandLoc), source);
        case "~":
            return ~ensureInt(valOperand, operandLoc);
        case "!":
            return !ensureBoolean(valOperand, operandLoc);
        case "!!":
            if (valOperand === null) {
                throwErrorConstEval(
                    "non-null value expected but got null",
                    operandLoc,
                );
            }
            return valOperand;
    }
}

export function evalBinaryOp(
    op: AstBinaryOperation,
    valLeft: Value,
    valRight: Value,
    locLeft: SrcInfo = dummySrcInfo,
    locRight: SrcInfo = dummySrcInfo,
    source: SrcInfo = dummySrcInfo,
): Value {
    switch (op) {
        case "+":
            return ensureInt(
                ensureInt(valLeft, locLeft) + ensureInt(valRight, locRight),
                source,
            );
        case "-":
            return ensureInt(
                ensureInt(valLeft, locLeft) - ensureInt(valRight, locRight),
                source,
            );
        case "*":
            return ensureInt(
                ensureInt(valLeft, locLeft) * ensureInt(valRight, locRight),
                source,
            );
        case "/": {
            // The semantics of integer division for TVM (and by extension in Tact)
            // is a non-conventional one: by default it rounds towards negative infinity,
            // meaning, for instance, -1 / 5 = -1 and not zero, as in many mainstream languages.
            // Still, the following holds: a / b * b + a % b == a, for all b != 0.
            const r = ensureInt(valRight, locRight);
            if (r === 0n)
                throwErrorConstEval(
                    "divisor expression must be non-zero",
                    locRight,
                );
            return ensureInt(divFloor(ensureInt(valLeft, locLeft), r), source);
        }
        case "%": {
            // Same as for division, see the comment above
            // Example: -1 % 5 = 4
            const r = ensureInt(valRight, locRight);
            if (r === 0n)
                throwErrorConstEval(
                    "divisor expression must be non-zero",
                    locRight,
                );
            return ensureInt(modFloor(ensureInt(valLeft, locLeft), r), source);
        }
        case "&":
            return ensureInt(valLeft, locLeft) & ensureInt(valRight, locRight);
        case "|":
            return ensureInt(valLeft, locLeft) | ensureInt(valRight, locRight);
        case "^":
            return ensureInt(valLeft, locLeft) ^ ensureInt(valRight, locRight);
        case "<<": {
            const valNum = ensureInt(valLeft, locLeft);
            const valBits = ensureInt(valRight, locRight);
            if (0n > valBits || valBits > 256n) {
                throwErrorConstEval(
                    `the number of bits shifted ('${valBits}') must be within [0..256] range`,
                    locRight,
                );
            }
            try {
                return ensureInt(valNum << valBits, source);
            } catch (e) {
                if (e instanceof RangeError)
                    // this actually should not happen
                    throwErrorConstEval(
                        `integer does not fit into TVM Int type`,
                        source,
                    );
                throw e;
            }
        }
        case ">>": {
            const valNum = ensureInt(valLeft, locLeft);
            const valBits = ensureInt(valRight, locRight);
            if (0n > valBits || valBits > 256n) {
                throwErrorConstEval(
                    `the number of bits shifted ('${valBits}') must be within [0..256] range`,
                    locRight,
                );
            }
            try {
                return ensureInt(valNum >> valBits, source);
            } catch (e) {
                if (e instanceof RangeError)
                    // this is actually should not happen
                    throwErrorConstEval(
                        `integer does not fit into TVM Int type`,
                        source,
                    );
                throw e;
            }
        }
        case ">":
            return ensureInt(valLeft, locLeft) > ensureInt(valRight, locRight);
        case "<":
            return ensureInt(valLeft, locLeft) < ensureInt(valRight, locRight);
        case ">=":
            return ensureInt(valLeft, locLeft) >= ensureInt(valRight, locRight);
        case "<=":
            return ensureInt(valLeft, locLeft) <= ensureInt(valRight, locRight);
        case "==":
            // the null comparisons account for optional types, e.g.
            // a const x: Int? = 42 can be compared to null
            if (
                typeof valLeft !== typeof valRight &&
                valLeft !== null &&
                valRight !== null
            ) {
                throwErrorConstEval(
                    "operands of `==` must have same type",
                    source,
                );
            }
            return valLeft === valRight;
        case "!=":
            if (typeof valLeft !== typeof valRight) {
                throwErrorConstEval(
                    "operands of `!=` must have same type",
                    source,
                );
            }
            return valLeft !== valRight;
        case "&&":
            return (
                ensureBoolean(valLeft, locLeft) &&
                ensureBoolean(valRight, locRight)
            );
        case "||":
            return (
                ensureBoolean(valLeft, locLeft) ||
                ensureBoolean(valRight, locRight)
            );
    }
}

function interpretEscapeSequences(stringLiteral: string, source: SrcInfo) {
    return stringLiteral.replace(
        /\\\\|\\"|\\n|\\r|\\t|\\v|\\b|\\f|\\u{([0-9A-Fa-f]{1,6})}|\\u([0-9A-Fa-f]{4})|\\x([0-9A-Fa-f]{2})/g,
        (match, unicodeCodePoint, unicodeEscape, hexEscape) => {
            switch (match) {
                case "\\\\":
                    return "\\";
                case '\\"':
                    return '"';
                case "\\n":
                    return "\n";
                case "\\r":
                    return "\r";
                case "\\t":
                    return "\t";
                case "\\v":
                    return "\v";
                case "\\b":
                    return "\b";
                case "\\f":
                    return "\f";
                default:
                    // Handle Unicode code point escape
                    if (unicodeCodePoint) {
                        const codePoint = parseInt(unicodeCodePoint, 16);
                        if (codePoint > 0x10ffff) {
                            throwErrorConstEval(
                                `unicode code point is outside of valid range 000000-10FFFF: ${stringLiteral}`,
                                source,
                            );
                        }
                        return String.fromCodePoint(codePoint);
                    }
                    // Handle Unicode escape
                    if (unicodeEscape) {
                        const codeUnit = parseInt(unicodeEscape, 16);
                        return String.fromCharCode(codeUnit);
                    }
                    // Handle hex escape
                    if (hexEscape) {
                        const hexValue = parseInt(hexEscape, 16);
                        return String.fromCharCode(hexValue);
                    }
                    return match;
            }
        },
    );
}
