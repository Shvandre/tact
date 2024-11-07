import * as A from "./grammar/ast";
import { groupBy, intercalate, isUndefined } from "./utils/array";
import { makeVisitor } from "./utils/tricks";

//
// Types
//

export const ppAstTypeId = (typeRef: A.AstTypeId): string => A.idText(typeRef);

export const ppAstTypeIdWithStorage = (type: A.AstTypeId, storageType: A.AstId | null): string => {
    const alias = storageType ? ` as ${ppAstId(storageType)}` : "";
    return `${ppAstTypeId(type)}${alias}`;
};

export const ppAstMapType = (typeRef: A.AstMapType): string => {
    const key = ppAstTypeIdWithStorage(typeRef.keyType, typeRef.keyStorageType);
    const value = ppAstTypeIdWithStorage(typeRef.valueType, typeRef.valueStorageType);
    return `map<${key}, ${value}>`;
}

export const ppAstBouncedMessageType = (typeRef: A.AstBouncedMessageType): string => {
    return `bounced<${ppAstTypeId(typeRef.messageType)}>`;
}

export const ppAstOptionalType = (typeRef: A.AstOptionalType): string => {
    return `${ppAstType(typeRef.typeArg)}?`;
}

export const ppAstType = makeVisitor<A.AstType>()({
    "type_id": ppAstTypeId,
    "map_type": ppAstMapType,
    "bounced_message_type": ppAstBouncedMessageType,
    "optional_type": ppAstOptionalType,
});

//
// Expressions
//

export const precedenceMap: Readonly<Record<A.AstBinaryOperation, number>> = {
    "||": 1,
    "&&": 2,
    "|": 3,
    "^": 4,
    "&": 5,
    "==": 6,
    "!=": 6,
    "<": 7,
    ">": 7,
    "<=": 7,
    ">=": 7,
    "+": 8,
    "-": 8,
    "*": 9,
    "/": 9,
    "%": 9,
    "<<": 11, // BUG?
    ">>": 11,
};

/**
 * Returns precedence used in unary/binary operations.
 * Lower number means higher precedence
 */
export const getPrecedence = makeVisitor<A.AstExpression>()({
    'op_binary': (expr) => precedenceMap[expr.op],
    "conditional": () => 0,
    "static_call": () => 0,
    "method_call": () => 0,
    "op_unary": () => 10,
    "id": () => 11,
    "field_access": () => 11,
    "number": () => 11,
    "boolean": () => 11,
    "struct_instance": () => 11,
    "null": () => 11,
    "init_of": () => 11,
    "string": () => 11,
});

export const ppAstStructFieldInit = (param: A.AstStructFieldInitializer): string => `${ppAstId(param.field)}: ${ppAstExpression(param.initializer)}`;

export const ppAstOpBinary = (expr: A.AstOpBinary) => (currentPrecedence: number) => `${ppAstExpression(expr.left, currentPrecedence)} ${expr.op} ${ppAstExpression(expr.right, currentPrecedence)}`;
export const ppAstOpUnary = (expr: A.AstOpUnary) => (currentPrecedence: number) => `${expr.op}${ppAstExpression(expr.operand, currentPrecedence)}`;
export const ppAstFieldAccess = (expr: A.AstFieldAccess) => (currentPrecedence: number) => `${ppAstExpression(expr.aggregate, currentPrecedence)}.${ppAstId(expr.field)}`;
export const ppAstMethodCall = (expr: A.AstMethodCall) => (currentPrecedence: number) => `${ppAstExpression(expr.self, currentPrecedence)}.${ppAstId(expr.method)}(${expr.args.map((arg) => ppAstExpression(arg, currentPrecedence)).join(", ")})`;
export const ppAstStaticCall = (expr: A.AstStaticCall) => (currentPrecedence: number) => `${ppAstId(expr.function)}(${expr.args.map((arg) => ppAstExpression(arg, currentPrecedence)).join(", ")})`;
export const ppAstInitOf = (expr: A.AstInitOf) => (currentPrecedence: number) => `initOf ${ppAstId(expr.contract)}(${expr.args.map((arg) => ppAstExpression(arg, currentPrecedence)).join(", ")})`;
export const ppAstConditional = (expr: A.AstConditional) => (currentPrecedence: number) => `${ppAstExpression(expr.condition, currentPrecedence)} ? ${ppAstExpression(expr.thenBranch, currentPrecedence)} : ${ppAstExpression(expr.elseBranch, currentPrecedence)}`;

export const ppAstStructInstance = (expr: A.AstStructInstance) => `${ppAstId(expr.type)}{${expr.args.map((x) => ppAstStructFieldInit(x)).join(", ")}}`;
export const ppAstNumber = (expr: A.AstNumber) => A.astNumToString(expr);
export const ppAstBoolean = (expr: A.AstBoolean) => expr.value.toString();
export const ppAstString = (expr: A.AstString) => `"${expr.value}"`;
export const ppAstNull = (_expr: A.AstNull) => "null";
export const ppAstId = (expr: A.AstId) => expr.text;

export const ppLeaf = <T>(printer: (t: T) => string) => (node: T) => (): string => printer(node);

export const ppAstExpressionVisitor = makeVisitor<A.AstExpression>()({
    "op_binary": ppAstOpBinary,
    "op_unary": ppAstOpUnary,
    "field_access": ppAstFieldAccess,
    "method_call": ppAstMethodCall,
    "static_call": ppAstStaticCall,
    "init_of": ppAstInitOf,
    "conditional": ppAstConditional,
    "struct_instance": ppLeaf(ppAstStructInstance),
    "number": ppLeaf(ppAstNumber),
    "boolean": ppLeaf(ppAstBoolean),
    "string": ppLeaf(ppAstString),
    "null": ppLeaf(ppAstNull),
    "id": ppLeaf(ppAstId),
});

export const ppAstExpression = (expr: A.AstExpression, parentPrecedence: number = 0): string => {
    const currentPrecedence = getPrecedence(expr);

    const result = ppAstExpressionVisitor(expr)(currentPrecedence);

    const needParens = parentPrecedence > 0 &&
        currentPrecedence > 0 &&
        currentPrecedence < parentPrecedence;

    return needParens ? `(${result})` : result;
}

type Ctx<U> = {
    /**
     * Empty line of code
     */
    empty: U;

    /**
     * Line of code with \n implied
     */
    row: (s: string) => U;
    
    /**
     * Stacks lines after each other
     */
    block: (rows: readonly U[]) => U;

    /**
     * Similar to `block`, but adjacent lines of groups get concatenated
     * [a, b] + [c, d] = [a, bc, d]
     */
    concat: (rows: readonly U[]) => U;

    /**
     * Same as `indent`, but indents `rows` 1 level deeper and adds `{` and `}`
     */
    braced: (rows: readonly U[]) => U;
}

type LevelFn = (level: number) => string;

const createContext = (spaces: number): Ctx<readonly LevelFn[]> => {
    const empty = Object.freeze(new Array<LevelFn>());
    const row = (s: string) => [(level: number) => " ".repeat(level * spaces) + s];
    const concat = (rows: readonly (readonly LevelFn[])[]): readonly LevelFn[] => {
        const [head, ...tail] = rows;
        if (isUndefined(head)) {
            return [];
        }
        const next = concat(tail);
        const init = [...head];
        const last = init.pop();
        if (isUndefined(last)) {
            return next;
        }
        const [nextHead, ...nextTail] = next;
        if (isUndefined(nextHead)) {
            return head;
        }
        return [...init, (level) => last(level) + nextHead(level), ...nextTail];
    };
    const block = (rows: readonly (readonly LevelFn[])[]) => rows.flat();
    const indent = (rows: readonly (readonly LevelFn[])[]) => block(rows).map((f) => (level: number) => f(level + 1));
    const braced = (rows: readonly (readonly LevelFn[])[]) => block([row(`{`), indent(rows), row(`}`)]);
    return { empty, row, concat, block, braced };
};

type Printer<T> = (item: T) => <U>(ctx: Ctx<U>) => U;

type Functional = A.AstFunctionDef | A.AstAsmFunctionDef | A.AstFunctionDecl;

export const ppAstModule: Printer<A.AstModule> = ({ imports, items }) => (ctx) => {
    const importsCode = imports.length > 0 ? [
        ...imports.map((entry) => ppAstImport(entry)(ctx)),
        ctx.empty,
    ] : [];
    const entriesCode = intercalate(
        groupBy(items, ({ kind }) => kind === "constant_def" ? 1 : NaN)
            .map((group) => group.map((node) => ppModuleItem(node)(ctx))),
        ctx.empty,
    );
    return ctx.block([...importsCode, ...entriesCode]);
}

export const ppAstStruct: Printer<A.AstStructDecl> = ({ name, fields }) => (ctx) => {
    // BUG with }
    return ctx.concat([
        ctx.row(`struct ${ppAstId(name)} `),
        ctx.braced(fields.map((field) => ppAstFieldDecl(field)(ctx))),
    ]);
}

export const ppAstContract: Printer<A.AstContract> = ({ name, traits, declarations, attributes }) => (ctx) => {
    const attrsRaw = attributes
        .map(({ name: { value } }) => `@interface("${value}")`)
        .join(" ");
    const attrsFormatted = attrsRaw ? `${attrsRaw} ` : "";
    const traitsFormatted = traits
        .map((trait) => trait.text)
        .join(", ");
    const header = traitsFormatted
        ? `contract ${ppAstId(name)} with ${traitsFormatted}`
        : `contract ${ppAstId(name)}`;
    return ctx.concat([
        ctx.row(`${attrsFormatted}${header} `),
        ctx.braced(intercalate(
            groupBy(declarations, ({ kind }) => kind === "constant_def" ? 1 : kind === "field_decl" ? 2 : NaN)
                .map((group) => group.map((node) => ppContractBody(node)(ctx))),
            ctx.empty,
        )),
    ]);
}

export const ppAstPrimitiveTypeDecl: Printer<A.AstPrimitiveTypeDecl> = ({ name }) => ({ row }) => row(`primitive ${ppAstId(name)};`);

export const ppAstFunctionDef: Printer<A.AstFunctionDef> = (node) => (ctx) => {
    return ctx.concat([
        ctx.row(ppAstFunctionSignature(node)),
        ppStatementBlock(node.statements)(ctx)
    ]);
}

export const ppAsmShuffle = ({ args, ret }: A.AstAsmShuffle): string => {
    if (args.length === 0 && ret.length === 0) {
        return "";
    }
    const argsCode = args.map(({ text }) => text).join(" ");
    if (ret.length === 0) {
        return `(${argsCode})`;
    }
    const retCode = ret.map(({ value }) => value.toString()).join(" ");
    return `(${argsCode} -> ${retCode})`;
}

export const ppAstAsmFunctionDef: Printer<A.AstAsmFunctionDef> = (node) => (ctx) => {
    return ctx.concat([
        ctx.row(`asm${ppAsmShuffle(node.shuffle)} ${ppAstFunctionSignature(node)} `),
        ppAsmInstructionsBlock(node.instructions)(ctx)
    ]);
}

export const ppAstNativeFunction: Printer<A.AstNativeFunctionDecl> = ({ name, nativeName, params, return: retTy, attributes }) => (ctx) => {
    const argsFormatted = params
        .map(({ name, type }) => `${ppAstId(name)}: ${ppAstType(type)}`)
        .join(", ");
    const returnType = retTy ? `: ${ppAstType(retTy)}` : "";
    const attrs = attributes.map(({ type }) => type + " ").join("");
    return ctx.block([
        ctx.row(`@name(${ppAstFuncId(nativeName)})`),
        ctx.row(`${attrs}native ${ppAstId(name)}(${argsFormatted})${returnType};`),
    ]);
}

export const ppAstTrait: Printer<A.AstTrait> = ({ name, traits, attributes, declarations }) => (ctx) => {
    const traitsFormatted = traits.map((t) => ppAstId(t)).join(", ");
    const header = traitsFormatted
        ? `trait ${ppAstId(name)} with ${traitsFormatted}`
        : `trait ${ppAstId(name)}`;
    const attrsRaw = attributes.map((attr) => `@${attr.type}("${attr.name.value}")`).join(" ");
    const attrsFormatted = attrsRaw ? `${attrsRaw} ` : "";
    return ctx.concat([
        ctx.row(`${attrsFormatted}${header} `),
        ctx.braced(intercalate(
            groupBy(declarations, ({ kind }) => kind === "constant_def" || kind === "constant_decl" ? 1 : kind === "field_decl" ? 2 : NaN)
                .map((group) => group.map((node) => ppTraitBody(node)(ctx))),
            ctx.empty,
        )),
    ]);
}

export const ppAstConstant: Printer<A.AstConstantDef> = ({ attributes, initializer, name, type }) => ({ row }) => {
    const attrsFormatted = attributes.map(({ type }) => type + " ").join("");
    return row(`${attrsFormatted}const ${ppAstId(name)}: ${ppAstType(type)} = ${ppAstExpression(initializer)};`);
}

export const ppAstMessage: Printer<A.AstMessageDecl> = ({ name, opcode, fields }) => (ctx) => {
    const prefixFormatted = opcode !== null ? `(${A.astNumToString(opcode)})` : "";
    // BUG with }
    return ctx.concat([
        ctx.row(`message${prefixFormatted} ${ppAstId(name)} `),
        ctx.braced(fields.map((field) => ppAstFieldDecl(field)(ctx))),
    ]);
}

export const ppModuleItem: Printer<A.AstModuleItem> = makeVisitor<A.AstModuleItem>()({
    "struct_decl": ppAstStruct,
    "contract": ppAstContract,
    "primitive_type_decl": ppAstPrimitiveTypeDecl,
    "function_def": ppAstFunctionDef,
    "asm_function_def": ppAstAsmFunctionDef,
    "native_function_decl": ppAstNativeFunction,
    "trait": ppAstTrait,
    "constant_def": ppAstConstant,
    "message_decl": ppAstMessage,
});

export const ppAstFieldDecl: Printer<A.AstFieldDecl> = ({ type, initializer, as, name }) => ({ row }) => {
    const fieldName = ppAstId(name);
    const typeFormatted = ppAstType(type);
    const asAlias = as ? ` as ${ppAstId(as)}` : "";
    const initializerCode = initializer
        ? ` = ${ppAstExpression(initializer)}`
        : "";
    return row(`${fieldName}: ${typeFormatted}${asAlias}${initializerCode};`);
};

export const ppAstReceiver: Printer<A.AstReceiver> = ({ selector, statements }) => (ctx) => {
    return ctx.concat([
        ctx.row(`${ppAstReceiverHeader(selector)} `),
        ppStatementBlock(statements)(ctx),
    ]);
}

export const ppAstFunctionDecl: Printer<A.AstFunctionDecl> = (f) => ({ row }) => {
    return row(`${ppAstFunctionSignature(f)};`);
}

export const ppAstConstDecl: Printer<A.AstConstantDecl> = ({ attributes, name, type }) => ({ row }) => {
    const attrsFormatted = attributes.map(({ type }) => type + " ").join("");
    return row(`${attrsFormatted}const ${ppAstId(name)}: ${ppAstType(type)};`);
}

export const ppTraitBody: Printer<A.AstTraitDeclaration> = makeVisitor<A.AstTraitDeclaration>()({
    "function_def": ppAstFunctionDef,
    "asm_function_def": ppAstAsmFunctionDef,
    "constant_def": ppAstConstant,
    "field_decl": ppAstFieldDecl,
    "receiver": ppAstReceiver,
    "function_decl": ppAstFunctionDecl,
    "constant_decl": ppAstConstDecl,
});

export const ppAstInitFunction: Printer<A.AstContractInit> = ({ params, statements }) => (ctx) => {
    const argsFormatted = params
        .map(({ name, type }) => `${ppAstId(name)}: ${ppAstType(type)}`)
        .join(", ");
    if (statements.length === 0) {
        return ctx.row(`init(${argsFormatted}) {}`);
    }
    return ctx.concat([
        ctx.row(`init(${argsFormatted}) `),
        ctx.braced(statements.map((stmt) => ppAstStatement(stmt)(ctx))),
    ]);
}

export const ppContractBody: Printer<A.AstContractDeclaration> = makeVisitor<A.AstContractDeclaration>()({
    "field_decl": ppAstFieldDecl,
    "function_def": ppAstFunctionDef,
    "asm_function_def": ppAstAsmFunctionDef,
    "contract_init": ppAstInitFunction,
    "receiver": ppAstReceiver,
    "constant_def": ppAstConstant,
});

export const ppAstImport: Printer<A.AstImport> = ({ path }) => ({ row }) => row(`import "${path.value}";`);

export const ppAstFunctionSignature = ({ name, attributes, return: retTy, params }: Functional): string => {
    const argsFormatted = params
        .map(({ name, type }) => `${ppAstId(name)}: ${ppAstType(type)}`)
        .join(", ");
    const attrsFormatted = attributes
        .map((attr) => ppAstFunctionAttribute(attr) + " ")
        .join("");
    const returnType = retTy ? `: ${ppAstType(retTy)}` : "";
    return `${attrsFormatted}fun ${ppAstId(name)}(${argsFormatted})${returnType}`;
};

export const ppAstFunctionAttribute = (attr: A.AstFunctionAttribute): string => {
    if (attr.type === "get" && attr.methodId !== null) {
        return `get(${ppAstExpression(attr.methodId)})`;
    } else {
        return attr.type;
    }
}

export const ppAstReceiverHeader = makeVisitor<A.AstReceiverKind>()({
    "bounce": ({ param: { name, type } }) => `bounced(${ppAstId(name)}: ${ppAstType(type)})`,
    "internal-simple": ({ param: { name, type } }) => `receive(${ppAstId(name)}: ${ppAstType(type)})`,
    "external-simple": ({ param: { name, type } }) => `external(${ppAstId(name)}: ${ppAstType(type)})`,
    "internal-fallback": () => `receive()`,
    "external-fallback": () => `external()`,
    "internal-comment": ({ comment: { value } }) => `receive("${value}")`,
    "external-comment": ({ comment: { value } }) => `external("${value}")`,
});

export const ppAstFuncId = (func: A.AstFuncId): string => func.text;

//
// Statements
//

export const ppStatementBlock: Printer<A.AstStatement[]> = (stmts) => (ctx) => {
    return ctx.braced(stmts.map((stmt) => ppAstStatement(stmt)(ctx)));
};

export const ppAsmInstructionsBlock: Printer<A.AstAsmInstruction[]> = (instructions) => (ctx) => {
    return ctx.braced(instructions.map((instruction) => ctx.row(instruction)));
}

export const ppAstStatementLet: Printer<A.AstStatementLet> = ({ type, name, expression }) => ({ row }) => {
    const tyAnnotation = type === null ? "" : `: ${ppAstType(type)}`;
    return row(`let ${ppAstId(name)}${tyAnnotation} = ${ppAstExpression(expression)};`);
}

export const ppAstStatementReturn: Printer<A.AstStatementReturn> = ({ expression }) => ({ row }) => {
    return row(`return ${expression ? ppAstExpression(expression) : ""};`);
}

export const ppAstStatementExpression: Printer<A.AstStatementExpression> = ({ expression }) => ({ row }) => {
    return row(`${ppAstExpression(expression)};`);
}

export const ppAstStatementAssign: Printer<A.AstStatementAssign> = ({ path, expression }) => ({ row }) => {
    return row(`${ppAstExpression(path)} = ${ppAstExpression(expression)};`);
}

export const ppAstStatementAugmentedAssign: Printer<A.AstStatementAugmentedAssign> = ({ path, op, expression }) => ({ row }) => {
    return row(`${ppAstExpression(path)} ${op}= ${ppAstExpression(expression)};`);
}

export const ppAstCondition: Printer<A.AstCondition> = ({ condition, trueStatements, falseStatements }) => (ctx) => {
    if (falseStatements) {
        return ctx.concat([
            ctx.row(`if (${ppAstExpression(condition)}) `),
            ppStatementBlock(trueStatements)(ctx),
            ctx.row(" else "),
            ppStatementBlock(falseStatements)(ctx),
        ]);
    } else {
        return ctx.concat([
            ctx.row(`if (${ppAstExpression(condition)}) `),
            ppStatementBlock(trueStatements)(ctx),
        ]);
    }
}

export const ppAstStatementWhile: Printer<A.AstStatementWhile> = ({ condition, statements }) => (ctx) => {
    return ctx.concat([
        ctx.row(`while (${ppAstExpression(condition)}) `),
        ppStatementBlock(statements)(ctx),
    ]);
}

export const ppAstStatementRepeat: Printer<A.AstStatementRepeat> = ({ iterations, statements }) => (ctx) => {
    return ctx.concat([
        ctx.row(`repeat (${ppAstExpression(iterations)}) `),
        ppStatementBlock(statements)(ctx),
    ]);
}

export const ppAstStatementUntil: Printer<A.AstStatementUntil> = ({ condition, statements }) => (ctx) => {
    return ctx.concat([
        ctx.row(`do `),
        ppStatementBlock(statements)(ctx),
        ctx.row(` until (${ppAstExpression(condition)});`),
    ]);
}

export const ppAstStatementForEach: Printer<A.AstStatementForEach> = ({ keyName, valueName, map, statements }) => (ctx) => {
    return ctx.concat([
        ctx.row(`foreach (${ppAstId(keyName)}, ${ppAstId(valueName)} in ${ppAstExpression(map)}) `),
        ppStatementBlock(statements)(ctx),
    ]);
}

export const ppAstStatementTry: Printer<A.AstStatementTry> = ({ statements }) => (ctx) => {
    return ctx.concat([
        ctx.row(`try `),
        ppStatementBlock(statements)(ctx),
    ]);
}

export const ppAstStatementTryCatch: Printer<A.AstStatementTryCatch> = ({ statements, catchName, catchStatements }) => (ctx) => {
    return ctx.concat([
        ctx.row(`try `),
        ppStatementBlock(statements)(ctx),
        ctx.row(` catch (${ppAstId(catchName)}) `),
        ppStatementBlock(catchStatements)(ctx),
    ]);
}

export const ppAstStatementDestruct: Printer<A.AstStatementDestruct> = ({ type, identifiers, expression }) => ({ row }) => {
    const ids: string[] = [];
    for (const [field, name] of identifiers.values()) {
        const id =
            field.text === name.text
                ? ppAstId(name)
                : `${ppAstId(field)}: ${ppAstId(name)}`;
        ids.push(id);
    }
    return row(`let ${ppAstTypeId(type)} {${ids.join(", ")}} = ${ppAstExpression(expression)};`);
}

export const ppAstStatement: Printer<A.AstStatement> = makeVisitor<A.AstStatement>()({
    "statement_let": ppAstStatementLet,
    "statement_return": ppAstStatementReturn,
    "statement_expression": ppAstStatementExpression,
    "statement_assign": ppAstStatementAssign,
    "statement_augmentedassign": ppAstStatementAugmentedAssign,
    "statement_condition": ppAstCondition,
    "statement_while": ppAstStatementWhile,
    "statement_until": ppAstStatementUntil,
    "statement_repeat": ppAstStatementRepeat,
    "statement_foreach": ppAstStatementForEach,
    "statement_try": ppAstStatementTry,
    "statement_try_catch": ppAstStatementTryCatch,
    "statement_destruct": ppAstStatementDestruct,
});

export const exprNode = <T>(exprPrinter: (expr: T) => string): Printer<T> => (node) => ({ row }) => row(exprPrinter(node));

export const ppAstNode: Printer<A.AstNode> = makeVisitor<A.AstNode>()({
    "op_binary": exprNode(ppAstExpression),
    "op_unary": exprNode(ppAstExpression),
    "field_access": exprNode(ppAstExpression),
    "method_call": exprNode(ppAstExpression),
    "static_call": exprNode(ppAstExpression),
    "struct_instance": exprNode(ppAstExpression),
    "init_of": exprNode(ppAstExpression),
    "conditional": exprNode(ppAstExpression),
    "number": exprNode(ppAstExpression),
    "id": exprNode(ppAstExpression),
    "boolean": exprNode(ppAstExpression),
    "string": exprNode(ppAstExpression),
    "null": exprNode(ppAstExpression),
    "type_id": exprNode(ppAstType),
    "optional_type": exprNode(ppAstType),
    "map_type": exprNode(ppAstType),
    "bounced_message_type": exprNode(ppAstType),
    "struct_field_initializer": exprNode(ppAstStructFieldInit),
    "destruct_mapping": () => { throw new Error('BUG'); },
    "typed_parameter": () => { throw new Error('BUG'); },

    "module": ppAstModule,
    "struct_decl": ppAstStruct,
    "constant_def": ppAstConstant,
    "constant_decl": ppAstConstDecl,
    "function_def": ppAstFunctionDef,
    "contract": ppAstContract,
    "trait": ppAstTrait,
    "primitive_type_decl": ppAstPrimitiveTypeDecl,
    "message_decl": ppAstMessage,
    "native_function_decl": ppAstNativeFunction,
    "field_decl": ppAstFieldDecl,
    "function_decl": ppAstFunctionDecl,
    "receiver": ppAstReceiver,
    "contract_init": ppAstInitFunction,
    "statement_let": ppAstStatementLet,
    "statement_return": ppAstStatementReturn,
    "statement_expression": ppAstStatementExpression,
    "statement_assign": ppAstStatementAssign,
    "statement_augmentedassign": ppAstStatementAugmentedAssign,
    "statement_condition": ppAstCondition,
    "statement_while": ppAstStatementWhile,
    "statement_until": ppAstStatementUntil,
    "statement_repeat": ppAstStatementRepeat,
    "statement_try": ppAstStatementTry,
    "statement_try_catch": ppAstStatementTryCatch,
    "statement_foreach": ppAstStatementForEach,
    "import": ppAstImport,
    "func_id": exprNode(ppAstFuncId),
    "statement_destruct": ppAstStatementDestruct,
    "function_attribute": exprNode(ppAstFunctionAttribute),
    "asm_function_def": ppAstAsmFunctionDef,
});

/**
 * Pretty-prints an AST node into a string representation.
 * @param node The AST node to format.
 * @returns A string that represents the formatted AST node.
 */
export const prettyPrint = (node: A.AstNode): string => {
    // Default number of spaces per indentation level is 4
    return ppAstNode(node)(createContext(4))
        // Initial level of indentation is 0
        .map((f) => f(0))
        // Lines are terminated with \n
        .join('\n');
};
