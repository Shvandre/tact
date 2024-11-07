import * as A from "./grammar/ast";
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

type Ctx = {
    row: (s: string) => string;
    tab: <T>(cb: () => T) => T;
}

/**
 * @param level Initial level of indentation.
 * @param spaces Number of spaces per indentation level.
 */
const createContext = (level: number = 0, spaces: number = 4): Ctx => ({
    row: (s) => " ".repeat(level * spaces) + s,
    tab: (cb) => {
        try {
            ++level;
            return cb();
        } finally {
            --level;
        }
    },
});
        
type Printer<T> = (item: T) => (ctx: Ctx) => string;

type Functional = A.AstFunctionDef | A.AstAsmFunctionDef | A.AstFunctionDecl;

export const ppAstModule: Printer<A.AstModule> = ({ imports, items }) => (ctx) => {
    const importsFormatted =
        imports.length > 0
            ? `${imports
                    .map((entry) => ppAstImport(entry))
                    .join("\n")}\n\n`
            : "";
    const entriesFormatted = items
        .map((entry, index, array) => {
            const formattedEntry = ppModuleItem(entry)(ctx);
            const nextEntry = array[index + 1];
            if (
                entry.kind === "constant_def" &&
                // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
                nextEntry?.kind === "constant_def"
            ) {
                return formattedEntry;
            }
            return formattedEntry + "\n";
        })
        .join("\n");
    return `${importsFormatted}${entriesFormatted.trim()}`;
}

export const ppAstStruct: Printer<A.AstStructDecl> = ({ name, fields }) => ({ row, tab }) => {
    const start = row(`struct ${ppAstId(name)} {\n`);
    const fieldsFormatted = tab(() => {
        return fields.map((field) => ppAstFieldDecl(field)).join("\n");
    });
    const end = `}`; // row(`}`); // BUG?
    return `${start}${fieldsFormatted}\n${end}`;
}

export const ppAstContract: Printer<A.AstContract> = ({ name, traits, declarations, attributes }) => (ctx) => {
    const traitsFormatted = traits
        .map((trait) => trait.text)
        .join(", ");
    const attrsRaw = attributes
        .map((attr) => `@interface("${attr.name.value}")`)
        .join(" ");
    const attrsFormatted = attrsRaw ? `${attrsRaw} ` : "";
    const header = traitsFormatted
        ? `contract ${ppAstId(name)} with ${traitsFormatted}`
        : `contract ${ppAstId(name)}`;
    const start = ctx.row(`${attrsFormatted}${header} {\n`);
    const bodyFormatted = ctx.tab(() => {
        return declarations
            .map((dec, index, array) => {
                const formattedDec = ppContractBody(dec)(ctx);
                const nextDec = array[index + 1];
                if (
                    (dec.kind === "constant_def" &&
                        // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
                        nextDec?.kind === "constant_def") ||
                    (dec.kind === "field_decl" &&
                        // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
                        nextDec?.kind === "field_decl")
                ) {
                    return formattedDec;
                }
                return formattedDec + "\n";
            })
            .join("\n");
    })
    const end = ctx.row(`}`);
    return `${start}${bodyFormatted}${end}`;
}

export const ppAstPrimitiveTypeDecl: Printer<A.AstPrimitiveTypeDecl> = ({ name }) => ({ row }) => row(`primitive ${ppAstId(name)};`);

export const ppAstFunctionDef: Printer<A.AstFunctionDef> = (node) => (ctx) => {
    const signature = ctx.row(ppAstFunctionSignature(node));
    const body = ppStatementBlock(node.statements)(ctx);
    return `${signature} ${body}`;
}

export const ppAsmShuffle = (shuffle: A.AstAsmShuffle): string => {
    const ppArgShuffle = shuffle.args.map((id) => A.idText(id)).join(" ");
    const ppRetShuffle =
        shuffle.ret.length === 0
            ? ""
            : ` -> ${shuffle.ret.map((num) => num.value.toString()).join(" ")}`;
    return shuffle.args.length === 0 && shuffle.ret.length === 0
        ? ""
        : `(${ppArgShuffle}${ppRetShuffle})`;
}

export const ppAstAsmFunctionDef: Printer<A.AstAsmFunctionDef> = (node) => (ctx) => {
    const signature = ctx.row(`asm${ppAsmShuffle(node.shuffle)} ${ppAstFunctionSignature(node)}`);
    const body = ppAsmInstructionsBlock(node.instructions)(ctx);
    return `${signature} ${body}`;
}

export const ppAstNativeFunction: Printer<A.AstNativeFunctionDecl> = ({ name, nativeName, params, return: retTy, attributes }) => ({ row }) => {
    const argsFormatted = params
        .map(
            (arg) =>
                `${ppAstId(arg.name)}: ${ppAstType(arg.type)}`,
        )
        .join(", ");
    const returnType = retTy ? `: ${ppAstType(retTy)}` : "";
    const attrs = attributes.map((attr) => attr.type + " ").join("");
    const attribute = row(`@name(${ppAstFuncId(nativeName)})\n`);
    const definition = row(`${attrs}native ${ppAstId(name)}(${argsFormatted})${returnType};`);
    return `${attribute}${definition}`;
}

export const ppAstTrait: Printer<A.AstTrait> = ({ name, traits, attributes, declarations }) => (ctx) => {
    const traitsFormatted = traits.map((t) => ppAstId(t)).join(", ");
    const attrsRaw = attributes.map((attr) => `@${attr.type}("${attr.name.value}")`).join(" ");
    const attrsFormatted = ctx.row(attrsRaw ? `${attrsRaw} ` : "");
    const header = traitsFormatted
        ? `trait ${ppAstId(name)} with ${traitsFormatted}`
        : `trait ${ppAstId(name)}`;
    const start = `${attrsFormatted}${header} {\n`;
    const bodyFormatted = ctx.tab(() => {
        return declarations
            .map((dec, index, array) => {
                const formattedDec = ppTraitBody(dec)(ctx);
                const nextDec = array[index + 1];
                /* eslint-disable @typescript-eslint/no-unnecessary-condition */
                if (
                    ((dec.kind === "constant_def" ||
                        dec.kind === "constant_decl") &&
                        (nextDec?.kind === "constant_def" ||
                            nextDec?.kind === "constant_decl")) ||
                    (dec.kind === "field_decl" &&
                        nextDec?.kind === "field_decl")
                ) {
                    return formattedDec;
                }
                /* eslint-enable @typescript-eslint/no-unnecessary-condition */
                return formattedDec + "\n";
            })
            .join("\n");
    });
    const end = ctx.row(`}`);
    return `${start}${bodyFormatted}${end}`;
}

export const ppAstConstant: Printer<A.AstConstantDef> = ({ attributes, initializer, name, type }) => ({ row }) => {
    const attrsFormatted = attributes.map((attr) => attr.type + " ").join("");
    const valueFormatted = ` = ${ppAstExpression(initializer)}`;
    return row(`${attrsFormatted}const ${ppAstId(name)}: ${ppAstType(type)}${valueFormatted};`);
}

export const ppAstMessage: Printer<A.AstMessageDecl> = ({ name, opcode, fields }) => ({ row, tab }) => {
    const prefixFormatted = opcode !== null ? `(${A.astNumToString(opcode)})` : "";
    const start = row(`message${prefixFormatted} ${ppAstId(name)} {\n`);
    const fieldsFormatted = tab(() => {
        return fields.map((field) => ppAstFieldDecl(field)).join("\n");
    });
    const end = `}`; // row(`}`); // BUG?
    return `${start}${fieldsFormatted}\n${end}`;
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

export const ppAstFieldDecl: Printer<A.AstFieldDecl> = (field) => ({ row }) => {
    const typeFormatted = ppAstType(field.type);
    const initializer = field.initializer
        ? ` = ${ppAstExpression(field.initializer)}`
        : "";
    const asAlias = field.as ? ` as ${ppAstId(field.as)}` : "";
    return row(`${ppAstId(field.name)}: ${typeFormatted}${asAlias}${initializer};`);
}

export const ppAstReceiver: Printer<A.AstReceiver> = (receive) => (ctx) => {
    const header = ppAstReceiverHeader(receive.selector);
    const stmtsFormatted = ppStatementBlock(receive.statements)(ctx);
    return ctx.row(`${header} ${stmtsFormatted}`);
}

export const ppAstFunctionDecl: Printer<A.AstFunctionDecl> = (f) => ({ row }) => {
    return row(`${ppAstFunctionSignature(f)};`);
}

export const ppAstConstDecl: Printer<A.AstConstantDecl> = (constant) => ({ row }) => {
    const attrsRaw = constant.attributes.map((attr) => attr.type).join(" ");
    const attrsFormatted = attrsRaw ? `${attrsRaw} ` : "";
    return row(`${attrsFormatted}const ${ppAstId(constant.name)}: ${ppAstType(constant.type)};`);
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

export const ppAstInitFunction: Printer<A.AstContractInit> = ({ params, statements }) => ({ row, tab }) => {
    const argsFormatted = params
        .map(
            (arg) =>
                `${ppAstId(arg.name)}: ${ppAstType(arg.type)}`,
        )
        .join(", ");
    if (statements.length === 0) {
        return row(`init(${argsFormatted}) {}`);
    }
    const start = row(`init(${argsFormatted}) {\n`)
    const stmtsFormatted = tab(() => {
        return statements
            .map((stmt) => ppAstStatement(stmt))
            .join("\n");
    });
    const end = row('}');
    return `${start}${stmtsFormatted}\n${end}`;
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
        .map(
            (arg) =>
                `${ppAstId(arg.name)}: ${ppAstType(arg.type)}`,
        )
        .join(", ");
    const attrsRaw = attributes
        .map((attr) => ppAstFunctionAttribute(attr))
        .join(" ");
    const attrsFormatted = attrsRaw ? `${attrsRaw} ` : "";
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
    "internal-simple": (node) => `receive(${ppAstId(node.param.name)}: ${ppAstType(node.param.type)})`,
    "internal-fallback": () => `receive()`,
    "internal-comment": (node) => `receive("${node.comment.value}")`,
    "bounce": (node) => `bounced(${ppAstId(node.param.name)}: ${ppAstType(node.param.type)})`,
    "external-simple": (node) => `external(${ppAstId(node.param.name)}: ${ppAstType(node.param.type)})`,
    "external-fallback": () => `external()`,
    "external-comment": (node) => `external("${node.comment.value}")`,
});

export const ppAstFuncId = (func: A.AstFuncId): string => func.text;

//
// Statements
//

export const ppStatementBlock: Printer<A.AstStatement[]> = (stmts) => (ctx) => {
    const stmtsFormatted = ctx.tab(() => {
        return stmts
            .map((stmt) => ppAstStatement(stmt)(ctx))
            .join("\n");
    });
    const end = ctx.row('}');
    return `{\n${stmtsFormatted}\n${end}`;
}

export const ppAsmInstructionsBlock: Printer<A.AstAsmInstruction[]> = (instructions) => (ctx) => {
    const instructionsFormatted = ctx.tab(() => {
        return instructions
            .map((instruction) => ctx.row(instruction))
            .join("\n");
    });
    const end = ctx.row('}');
    return `{\n${instructionsFormatted}\n${end}`;
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
    const conditionCode = ctx.row(`if (${ppAstExpression(condition)}) `);
    const trueBranch = ppStatementBlock(trueStatements)(ctx);
    const falseBranch = falseStatements ? ` else ${ppStatementBlock(falseStatements)(ctx)}` : "";
    return `${conditionCode}${trueBranch}${falseBranch}`;
}

export const ppAstStatementWhile: Printer<A.AstStatementWhile> = ({ condition, statements }) => (ctx) => {
    const conditionCode = ppAstExpression(condition);
    const start = ctx.row(`while (${conditionCode})`);
    const stmts = ppStatementBlock(statements)(ctx);
    return `${start} ${stmts}`;
}

export const ppAstStatementRepeat: Printer<A.AstStatementRepeat> = ({ iterations, statements }) => (ctx) => {
    const condition = ppAstExpression(iterations);
    const start = ctx.row(`repeat (${condition})`);
    const stmts = ppStatementBlock(statements)(ctx);
    return `${start} ${stmts}`;
}

export const ppAstStatementUntil: Printer<A.AstStatementUntil> = ({ condition, statements }) => (ctx) => {
    const conditionCode = ppAstExpression(condition);
    const start = ctx.row(`do`);
    const stmts = ppStatementBlock(statements)(ctx);
    return `${start} ${stmts} until (${conditionCode});`;
}

export const ppAstStatementForEach: Printer<A.AstStatementForEach> = ({ keyName, valueName, map, statements }) => (ctx) => {
    const header = ctx.row(`foreach (${ppAstId(keyName)}, ${ppAstId(valueName)} in ${ppAstExpression(map)})`);
    const body = ppStatementBlock(statements)(ctx);
    return `${header} ${body}`;
}

export const ppAstStatementTry: Printer<A.AstStatementTry> = ({ statements }) => (ctx) => {
    const start = ctx.row(`try`);
    const body = ppStatementBlock(statements)(ctx);
    return `${start} ${body}`;
}

export const ppAstStatementTryCatch: Printer<A.AstStatementTryCatch> = ({ statements, catchName, catchStatements }) => (ctx) => {
    const start = ctx.row(`try`);
    const tryBody = ppStatementBlock(statements)(ctx);
    const catchBody = ppStatementBlock(catchStatements)(ctx);
    return `${start} ${tryBody} catch (${ppAstId(catchName)}) ${catchBody}`;
}

export const ppAstStatementDestruct: Printer<A.AstStatementDestruct> = ({ type, identifiers, expression }) => ({ row }) => {
    const ids: string[] = [];
    for (const[field, name] of identifiers.values()) {
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

export const exprNode = <T>(exprPrinter: (expr: T) => string): Printer<T> => (node) => () => exprPrinter(node);

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
export const prettyPrint = (node: A.AstNode): string => ppAstNode(node)(createContext());
