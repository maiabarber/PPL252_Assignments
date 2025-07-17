import {
    Exp, Program, CExp, DefineExp, VarDecl,
    isProgram, isDefineExp, isCExp, isNumExp, isBoolExp,
    isStrExp, isPrimOp, isVarRef, isAppExp, isIfExp, isProcExp
} from './L3/L3-ast';

import { Result, makeOk, makeFailure } from './shared/result';

// מיפוי אופרטורים
const opMap: { [op: string]: string } = {
    "=": "===",
    "eq?": "===",
    "number?": "number",
    "boolean?": "boolean",
    "not": "!",
    "and": "&&",
    "or": "||"
};

// תרגום ביטוי CExp ל-JavaScript
const l2ToJSExp = (exp: CExp): string =>
    isNumExp(exp) ? `${exp.val}` :
    isBoolExp(exp) ? `${exp.val}` :
    isStrExp(exp) ? `"${exp.val}"` :
    isVarRef(exp) ? exp.var :
    isIfExp(exp) ? `(${l2ToJSExp(exp.test)} ? ${l2ToJSExp(exp.then)} : ${l2ToJSExp(exp.alt)})` :
    isProcExp(exp) ?
        `((${exp.args.map((v: VarDecl) => v.var).join(",")}) => ${l2ToJSExp(exp.body[0])})` :
    isAppExp(exp) ? (() => {
        const rator = exp.rator;
        const rands = exp.rands.map(l2ToJSExp);
        if (isPrimOp(rator)) {
            const op = rator.op;
            if (op === "not") return `(!${rands[0]})`;
            if (op === "number?" || op === "boolean?") {
                const type = opMap[op];
                return `((x) => typeof(x) === '${type}')(${rands[0]})`;
            }
            return `(${rands.join(` ${opMap[op] || op} `)})`;
        } else {
            return `${l2ToJSExp(rator)}(${rands.join(",")})`;
        }
    })() :
    isPrimOp(exp) ? (() => {
        if (exp.op === "number?" || exp.op === "boolean?") {
            const type = opMap[exp.op];
            return `((x) => typeof(x) === '${type}')`;
        }
        return exp.op;
    })() :
    "UNKNOWN";

// תרגום define — בלי ;
const l2ToJSDefine = (def: DefineExp): string =>
    `const ${def.var.var} = ${l2ToJSExp(def.val)}`;

/*
Purpose: Transform L2 AST to JavaScript program string
Signature: l2ToJS(l2AST)
Type: [EXP | Program] => Result<string>
*/
export const l2ToJS = (exp: Exp | Program): Result<string>  => {
    if (isProgram(exp)) {
        const jsLines = exp.exps.map((e, i) => {
            const line = isDefineExp(e) ? l2ToJSDefine(e) : l2ToJSExp(e);
            const isLast = i === exp.exps.length - 1;
            return isLast && !isDefineExp(e) ? line : `${line};`;
        });
        return makeOk(jsLines.join('\n'));
    } else if (isDefineExp(exp)) {
        return makeOk(l2ToJSDefine(exp));
    } else if (isCExp(exp)) {
        return makeOk(l2ToJSExp(exp));
    } else {
        return makeFailure("Invalid AST node");
    }
};
