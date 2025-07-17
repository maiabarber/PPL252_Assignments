import {
    Program, Exp, CExp, isDictExp, makeAppExp, makeVarRef, makeLitExp,
    isNumExp, isBoolExp, isStrExp, isPrimOp, isVarRef, isLitExp,
    isIfExp, isProcExp, isAppExp, isLetExp,
    isDefineExp, makeDefineExp, makeProgram, makeVarDecl, makeProcExp, Binding
} from "./L32/L32-ast";

import {
    makeCompoundSExp, makeEmptySExp, makeSymbolSExp, SExpValue
} from "./L32/L32-value";

import { parseL3 } from "./L3/L3-ast";
import { isOk, Result } from "./shared/result";

// ממיר CExp לסוג SExpValue כדי להשתמש בתוך quote
const cexpToQuotedSExp = (e: CExp): SExpValue => {
    if (isNumExp(e) || isBoolExp(e) || isStrExp(e)) return e.val;
    if (isPrimOp(e)) return makeSymbolSExp(e.op);
    if (isVarRef(e)) return makeSymbolSExp(e.var);
    if (isLitExp(e)) return e.val;
    if (isAppExp(e))
        return makeCompoundSExp(
            cexpToQuotedSExp(e.rator),
            buildList(e.rands.map(cexpToQuotedSExp))
        );
    if (isProcExp(e)) {
        const args = e.args.map(arg => makeSymbolSExp(arg.var));
        const body = buildList(e.body.map(cexpToQuotedSExp));
        return makeCompoundSExp(makeSymbolSExp("lambda"),
                makeCompoundSExp(buildList(args), body));
    }
    if (isIfExp(e)) {
        return buildList([
            makeSymbolSExp("if"),
            cexpToQuotedSExp(e.test),
            cexpToQuotedSExp(e.then),
            cexpToQuotedSExp(e.alt)
        ]);
    }
    if (isLetExp(e)) {
        const bindings = buildList(e.bindings.map(b =>
            buildList([
                makeSymbolSExp(b.var.var),
                cexpToQuotedSExp(b.val)
            ])
        ));
        const body = e.body.map(cexpToQuotedSExp);
        return buildList([
            makeSymbolSExp("let"),
            bindings,
            ...body
        ]);
    }
    throw new Error(`Unsupported CExp for quote: ${JSON.stringify(e)}`);
};

// בונה רשימה סקספרשן רקורסיבית (SExp)
const buildList = (elts: SExpValue[]): SExpValue =>
    elts.length === 0
        ? makeEmptySExp()
        : makeCompoundSExp(elts[0], buildList(elts.slice(1)));

// ממיר זוגות מ־DictExp לרשימה של צמדי SExp (a . 1)
const dictPairsToSExpList = (pairs: [string, CExp][]): SExpValue =>
    pairs.reduceRight<SExpValue>(
        (acc, [k, v]) =>
            makeCompoundSExp(
                makeCompoundSExp(makeSymbolSExp(k), cexpToQuotedSExp(v)),
                acc
            ),
        makeEmptySExp()
    );


// מעבד כל ביטוי CExp וממיר DictExp ל־AppExp של (dict '(...))
const transformCExp = (e: CExp): CExp =>
    isNumExp(e) || isBoolExp(e) || isStrExp(e) || isPrimOp(e) || isVarRef(e) || isLitExp(e)
        ? e
        : isIfExp(e)
            ? { ...e, test: transformCExp(e.test), then: transformCExp(e.then), alt: transformCExp(e.alt) }
            : isProcExp(e)
                ? { ...e, body: e.body.map(transformCExp) }
                : isAppExp(e)
                    ? { ...e, rator: transformCExp(e.rator), rands: e.rands.map(transformCExp) }
                    : isLetExp(e)
                        ? {
                            ...e,
                            bindings: e.bindings.map((b: Binding) => ({ ...b, val: transformCExp(b.val) })),
                            body: e.body.map(transformCExp)
                        }
                        : isDictExp(e)
                            ? makeAppExp(makeVarRef("dict"), [
                                makeLitExp(dictPairsToSExpList(
                                    e.pairs.map(([k, v]) => [k, transformCExp(v)])
                                ))
                            ])
                            : e;

// מעבד Exp (DefineExp או CExp)
const transformExp = (exp: Exp): Exp =>
    isDefineExp(exp)
        ? makeDefineExp(exp.var, transformCExp(exp.val))
        : transformCExp(exp);

/*
Purpose: rewrite all occurrences of DictExp in a program to AppExp.
Signature: Dict2App (exp)
Type: Program -> Program
*/
export const Dict2App  = (exp: Program) : Program =>
    makeProgram(exp.exps.map(transformExp));

/*
  הגדרות קבועות: dict, assoc, bind וכו'
*/
const preludeCode = `(L3
    (define assoc
        (lambda (key pairs)
            (if (pair? pairs)
                (if (eq? key (car (car pairs)))
                    (car pairs)
                    (assoc key (cdr pairs)))
                #f)))

    (define error
        (lambda (msg)
            (list 'error msg)))

    (define is-error?
        (lambda (v)
            (if (pair? v)
                (eq? (car v) 'error)
                #f)))

    (define bind
        (lambda (v f)
            (if (is-error? v)
                v
                (f v))))

    (define dict
        (lambda (pairs)
            (lambda (key)
                ((lambda (found)
                    (if (eq? found #f)
                        (error "key not found")
                        (cdr found)))
                (assoc key pairs)))))
)`;

const parsedPrelude: Result<Program> = parseL3(preludeCode);

const dictDefs: Exp[] =
    isOk(parsedPrelude) ? parsedPrelude.value.exps : [];

/*
Purpose: Transform L32 program to L3
Signature: L32ToL3(prog)
Type: Program -> Program
*/
export const L32toL3 = (prog : Program): Program => {
    const transformed = Dict2App(prog);
    return makeProgram([
        ...dictDefs,
        ...transformed.exps
    ]);
};
