// ===========================================================
// AST type models for L5
// L5 extends L4 with:
// optional type annotations

import { join, map, zipWith } from "ramda";
import { Sexp, Token } from 's-expression';
import { isCompoundSExp, isEmptySExp, isSymbolSExp, makeCompoundSExp, makeEmptySExp, makeSymbolSExp, SExpValue, valueToString } from './L5-value';
import { isTVar, makeFreshTVar, parseTExp, unparseTExp, TExp } from './TExp';
import { allT, first, rest, second, isEmpty, isNonEmptyList } from '../shared/list';
import { parse as p, isToken, isSexpString } from "../shared/parser";
import { Result, bind, makeFailure, mapResult, makeOk, mapv } from "../shared/result";
import { isArray, isString, isNumericString, isIdentifier } from "../shared/type-predicates";
import { format } from "../shared/format";
import { typeOfSExpValue } from './L5-typeinference';


/*
// =============================================================================
// Examples of type annotated programs
// (define [x : number] 5)
// (define [f : [number -> number]] (lambda ([x : number]) : number (* x x))
// (define [f : [number * number -> number]] (lambda ([x : number] [y : number]) : number (* x x))
// (define f (lambda ([x : number]) (* x x)))  // no type annotation on f and on return value of lambda
// (let (([a : number] 1)
//       ([b : boolean] #t))
//   (if b a (+ a 1)))
// (define [id : (T1 -> T1)] (lambda ([x : T1]) : T1 x))
;;
// The only changes in the syntax of L5 are optional type annotations in var-decl and proc-exp
;;
// <program> ::= (L5 <exp>+)                  / Program(exps:List(exp))
// <exp> ::= <define> | <cexp>                / DefExp | CExp
// <define> ::= ( define <var-decl> <cexp> )  / DefExp(var:VarDecl, val:CExp)
// <var> ::= <identifier>                     / VarRef(var:string)
// <cexp> ::= <number>                        / NumExp(val:number)
//         |  <boolean>                       / BoolExp(val:boolean)
//         |  <string>                        / StrExp(val:string)
//         |  <var-ref>
//         |  ( lambda ( <var-decl>* ) <TExp>? <cexp>+ ) / ProcExp(args:VarDecl[], body:CExp[], returnTE: TExp))
//         |  ( if <cexp> <cexp> <cexp> )     / IfExp(test: CExp, then: CExp, alt: CExp)
//         |  ( quote <sexp> )                / LitExp(val:SExp)
//         |  ( <cexp> <cexp>* )              / AppExp(operator:CExp, operands:CExp[]))
//         |  ( let ( <binding>* ) <cexp>+ )  / LetExp(bindings:Binding[], body:CExp[]))
//         |  ( letrec ( binding*) <cexp>+ )  / LetrecExp(bindings:Bindings[], body: CExp)
//         |  ( set! <var> <cexp>)            / SetExp(var: varRef, val: CExp)
// <binding>  ::= ( <var> <cexp> )            / Binding(var:VarDecl, val:Cexp)
// <prim-op>  ::= + | - | * | / | < | > | = | not |  eq? | string=?
//                  | cons | car | cdr | list? | number?
//                  | boolean? | symbol? | string?
//                  | display | newline
// <num-exp>  ::= a number token
// <bool-exp> ::= #t | #f
// <var-ref>  ::= an identifier token         / VarRef(var)
// <var-decl> ::= an identifier token | (var : TExp) / VarRef(var, TE: TExp) ##### L5
// <sexp>     ::= symbol | number | bool | string | ( <sexp>* )              ##### L3
*/

// A value returned by parseL5
export type Parsed = Exp | Program;

export type Exp = DefineExp | CExp;
export const isExp = (x: any): x is Exp => isDefineExp(x) || isCExp(x);

export type CExp =  AtomicExp | CompoundExp;
export const isCExp = (x: any): x is CExp => isAtomicExp(x) || isCompoundExp(x);

export type AtomicExp = NumExp | BoolExp | StrExp | PrimOp | VarRef;
export const isAtomicExp = (x: any): x is AtomicExp =>
    isNumExp(x) || isBoolExp(x) || isStrExp(x) ||
    isPrimOp(x) || isVarRef(x);

export type CompoundExp = AppExp | IfExp | ProcExp | LetExp | LitExp | LetrecExp | SetExp;
export const isCompoundExp = (x: any): x is CompoundExp =>
    isAppExp(x) || isIfExp(x) || isProcExp(x) || isLitExp(x) || isLetExp(x) || isLetrecExp(x) || isSetExp(x);
export const expComponents = (e: Exp): CExp[] =>
    isIfExp(e) ? [e.test, e.then, e.alt] :
    isProcExp(e) ? e.body :
    isLetExp(e) ? [...e.body, ...map((b) => b.val, e.bindings)] :
    isLetrecExp(e) ? [...e.body, ...map((b) => b.val, e.bindings)] :
    isAppExp(e) ? [e.rator, ...e.rands] :
    isSetExp(e) ? [e.val] :
    isDefineExp(e) ? [e.val] :
    []; // Atomic expressions have no components

// Type definitions
export type Program = {tag: "Program"; exps: Exp[]; }
export const makeProgram = (exps: Exp[]): Program => ({tag: "Program", exps: exps});
export const isProgram = (x: any): x is Program => x.tag === "Program";

export type DefineExp = {tag: "DefineExp"; var: VarDecl; val: CExp; }
export const makeDefineExp = (v: VarDecl, val: CExp): DefineExp =>
    ({tag: "DefineExp", var: v, val: val});
export const isDefineExp = (x: any): x is DefineExp => x.tag === "DefineExp";

export type NumExp = {tag: "NumExp"; val: number; }
export const makeNumExp = (n: number): NumExp => ({tag: "NumExp", val: n});
export const isNumExp = (x: any): x is NumExp => x.tag === "NumExp";

export type BoolExp = {tag: "BoolExp"; val: boolean; }
export const makeBoolExp = (b: boolean): BoolExp => ({tag: "BoolExp", val: b});
export const isBoolExp = (x: any): x is BoolExp => x.tag === "BoolExp";

export type StrExp = {tag: "StrExp"; val: string; }
export const makeStrExp = (s: string): StrExp => ({tag: "StrExp", val: s});
export const isStrExp = (x: any): x is StrExp => x.tag === "StrExp";

export type PrimOp = {tag: "PrimOp"; op: PrimOpKeyword; }
export const makePrimOp = (op: PrimOpKeyword): PrimOp => ({tag: "PrimOp", op: op});
export const isPrimOp = (x: any): x is PrimOp => x.tag === "PrimOp" || x.op === 'cons' || x.op === 'car' || x.op === 'cdr';

export type VarRef = {tag: "VarRef"; var: string; }
export const makeVarRef = (v: string): VarRef => ({tag: "VarRef", var: v});
export const isVarRef = (x: any): x is VarRef => x.tag === "VarRef";

export type VarDecl = {tag: "VarDecl"; var: string; texp: TExp}
export const makeVarDecl = (v: string, te: TExp): VarDecl => ({tag: "VarDecl", var: v, texp: te});
export const isVarDecl = (x: any): x is VarDecl => x.tag === "VarDecl";

export type AppExp = {tag: "AppExp"; rator: CExp; rands: CExp[]; }
export const makeAppExp = (rator: CExp, rands: CExp[]): AppExp =>
    ({tag: "AppExp", rator: rator, rands: rands});
export const isAppExp = (x: any): x is AppExp => x.tag === "AppExp";

export type IfExp = {tag: "IfExp"; test: CExp; then: CExp; alt: CExp; }
export const makeIfExp = (test: CExp, then: CExp, alt: CExp): IfExp =>
    ({tag: "IfExp", test: test, then: then, alt: alt});
export const isIfExp = (x: any): x is IfExp => x.tag === "IfExp";

export type ProcExp = {tag: "ProcExp"; args: VarDecl[], body: CExp[]; returnTE: TExp }
export const makeProcExp = (args: VarDecl[], body: CExp[], returnTE: TExp): ProcExp =>
    ({tag: "ProcExp", args: args, body: body, returnTE: returnTE});
export const isProcExp = (x: any): x is ProcExp => x.tag === "ProcExp";

export type Binding = {tag: "Binding"; var: VarDecl; val: CExp; }
export const makeBinding = (v: VarDecl, val: CExp): Binding =>
    ({tag: "Binding", var: v, val: val});
export const isBinding = (x: any): x is Binding => x.tag === "Binding";

export type LetExp = {tag: "LetExp"; bindings: Binding[]; body: CExp[]; }
export const makeLetExp = (bindings: Binding[], body: CExp[]): LetExp =>
    ({tag: "LetExp", bindings: bindings, body: body});
export const isLetExp = (x: any): x is LetExp => x.tag === "LetExp";

export type LitExp = {tag: "LitExp"; val: SExpValue; }
export const makeLitExp = (val: SExpValue): LitExp => ({tag: "LitExp", val: val});
export const isLitExp = (x: any): x is LitExp => x.tag === "LitExp";

export type LetrecExp = {tag: "LetrecExp"; bindings: Binding[]; body: CExp[]; }
export const makeLetrecExp = (bindings: Binding[], body: CExp[]): LetrecExp =>
    ({tag: "LetrecExp", bindings: bindings, body: body});
export const isLetrecExp = (x: any): x is LetrecExp => x.tag === "LetrecExp";

export type SetExp = {tag: "SetExp"; var: VarRef; val: CExp; }
export const makeSetExp = (v: VarRef, val: CExp): SetExp =>
    ({tag: "SetExp", var: v, val: val});
export const isSetExp = (x: any): x is SetExp => x.tag === "SetExp";

// To help parser - define a type for reserved key words.
export type SpecialFormKeyword = "lambda" | "let" | "letrec" | "if" | "set!" | "quote";
const isSpecialFormKeyword = (x: string): x is SpecialFormKeyword =>
    ["if", "lambda", "let", "quote", "letrec", "set!"].includes(x);

/*
    ;; <prim-op>  ::= + | - | * | / | < | > | = | not | and | or | eq? | string=?
    ;;                  | cons | car | cdr | pair? | number? | list
    ;;                  | boolean? | symbol? | string?      ##### L3
*/
export type PrimOpKeyword = "+" | "-" | "*" | "/" | ">" | "<" | "=" | "not" | "and" | "or" | "eq?" | "string=?" | 
        "cons" | "car" | "cdr" | "list" | "pair?" | "list?" | "number?" | "boolean?" | "symbol?" | "string?" |
        "display" | "newline";
const isPrimOpKeyword = (x: string): x is PrimOpKeyword =>
    ["+", "-", "*", "/", ">", "<", "=", "not", "and", "or", 
     "eq?", "string=?", "cons", "car", "cdr", "list", "pair?",
     "list?", "number?", "boolean?", "symbol?", "string?", "display", "newline"].includes(x);

// ========================================================
// Parsing

export const parseL5 = (x: string): Result<Program> =>
    bind(p(x), parseL5Program);

export const parseL5Program = (sexp: Sexp): Result<Program> =>
    isToken(sexp) ? makeFailure(`Program cannot be a single token: ${format(sexp)}`) :
    isNonEmptyList<Sexp>(sexp) ? parseL5GoodProgram(first(sexp), rest(sexp)) :
    makeFailure("Unexpected empty program");

const parseL5GoodProgram = (keyword: Sexp, body: Sexp[]): Result<Program> =>
    keyword === "L5" && !isEmpty(body) ? mapv(mapResult(parseL5Exp, body), (exps: Exp[]) => makeProgram(exps)) :
    makeFailure(`Program must be of the form (L5 <exp>+): ${format([keyword, ...body])}`);

export const parseL5Exp = (sexp: Sexp): Result<Exp> =>
    isNonEmptyList<Sexp>(sexp) ? parseL5CompoundExp(first(sexp), rest(sexp)) :
    isToken(sexp) ? parseL5Atomic(sexp) :
    makeFailure("Exp cannot be an empty list");

export const parseL5CompoundExp = (op: Sexp, params: Sexp[]): Result<Exp> =>
    op === "define" ? parseDefine(params) :
    op === "quote" ?
        (params.length !== 1 ? makeFailure(`Bad quote expression: ${format([op, ...params])}`) :
         bind(parseSExp(params[0]), (val: SExpValue) => makeOk(makeLitExp(val)))) :
    parseL5CompoundCExp(op, params);


export const parseL5CompoundCExp = (op: Sexp, params: Sexp[]): Result<CExp> =>
    isString(op) && isSpecialFormKeyword(op) ? parseL5SpecialForm(op, params) :
    parseAppExp(op, params);

export const parseL5SpecialForm = (op: SpecialFormKeyword, params: Sexp[]): Result<CExp> =>
    isNonEmptyList<Sexp>(params) ?
        op === "if" ? parseIfExp(params) :
        op === "lambda" ? parseProcExp(first(params), rest(params)) :
        op === "let" ? parseLetExp(first(params), rest(params)) :
        op === "quote" ? parseLitExp(first(params)) :
        op === "letrec" ? parseLetrecExp(first(params), rest(params)) :
        op === "set!" ? parseSetExp(params) :
        makeFailure(`Unknown special form: ${op}`) :
    makeFailure("Empty args for special form");

export const parseDefine = (params: Sexp[]): Result<DefineExp> =>
    isNonEmptyList<Sexp>(params) ?
        (params.length === 1) ? makeFailure(`define missing 1 arguments: ${format(params)}`) :
        (params.length > 2) ? makeFailure(`define has too many arguments: ${format(params)}`) :
        parseGoodDefine(first(params), second(params)) :
    makeFailure("define missing 2 arguments");

const parseGoodDefine = (variable: Sexp, val: Sexp): Result<DefineExp> =>
    ! isConcreteVarDecl(variable) ? makeFailure(`First arg of define must be an identifier: ${format(variable)}`) :
    bind(parseVarDecl(variable), (varDecl: VarDecl) =>
        mapv(parseL5CExp(val), (val: CExp) =>
            makeDefineExp(varDecl, val)));

export const parseL5Atomic = (token: Token): Result<AtomicExp> =>
    token === "#t" ? makeOk(makeBoolExp(true)) :
    token === "#f" ? makeOk(makeBoolExp(false)) :
    isString(token) && isNumericString(token) ? makeOk(makeNumExp(+token)) :
    isString(token) && isPrimOpKeyword(token) ? makeOk(makePrimOp(token)) :
    isString(token) ? makeOk(makeVarRef(token)) :
    makeOk(makeStrExp(token.toString()));

export const parseL5CExp = (sexp: Sexp): Result<CExp> =>
    isNonEmptyList<Sexp>(sexp) ? parseL5CompoundCExp(first(sexp), rest(sexp)) :
    isToken(sexp) ? parseL5Atomic(sexp) :
    makeFailure("CExp cannot be an empty list");

const parseAppExp = (op: Sexp, params: Sexp[]): Result<AppExp> =>
    bind(parseL5CExp(op), (rator: CExp) =>
        mapv(mapResult(parseL5CExp, params), (rands: CExp[]) =>
            makeAppExp(rator, rands)));

const parseIfExp = (params: Sexp[]): Result<IfExp> =>
    params.length !== 3 ? makeFailure(`Expression not of the form (if <cexp> <cexp> <cexp>): ${format(params)}`) :
    mapv(mapResult(parseL5CExp, params), (cexps: CExp[]) => 
        makeIfExp(cexps[0], cexps[1], cexps[2]));

// (lambda (<vardecl>*) [: returnTE]? <CExp>+)
const parseProcExp = (vars: Sexp, rest: Sexp[]): Result<ProcExp> => {
    if (isArray(vars)) {
        const args = mapResult(parseVarDecl, vars);
        const body = mapResult(parseL5CExp, rest[0] === ":" ? rest.slice(2) : rest);
        const returnTE = rest[0] === ":" ? parseTExp(rest[1]) : makeOk(makeFreshTVar());
        return bind(args, (args: VarDecl[]) =>
                    bind(body, (body: CExp[]) =>
                        mapv(returnTE, (returnTE: TExp) =>
                            makeProcExp(args, body, returnTE))));
    } else {
        return makeFailure(`Invalid args ${format(vars)}`)
    }
}

const isGoodBindings = (bindings: Sexp): bindings is [Sexp, Sexp][] =>
    isArray(bindings) && allT(isArray, bindings);

const parseLetExp = (bindings: Sexp, body: Sexp[]): Result<LetExp> =>
    isEmpty(body) ? makeFailure('Body of "let" cannot be empty') :
    ! isGoodBindings(bindings) ? makeFailure(`Invalid bindings: ${format(bindings)}`) :
    bind(parseBindings(bindings), (bdgs: Binding[]) =>
        mapv(mapResult(parseL5CExp, body), (body: CExp[]) =>
            makeLetExp(bdgs, body)));

const isConcreteVarDecl = (sexp: Sexp): boolean =>
    isIdentifier(sexp) ||
    (isArray(sexp) && sexp.length > 2 && isIdentifier(sexp[0]) && (sexp[1] === ':'));

export const parseVarDecl = (sexp: Sexp): Result<VarDecl> => {
    if (isString(sexp)) {
        return makeOk(makeVarDecl(sexp, makeFreshTVar()));
    } else if (isArray(sexp)) {
        const v = sexp[0];
        if (isString(v)) {
            return mapv(parseTExp(sexp[2]), (te: TExp) => makeVarDecl(v, te));
        } else {
            return makeFailure(`Invalid var ${format(sexp[0])}`);
        }
    } else {
        return makeFailure(`Var cannot be a SexpString: ${format(sexp)}`);
    }
}

const parseBindings = (bindings: [Sexp, Sexp][]): Result<Binding[]> =>
    bind(mapResult(parseVarDecl, map(b => b[0], bindings)), (vds: VarDecl[]) =>
        mapv(mapResult(parseL5CExp, map(b => b[1], bindings)), (vals: CExp[]) =>
            zipWith(makeBinding, vds, vals)));

const parseLetrecExp = (bindings: Sexp, body: Sexp[]): Result<LetrecExp> =>
    isEmpty(body) ? makeFailure('Body of "letrec" cannot be empty') :
    ! isGoodBindings(bindings) ? makeFailure(`Invalid bindings: ${format(bindings)}`) :
    bind(parseBindings(bindings), (bdgs: Binding[]) =>
        mapv(mapResult(parseL5CExp, body), (body: CExp[]) => 
            makeLetrecExp(bdgs, body)));

const parseSetExp = (params: Sexp[]): Result<SetExp> =>
    isNonEmptyList<Sexp>(params) ?
        (params.length === 1) ? makeFailure(`set! missing 1 argument: ${format(params)}`) :
        (params.length > 2) ? makeFailure(`set! has too many arguments: ${format(params)}`) :
        parseGoodSetExp(first(params), second(params)) :
    makeFailure("set! missing 2 arguments");
    
const parseGoodSetExp = (variable: Sexp, val: Sexp): Result<SetExp> =>
    ! isIdentifier(variable) ? makeFailure(`First arg of set! must be an identifier: ${format(variable)}`) :
    mapv(parseL5CExp(val), (val: CExp) => makeSetExp(makeVarRef(variable), val));

// sexps has the shape (quote <sexp>)
export const parseLitExp = (param: Sexp): Result<LitExp> =>
    mapv(parseSExp(param), (sexp: SExpValue) => makeLitExp(sexp));

export const isDottedPair = (sexps: Sexp[]): boolean =>
    sexps.length === 3 && 
    sexps[1] === "."

export const makeDottedPair = (sexps : Sexp[]): Result<SExpValue> =>
    bind(parseSExp(sexps[0]), (val1: SExpValue) =>
        mapv(parseSExp(sexps[2]), (val2: SExpValue) =>
            makeCompoundSExp(val1, val2)));

// sexp is the output of p (sexp parser)
export const parseSExp = (sexp: Sexp): Result<SExpValue> =>
    sexp === "#t" ? makeOk(true) :
    sexp === "#f" ? makeOk(false) :
    isString(sexp) && isNumericString(sexp) ? makeOk(+sexp) :
    // תמיכה ב־'x => (quote x)
    isString(sexp) && sexp.startsWith("'") ?
        bind(parseSExp(sexp.slice(1)), (quoted) =>
            makeOk(makeCompoundSExp(
                makeSymbolSExp("quote"),
                makeCompoundSExp(quoted, makeEmptySExp())
            ))
        ) :
    isSexpString(sexp) ? makeOk(sexp.toString()) :
    isString(sexp) ? makeOk(makeSymbolSExp(sexp)) :
    isArray(sexp) && isDottedPair(sexp) ? makeDottedPair(sexp) :
    isNonEmptyList<Sexp>(sexp) ? (
        // fail on (x . y z)
        sexp[0] === '.' ? makeFailure(`Bad dotted sexp: ${format(sexp)}`) : 
        bind(parseSExp(first(sexp)), (val1: SExpValue) =>
            mapv(parseSExp(rest(sexp)), (val2: SExpValue) =>
                makeCompoundSExp(val1, val2)))
    ) :
    makeOk(makeEmptySExp());

// ==========================================================================
// Unparse: Map an AST to a concrete syntax string.

export const unparse = (e: Parsed): Result<string> =>
    // NumExp | StrExp | BoolExp | PrimOp | VarRef
    isNumExp(e) ? makeOk(`${e.val}`) :
    isStrExp(e) ? makeOk(`"${e.val}"`) :
    isBoolExp(e) ? makeOk(e.val ? "#t" : "#f") :
    isPrimOp(e) ? makeOk(e.op) :
    isVarRef(e) ? makeOk(e.var) :
    // AppExp | IfExp | ProcExp | LetExp | LitExp | LetrecExp | SetExp
    isAppExp(e) ? bind(unparse(e.rator), (rator: string) =>
                    mapv(mapResult(unparse, e.rands), (rands: string[]) =>
                        `(${rator} ${join(" ", rands)})`)) :
    isIfExp(e) ? bind(unparse(e.test), (test: string) =>
                    bind(unparse(e.then), (then: string) =>
                        mapv(unparse(e.alt), (alt: string) => 
                            `(if ${test} ${then} ${alt})`))) :
    isLetExp(e) ? unparseLetExp(e) :
    isLetrecExp(e) ? unparseLetrecExp(e) :
    isProcExp(e) ? unparseProcExp(e) :
    isLitExp(e) ? unparseTExp(typeOfSExpValue(e.val)) :
    isSetExp(e) ? unparseSetExp(e) :
    // DefineExp | Program
    
    isDefineExp(e) ? bind(unparseVarDecl(e.var), (vd: string) =>
                        mapv(unparse(e.val), (val: string) =>
                            `(define ${vd} ${val})`)) :
    isProgram(e) ? mapv(unparseLExps(e.exps), (exps: string) => `(L5 ${exps})`) :
    e;

const unparseReturn = (te: TExp): Result<string> =>
    isTVar(te) ? makeOk("") :
    mapv(unparseTExp(te), (te: string) => ` : ${te}`);

const unparseBindings = (bindings: Binding[]): Result<string> =>
    mapv(mapResult(bdg => bind(unparseVarDecl(bdg.var), (vd: string) =>
                            mapv(unparse(bdg.val), (val: string) => `(${vd} ${val})`)),
                   bindings), (bdgs: string[]) => 
            join(" ", bdgs));

const unparseVarDecl = (vd: VarDecl): Result<string> =>
    isTVar(vd.texp) ? makeOk(vd.var) :
    mapv(unparseTExp(vd.texp), te => `(${vd.var} : ${te})`);

// Add a quote for symbols, empty and compound sexp - strings and numbers are not quoted.
const unparseLitExp = (le: LitExp): string =>
    isEmptySExp(le.val) ? `'()` :
    isSymbolSExp(le.val) ? `'${valueToString(le.val)}` :
    isCompoundSExp(le.val) ? `'${valueToString(le.val)}` :
    `${le.val}`;

const unparseLExps = (les: Exp[]): Result<string> =>
    mapv(mapResult(unparse, les), (les: string[]) => join(" ", les));

const unparseProcExp = (pe: ProcExp): Result<string> =>
    bind(mapResult(unparseVarDecl, pe.args), (vds: string[]) =>
        bind(unparseReturn(pe.returnTE), (ret: string) =>
            mapv(unparseLExps(pe.body), (body: string) =>
            `(lambda (${join(" ", vds)})${ret} ${body})`)));

const unparseLetExp = (le: LetExp) : Result<string> => 
    bind(unparseBindings(le.bindings), (bdgs: string) =>
        mapv(unparseLExps(le.body), (body: string) => 
            `(let (${bdgs}) ${body})`));

const unparseLetrecExp = (le: LetrecExp): Result<string> =>
    bind(unparseBindings(le.bindings), (bdgs: string) => 
        mapv(unparseLExps(le.body), (body: string) =>
            `(letrec (${bdgs}) ${body})`));

const unparseSetExp = (se: SetExp): Result<string> =>
    mapv(unparse(se.val), (val: string) => `(set! ${se.var.var} ${val})`);
