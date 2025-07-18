// ===========================================================
// AST type models
import { eqProps, map, zipWith } from "ramda";
import { makeEmptySExp, makeSymbolSExp, SExpValue, makeCompoundSExp, valueToString } from './L32-value'
import { first, second, rest, allT, isEmpty, isNonEmptyList, List, NonEmptyList } from "../shared/list";
import { isArray, isString, isNumericString, isIdentifier } from "../shared/type-predicates";
import { Result, makeOk, makeFailure, bind, mapResult, mapv } from "../shared/result";
import { parse as p, isSexpString, isToken, isCompoundSexp } from "../shared/parser";
import { Sexp, Token } from "s-expression";

/*
;; =============================================================================
;; Scheme Parser
;;
;; L2 extends L1 with support for IfExp and ProcExp
;; L3 extends L2 with support for:
;; - Pair datatype
;; - The empty-list literal expression
;; - Compound literal expressions denoted with quote
;; - Primitives: cons, car, cdr, pair?, number?, boolean?, symbol?, string?, list
;; - Primitives: and, or, not
;; - The Let abbreviation is also supported.

;; <program> ::= (L32 <exp>+) // Program(exps:List(Exp))
;; <exp> ::= <define> | <cexp>              / DefExp | CExp
;; <define> ::= ( define <var> <cexp> )     / DefExp(var:VarDecl, val:CExp)
;; <var> ::= <identifier>                   / VarRef(var:string)
;; <cexp> ::= <number>                      / NumExp(val:number)
;;         |  <boolean>                     / BoolExp(val:boolean)
;;         |  <string>                      / StrExp(val:string)
;;         |  ( lambda ( <var>* ) <cexp>+ ) / ProcExp(args:VarDecl[], body:CExp[]))
;;         |  ( if <cexp> <cexp> <cexp> )   / IfExp(test: CExp, then: CExp, alt: CExp)
;;         |  ( let ( binding* ) <cexp>+ )  / LetExp(bindings:Binding[], body:CExp[]))
;;         |  ( quote <sexp> )              / LitExp(val:SExp)
;;         |  ( <cexp> <cexp>* )            / AppExp(operator:CExp, operands:CExp[]))
;;         |  (dict (<symbol> <cexp>)*)     / DictExp(pairs: [string, CExp][]) // Yuval
;; <binding>  ::= ( <var> <cexp> )           / Binding(var:VarDecl, val:Cexp)
;; <prim-op>  ::= + | - | * | / | < | > | = | not |  and | or | eq? | string=?
;;                  | cons | car | cdr | pair? | number? | list 
;;                  | boolean? | symbol? | string?      ##### L3
;; <num-exp>  ::= a number token
;; <bool-exp> ::= #t | #f
;; <var-ref>  ::= an identifier token
;; <var-decl> ::= an identifier token
;; <sexp>     ::= symbol | number | bool | string | 
;;                (<sexp>+ . <sexp>) | ( <sexp>* )       ##### L3
*/

export type Exp = DefineExp | CExp;
export type AtomicExp = NumExp | BoolExp | StrExp | PrimOp | VarRef;
export type CompoundExp = AppExp | IfExp | ProcExp | LetExp | LitExp;
export type CExp =  AtomicExp | CompoundExp | DictExp; // Yuval

export type Program = {tag: "Program"; exps: Exp[]; }
export type DefineExp = {tag: "DefineExp"; var: VarDecl; val: CExp; }
export type NumExp = {tag: "NumExp"; val: number; }
export type BoolExp = {tag: "BoolExp"; val: boolean; }
export type StrExp = {tag: "StrExp"; val: string; }
export type PrimOp = {tag: "PrimOp"; op: string; }
export type VarRef = {tag: "VarRef"; var: string; }
export type VarDecl = {tag: "VarDecl"; var: string; }
export type AppExp = {tag: "AppExp"; rator: CExp; rands: CExp[]; }
// L2
export type IfExp = {tag: "IfExp"; test: CExp; then: CExp; alt: CExp; }
export type ProcExp = {tag: "ProcExp"; args: VarDecl[], body: CExp[]; }
export type Binding = {tag: "Binding"; var: VarDecl; val: CExp; }
export type LetExp = {tag: "LetExp"; bindings: Binding[]; body: CExp[]; }
// L3
export type LitExp = {tag: "LitExp"; val: SExpValue; }
export type DictExp = {tag: "DictExp"; pairs: [string, CExp][]; } //YUVAL


// Type value constructors for disjoint types
export const makeProgram = (exps: Exp[]): Program => ({tag: "Program", exps: exps});
export const makeDefineExp = (v: VarDecl, val: CExp): DefineExp =>
    ({tag: "DefineExp", var: v, val: val});
export const makeNumExp = (n: number): NumExp => ({tag: "NumExp", val: n});
export const makeBoolExp = (b: boolean): BoolExp => ({tag: "BoolExp", val: b});
export const makeStrExp = (s: string): StrExp => ({tag: "StrExp", val: s});
export const makePrimOp = (op: string): PrimOp => ({tag: "PrimOp", op: op});
export const makeVarRef = (v: string): VarRef => ({tag: "VarRef", var: v});
export const makeVarDecl = (v: string): VarDecl => ({tag: "VarDecl", var: v});
export const makeAppExp = (rator: CExp, rands: CExp[]): AppExp =>
    ({tag: "AppExp", rator: rator, rands: rands});
// L2
export const makeIfExp = (test: CExp, then: CExp, alt: CExp): IfExp =>
    ({tag: "IfExp", test: test, then: then, alt: alt});
export const makeProcExp = (args: VarDecl[], body: CExp[]): ProcExp =>
    ({tag: "ProcExp", args: args, body: body});
export const makeBinding = (v: string, val: CExp): Binding =>
    ({tag: "Binding", var: makeVarDecl(v), val: val});
export const makeLetExp = (bindings: Binding[], body: CExp[]): LetExp =>
    ({tag: "LetExp", bindings: bindings, body: body});
// L3
export const makeLitExp = (val: SExpValue): LitExp =>
    ({tag: "LitExp", val: val});


// Type predicates for disjoint types
export const isProgram = (x: any): x is Program => x.tag === "Program";
export const isDefineExp = (x: any): x is DefineExp => x.tag === "DefineExp";

export const isNumExp = (x: any): x is NumExp => x.tag === "NumExp";
export const isBoolExp = (x: any): x is BoolExp => x.tag === "BoolExp";
export const isStrExp = (x: any): x is StrExp => x.tag === "StrExp";
export const isPrimOp = (x: any): x is PrimOp => x.tag === "PrimOp";
export const isVarRef = (x: any): x is VarRef => x.tag === "VarRef";
export const isVarDecl = (x: any): x is VarDecl => x.tag === "VarDecl";
export const isAppExp = (x: any): x is AppExp => x.tag === "AppExp";
// L2
export const isIfExp = (x: any): x is IfExp => x.tag === "IfExp";
export const isProcExp = (x: any): x is ProcExp => x.tag === "ProcExp";
export const isBinding = (x: any): x is Binding => x.tag === "Binding";
export const isLetExp = (x: any): x is LetExp => x.tag === "LetExp";
// L3
export const isLitExp = (x: any): x is LitExp => x.tag === "LitExp";
export const isDictExp = (x: any): x is DictExp => x.tag === "DictExp"; // Yuval


// Type predicates for type unions
export const isExp = (x: any): x is Exp => isDefineExp(x) || isCExp(x);
export const isAtomicExp = (x: any): x is AtomicExp =>
    isNumExp(x) || isBoolExp(x) || isStrExp(x) ||
    isPrimOp(x) || isVarRef(x);
export const isCompoundExp = (x: any): x is CompoundExp =>
    isAppExp(x) || isIfExp(x) || isProcExp(x) || isLitExp(x) || isLetExp(x);
export const isCExp = (x: any): x is CExp =>
    isAtomicExp(x) || isCompoundExp(x);

// ========================================================
// Parsing

export const parseL32 = (x: string): Result<Program> =>
    bind(p(x), parseL32Program);

export const parseL32Program = (sexp: Sexp): Result<Program> =>
    sexp === "" || isEmpty(sexp) ? makeFailure("Unexpected empty program") :
    isToken(sexp) ? makeFailure(`Program cannot be a single token: ${format(sexp)}`) :
    isNonEmptyList<Sexp>(sexp) ? parseL32GoodProgram(first(sexp), rest(sexp)) :
    makeFailure(`Unexpected type ${format(sexp)}`);

const parseL32GoodProgram = (keyword: Sexp, body: Sexp[]): Result<Program> =>
    keyword === "L32" && !isEmpty(body) ? mapv(mapResult(parseL32Exp, body), (exps: Exp[]) => 
                                              makeProgram(exps)) :
    makeFailure(`Program must be of the form (L32 <exp>+): ${format([keyword, ...body])}`);

// Exp -> <DefineExp> | <Cexp>
export const parseL32Exp = (sexp: Sexp): Result<Exp> =>
    isCompoundSexp(sexp) ? 
        isNonEmptyList<Sexp>(sexp) ? parseL32CompoundExp(first(sexp), rest(sexp)) :
        makeFailure(`Exp cannot be an empty list: ${format(sexp)}`) :
    isToken(sexp) ? parseL32Atomic(sexp) :
    makeFailure(`Bad param: ${sexp}`);

// Compound -> DefineExp | CompoundCExp
export const parseL32CompoundExp = (op: Sexp, params: Sexp[]): Result<Exp> => 
    op === "define"? parseDefine(params) :
    parseL32CompoundCExp(op, params);

// CompoundCExp -> IfExp | ProcExp | LetExp | LitExp | AppExp
export const parseL32CompoundCExp = (op: Sexp, params: Sexp[]): Result<CExp> =>
    isString(op) && isSpecialForm(op) ? parseL32SpecialForm(op, params) :
    parseAppExp(op, params);

export const parseL32SpecialForm = (op: Sexp, params: Sexp[]): Result<CExp> =>
    isEmpty(params) ? makeFailure("Empty args for special form") :
    op === "if" ? parseIfExp(params) :
    op === "lambda" ? 
        isNonEmptyList<Sexp>(params) ? parseProcExp(first(params), rest(params)) :
        makeFailure(`Bad proc: ${params}`) :
    op === "let" ? 
        isNonEmptyList<Sexp>(params) ? parseLetExp(first(params), rest(params)) :
        makeFailure(`Bad let: ${params}`) :
    op === "quote" ? 
        isNonEmptyList<Sexp>(params) ? parseLitExp(first(params)) :
        makeFailure(`Bad quote exp: ${params}`) :
    op === "dict" ? parseDictExp(params) : // Yuval
    makeFailure("Never");

// DefineExp -> (define <varDecl> <CExp>)
export const parseDefine = (params: List<Sexp>): Result<DefineExp> =>
    isNonEmptyList<Sexp>(params) ? 
        isEmpty(rest(params)) ? makeFailure(`define missing 1 arguments: ${format(params)}`) :
        (params.length > 2) ? makeFailure(`define too many arguments: ${format(params)}`) :
        parseGoodDefine(first(params), second(params)) :
    makeFailure("define missing 2 arguments");

const parseGoodDefine = (variable: Sexp, val: Sexp): Result<DefineExp> =>
    ! isIdentifier(variable) ? makeFailure(`First arg of define must be an identifier: ${format(variable)}`) :
    mapv(parseL32CExp(val), (value: CExp) => 
         makeDefineExp(makeVarDecl(variable), value));

export const parseL32CExp = (sexp: Sexp): Result<CExp> =>
    isCompoundSexp(sexp) ? 
        isNonEmptyList<Sexp>(sexp) ? parseL32CompoundCExp(first(sexp), rest(sexp)) :
        makeFailure("CExp cannot be an empty list") :
    isToken(sexp) ? parseL32Atomic(sexp) :
    makeFailure(`Bad sexp: ${sexp}`);

// Atomic -> number | boolean | primitiveOp | string
export const parseL32Atomic = (token: Token): Result<CExp> =>
    token === "#t" ? makeOk(makeBoolExp(true)) :
    token === "#f" ? makeOk(makeBoolExp(false)) :
    isString(token) && isNumericString(token) ? makeOk(makeNumExp(+token)) :
    isString(token) && isPrimitiveOp(token) ? makeOk(makePrimOp(token)) :
    isString(token) ? makeOk(makeVarRef(token)) :
    makeOk(makeStrExp(token.toString()));

/*
    ;; <prim-op>  ::= + | - | * | / | < | > | = | not | and | or | eq? | string=?
    ;;                  | cons | car | cdr | pair? | number? | list
    ;;                  | boolean? | symbol? | string?      ##### L3
*/
const isPrimitiveOp = (x: string): boolean =>
    ["+", "-", "*", "/", ">", "<", "=", "not", "and", "or",
     "eq?", "string=?", "cons", "car", "cdr", "list", "pair?",
     "number?", "boolean?", "symbol?", "string?"].includes(x);

const isSpecialForm = (x: string): boolean =>
    ["if", "lambda", "let", "quote", "dict"].includes(x); // Yuval

const parseAppExp = (op: Sexp, params: Sexp[]): Result<AppExp> =>
    bind(parseL32CExp(op), (rator: CExp) => 
        mapv(mapResult(parseL32CExp, params), (rands: CExp[]) =>
             makeAppExp(rator, rands)));

const parseIfExp = (params: Sexp[]): Result<IfExp> =>
    params.length !== 3 ? makeFailure(`Expression not of the form (if <cexp> <cexp> <cexp>): ${format(params)}`) :
    mapv(mapResult(parseL32CExp, params), (cexps: CExp[]) => 
        makeIfExp(cexps[0], cexps[1], cexps[2]));

const parseProcExp = (vars: Sexp, body: Sexp[]): Result<ProcExp> =>
    isArray(vars) && allT(isString, vars) ? mapv(mapResult(parseL32CExp, body), (cexps: CExp[]) => 
                                                 makeProcExp(map(makeVarDecl, vars), cexps)) :
    makeFailure(`Invalid vars for ProcExp ${format(vars)}`);

const isGoodBindings = (bindings: Sexp): bindings is [string, Sexp][] =>
    isArray(bindings) &&
    allT<NonEmptyList<Sexp>>(isNonEmptyList, bindings) &&
    allT(isIdentifier, map(first, bindings));

const parseLetExp = (bindings: Sexp, body: Sexp[]): Result<LetExp> => {
    if (!isGoodBindings(bindings)) {
        return makeFailure('Malformed bindings in "let" expression');
    }
    // Given (letrec ( (var <val>) ...) <cexp> ...)
    // Return makeLetExp( [makeBinding(var, parse(<val>)) ...], [ parse(<cexp>) ...] )
    // After isGoodBindings, bindings has type [string, Sexp][]
    const vars = map(b => b[0], bindings);
    const valsResult = mapResult(parseL32CExp, map(second, bindings));
    const bindingsResult = mapv(valsResult, (vals: CExp[]) => zipWith(makeBinding, vars, vals));
    return bind(bindingsResult, (bindings: Binding[]) => 
                mapv(mapResult(parseL32CExp, body), (body: CExp[]) =>
                     makeLetExp(bindings, body)));
}

// sexps has the shape (quote <sexp>)
export const parseLitExp = (param: Sexp): Result<LitExp> =>
    mapv(parseSExp(param), (sexp: SExpValue) => 
         makeLitExp(sexp));

export const isDottedPair = (sexps: Sexp[]): boolean =>
    sexps.length === 3 && 
    sexps[1] === "."

export const makeDottedPair = (sexps : Sexp[]): Result<SExpValue> =>
    bind(parseSExp(sexps[0]), (val1: SExpValue) => 
        mapv(parseSExp(sexps[2]), (val2: SExpValue) =>
             makeCompoundSExp(val1, val2)));

// x is the output of p (sexp parser)
export const parseSExp = (sexp: Sexp): Result<SExpValue> =>
    sexp === "#t" ? makeOk(true) :
    sexp === "#f" ? makeOk(false) :
    isString(sexp) && isNumericString(sexp) ? makeOk(+sexp) :
    isSexpString(sexp) ? makeOk(sexp.toString()) :
    isString(sexp) ? makeOk(makeSymbolSExp(sexp)) :
    sexp.length === 0 ? makeOk(makeEmptySExp()) :
    isDottedPair(sexp) ? makeDottedPair(sexp) :
    isNonEmptyList<Sexp>(sexp) ? (
        // fail on (x . y z)
        first(sexp) === '.' ? makeFailure(`Bad dotted sexp: ${format(sexp)}`) : 
        bind(parseSExp(first(sexp)), (val1: SExpValue) =>
             mapv(parseSExp(rest(sexp)), (val2: SExpValue) =>
                  makeCompoundSExp(val1, val2))) 
        ) :
    makeFailure(`Bad sexp: ${sexp}`);

export const parseDictExp = (params: Sexp[]): Result<DictExp> => { // Yuval
    const parsePair = (sexp: Sexp): Result<[string, CExp]> => {
        if (!isNonEmptyList<Sexp>(sexp)) {
            return makeFailure(`Expected pair but got: ${format(sexp)}`);
        }

        const [keySexp, valSexp] = [first(sexp), second(sexp)];
        if (!isString(keySexp) || !isIdentifier(keySexp)) {
            return makeFailure(`Invalid key in dict: ${format(keySexp)}`);
        }

        return bind(parseL32CExp(valSexp), (cexp: CExp) =>
            makeOk<[string, CExp]>([keySexp, cexp])
        );
    };

    return bind(mapResult(parsePair, params), (pairs: [string, CExp][]) => {
        const keys = pairs.map(p => p[0]);
        const hasDuplicates = keys.some((k, i) => keys.indexOf(k) !== i);

        return hasDuplicates
            ? makeFailure("duplicate keys in dict expression")
            : makeOk({ tag: "DictExp", pairs });
    });
};


// ==========================================================================
// Unparse: Map an AST to a concrete syntax string.

import { isSymbolSExp, isEmptySExp, isCompoundSExp } from './L32-value';
import { format } from "../shared/format";
import exp from "constants";
import { parse } from "path";

// Add a quote for symbols, empty and compound sexp - strings and numbers are not quoted.
const unparseLitExp = (le: LitExp): string =>
    isEmptySExp(le.val) ? `'()` :
    isSymbolSExp(le.val) ? `'${valueToString(le.val)}` :
    isCompoundSExp(le.val) ? `'${valueToString(le.val)}` :
    `${le.val}`;

const unparseLExps = (les: Exp[]): string =>
    map(unparseL32, les).join(" ");

const unparseProcExp = (pe: ProcExp): string => 
    `(lambda (${map((p: VarDecl) => p.var, pe.args).join(" ")}) ${unparseLExps(pe.body)})`

const unparseLetExp = (le: LetExp) : string => 
    `(let (${map((b: Binding) => `(${b.var.var} ${unparseL32(b.val)})`, le.bindings).join(" ")}) ${unparseLExps(le.body)})`

export const unparseL32 = (exp: Program | Exp): string =>
    isBoolExp(exp) ? valueToString(exp.val) :
    isNumExp(exp) ? valueToString(exp.val) :
    isStrExp(exp) ? valueToString(exp.val) :
    isLitExp(exp) ? unparseLitExp(exp) :
    isVarRef(exp) ? exp.var :
    isProcExp(exp) ? unparseProcExp(exp) :
    isIfExp(exp) ? `(if ${unparseL32(exp.test)} ${unparseL32(exp.then)} ${unparseL32(exp.alt)})` :
    isAppExp(exp) ? `(${unparseL32(exp.rator)} ${unparseLExps(exp.rands)})` :
    isPrimOp(exp) ? exp.op :
    isLetExp(exp) ? unparseLetExp(exp) :
    isDefineExp(exp) ? `(define ${exp.var.var} ${unparseL32(exp.val)})` :
    isProgram(exp) ? `(L32 ${unparseLExps(exp.exps)})` :
    isDictExp(exp) ? `(dict ${exp.pairs.map(([k, v]) => `(${k} ${unparseL32(v)})`).join(" ")})` : // Yuval
    exp;
