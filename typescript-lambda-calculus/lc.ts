type Expr = Var<string>|Abs<any, any>|App<any, any>;
type StringOfExpr<T extends Expr>
    = T extends Var<any> ? `${T['x']}`
    : T extends Abs<any,any> ?
        StringOfExpr<T['b']> extends infer B ? B extends string ?
        `(λ${T['s']}. ${B})` : never : never
    : T extends App<any,any> ?
        StringOfExpr<T['e1']> extends infer E1 ? E1 extends string ?
        StringOfExpr<T['e2']> extends infer E2 ? E2 extends string ?
        `(${E1} ${E2})` : never : never : never : never
    : never;
type Var<X extends string> = {x: X};
type Abs<S extends string, B extends Expr> = {s: S, b: B};
type App<E1 extends Expr, E2 extends Expr> = {e1: E1, e2: E2};

type ParseVar<T> = T extends `${infer X}` ? Var<X> : never;
type ParseAbs<T> = T extends `(λ${infer S}. ${infer B})` ? Abs<S, ParseExpr<B>> : never;
type ParseApp<T> = T extends `(${infer E1} ${infer E2})` ? App<ParseExpr<E1>, ParseExpr<E2>> : never;
type ParseExpr<T> =
    ParseAbs<T> extends never
    ? ( ParseApp<T> extends never
        ? ( ParseVar<T> extends never
            ? never
            : ParseVar<T> )
        : ParseApp<T> )
    : ParseAbs<T>;

type Free<T extends Expr>
    = T extends Var<infer X> ? X
    : T extends Abs<infer S, infer B> ? Exclude<Free<B>, S>
    : T extends App<infer E1, infer E2> ? Free<E1>|Free<E2> : never;

let free_test1: Free<ParseExpr<"(x z)">> extends 'x'|'z' ?
                'x'|'z' extends Free<ParseExpr<"(x z)">> ? true : false : false
                = true;
let free_test2: Free<ParseExpr<"(λz. (x (y z)))">> extends 'x'|'y' ?
                'x'|'y' extends Free<ParseExpr<"(λz. (x (y z)))">> ? true : false : false
                = true;

type Freshen<X extends string, Used> = X extends Used ? Freshen<`${X}'`, Used> : X;
const freshen_test1: Freshen<"x", "x"|"y"|"x'"> = `x''`;

type Subst_<V extends string, E extends Expr, Subs extends Ctx<Expr>, FreeInSub>
    = E extends Var<infer U> ? (Find<Expr, U, Subs> extends infer R ? R extends Expr ? R : E : never)
    : E extends App<infer E1, infer E2> ?
        Subst_<V, E1, Subs, FreeInSub> extends infer E11 ? E11 extends Expr ?
        Subst_<V, E2, Subs, FreeInSub> extends infer E21 ? E21 extends Expr ?
        App<E11, E21> : never : never : never : never
    : E extends Abs<infer S, infer B> ? (
        /*  Case 1: S = V, shadowing V, so no need to substitute at all. */
        S extends V ? E :
        /*  Case 2: S \in Free(X), so naively substituting X in the body will
            change what S in X refers to. For example, [x->y](λy. x) => λy. y,
            which is clearly incorrect.
            Instead, come up with a fresh name S1 for S.
            Replace S for S1 in the body, and then finally substitute X in. */
        S extends FreeInSub ?
            (FreeInSub|Free<B> extends infer FreeXB ?
            Freshen<S, FreeXB> extends infer S1 ? S1 extends string ?
            Subst_<V, B, [[S, Var<S1>], ...Subs], FreeInSub> extends infer B1 ? B1 extends Expr ?
            Abs<S1, B1> : never : never : never : never : never) :
        /*  Case 3: X doesn't conflict with the introduced binder S, so direct
            substitution is fine. */
        Subst_<V, B, Subs, FreeInSub> extends infer B1 ? B1 extends Expr ? Abs<S, B1> : never : never
    )
    : never
type Subst<V extends string, X extends Expr, E extends Expr> = Subst_<V, E, [[V, X]], Free<X>>;

type SubstTest<X extends string, E extends string, B extends string> =
    Subst<X, ParseExpr<E>, ParseExpr<B>> extends infer R ? R extends Expr ?
    StringOfExpr<R> : never : never;
const subst_test1: SubstTest<"x", "y", "(λz. x)"> = "(λz. y)";
const subst_test2: SubstTest<"x", "y", "(λy. x)"> = "(λy'. y)";
const subst_test3: SubstTest<"x", "y", "(λx. x)"> = "(λx. x)";

type TupToNum<T extends unknown[]> = T extends {length: infer N} ? N : never;
type NumToTup<N extends number, Tup extends unknown[] = []> =
    Tup extends {length: N} ? Tup : NumToTup<N, [...Tup, unknown]>;
type Incr<N extends number> = TupToNum<[...NumToTup<N>, unknown]>;
const incr_test: Incr<11> = 12;

type Ctx<T> = [string, T][];

type NotFound = {__brand: 'Not found'}
type Find<T, X extends string, L extends Ctx<T>> =
    L extends [[infer Y, infer N], ...infer B] ?
    (Y extends X ? N : B extends Ctx<T> ? Find<T, X, B> : never)
    : NotFound;
const find_test : Find<number, "a", [["b", 10], ["a", 7], ["c", 15]]> = 7;

type AlphaEq<E1 extends Expr, E2 extends Expr,
            L1 extends Ctx<number> = [], L2 extends Ctx<number> = [], Fr extends number = 0>
    = E1 extends Var<infer X1> ? (E2 extends Var<infer X2>
        ? (Find<number, X1, L1> extends number
            ? (Find<number, X2, L2> extends number
                ? Find<number, X1, L1> extends Find<number, X2, L2> ? true : false
                : false)
            : (Find<number, X2, L2> extends number
                ? false
                : X1 extends X2 ? true : false))
        : false)
    : E1 extends App<infer E11, infer E12> ? (E2 extends App<infer E21, infer E22> ?
        AlphaEq<E11, E21, L1, L2, Fr> extends true ? AlphaEq<E12, E22, L1, L2, Fr> extends true ? true
        : false : false : false)
    : E1 extends Abs<infer S1, infer B1> ? (E2 extends Abs<infer S2, infer B2> ?
        Incr<Fr> extends infer Fr1 ?
        Fr1 extends number ?
        AlphaEq<B1, B2, [[S1, Fr1], ...L1], [[S2, Fr1], ...L2], Fr1> : never : never : false)
    : never

type AlphaEqTest<E1 extends string, E2 extends string> = AlphaEq<ParseExpr<E1>, ParseExpr<E2>>;
const alpha_eq_test1: AlphaEqTest<"(λx. x)", "(λy. y)"> = true;
const alpha_eq_test2: AlphaEqTest<"(λx. z)", "(λy. z)"> = true;
const alpha_eq_test3: AlphaEqTest<"(λz. (λx. (z x)))", "(λx. (λw. (x w)))"> = true;
const alpha_eq_test4: AlphaEqTest<"(y (λx. (λz. (z x))))", "(y (λx. (λw. (w x))))"> = true;
const alpha_eq_test5: AlphaEqTest<"(u (λx. (λz. (z x))))", "(y (λx. (λw. (w x))))"> = false;

type BetaNF<E extends Expr>
    = E extends App<infer F, infer A> ?
        (BetaNF<F> extends infer F ? F extends Expr ?
            (F extends Abs<infer S, infer B>
            ? (Subst<S, A, B> extends infer B1 ? B1 extends Expr ? BetaNF<B1> : never : never)
            : BetaNF<A> extends infer A1 ? A1 extends Expr ? App<F, A1> : never : never) : never : never)
    : E extends Abs<infer S, infer B> ?
        BetaNF<B> extends infer B1 ? B1 extends Expr ? Abs<S, B1> : never : never
    : E;

type BetaNFTest<E extends Expr> =
    BetaNF<E> extends infer PE ? PE extends Expr ?  StringOfExpr<PE> extends infer R ? R extends string ?
    StringOfExpr<E> extends infer E ? E extends string ?
    `${E} => ${R}` : never : never : never : never : never : never;
const nf_test1: BetaNFTest<ParseExpr<"(x y)">>
    = "(x y) => (x y)";
const nf_test2: BetaNFTest<App<Var<"x">, App<Abs<"y", Var<"y">>, Var<"z">>>>
    = "(x ((λy. y) z)) => (x z)";
const nf_test3: BetaNFTest<Abs<"x", App<Abs<"y", Var<"y">>, Var<"z">>>>
    = "(λx. ((λy. y) z)) => (λx. z)";
const nf_test4: BetaNFTest<App<Abs<"x", Var<"z">>, App<Abs<"w", App<App<Var<"w">, Var<"w">>, Var<"w">>>,
                                                   Abs<"w", App<App<Var<"w">, Var<"w">>, Var<"w">>>>>>
    = "((λx. z) ((λw. ((w w) w)) (λw. ((w w) w)))) => z";

type ABEq<E1 extends Expr, E2 extends Expr> =
    BetaNF<E1> extends infer E1 ? E1 extends Expr ?
    BetaNF<E2> extends infer E2 ? E2 extends Expr ?
    AlphaEq<E1, E2> : never : never : never : never;

type ABEqTest<E1 extends Expr, E2 extends Expr> =
    StringOfExpr<E1> extends infer SE1 ? SE1 extends string ?
    StringOfExpr<E2> extends infer SE2 ? SE2 extends string ?
    ABEq<E1, E2> extends true ?
    `${SE1} ≡ ${SE2}` : `${SE1} ≢ ${SE2}` : never : never : never : never;
type Zero = ParseExpr<"(λs. (λz. z))">;
type One = ParseExpr<"(λs. (λz. (s z)))">;
type Two = ParseExpr<"(λs. (λz. (s (s z))))">;
type Add = Abs<"m", Abs<"n", Abs<"s", Abs<"z",
                App<ParseExpr<"(m s)">, App<ParseExpr<"(n s)">, Var<"z">>>>>>>;
const abeq_test1: ABEqTest<App<App<Add, One>, One>, Two>
    = "(((λm. (λn. (λs. (λz. ((m s) ((n s) z)))))) (λs. (λz. (s z)))) (λs. (λz. (s z)))) ≡ (λs. (λz. (s (s z))))";
