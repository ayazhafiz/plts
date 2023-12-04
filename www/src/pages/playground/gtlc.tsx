import type * as monaco from "monaco-editor";
import * as React from "react";
import ReactMarkdown from "react-markdown";
import Playground from "../../components/playground";
import type {Backend, LanguageRegistration} from "../../common/types";
import {C, TS} from "../../common/evaluator";
import {shapeBackend, shapeBackendP} from "../../common/util";
import {
  infer,
  irCompile,
  cCompile,
  tsCompile,
  doEval,
  docs,
  getHover,
} from "gtlc";

const examples = {
  "Cast Error": `(λx. succ x) #t`,
  "Closure Passing": `
let apply1To = λf: ? -> nat. f 1 in
apply1To (λx: nat. succ x)
`.trim(),
  "Large Closures": `
let add8 =
  \\a. \\b. \\c. \\d. \\e. \\f. \\g. \\h.
  add a (add b (add c (add d (add e (add f (add g h)))))) in
add8 1 2 3 4 5 6 7 8
`.trim(),
  "Fixpoint Combinator": `
let fix = λf. (λx. f (λy. x x y)) (λx. f (λy. x x y)) in
let fib = λn: _.
  let go' = λgo: _. λi: _. λa: _. λb: _.
    if eqn i n
    then a
    else go (succ i) b (add a b)
  in
  let go = fix go' in
  go 0 0 1
in
fib 23
`.trim(),
  "Type Inference": `
\\f: ((? -> nat) -> (bool -> ?)) -> ((nat -> ?) -> (? -> bool)) -> nat.
  \\y: _.
    f y y
`.trim(),
  References: `
let fact_cell: _ = ref (\\n: _. 0) in
fact_cell := \\n: _.
    if eqn n 0
    then 1
    else mult n (!fact_cell (pred n));
let fact: _ = !fact_cell in
fact 6
`.trim(),
};

const fmtOptions: [["width", number]] = [["width", 55]];
const compileOptions: [["optimize", boolean], ["width", number]] = [
  ["optimize", true],
  ...fmtOptions,
];

const backends: {
  Infer: [Backend];
} & {
    [K in "Compiler IR" | "TypeScript" | "C"]: [Backend, Backend];
  } = {
  Infer: [
    {
      title: "Infer",
      editorLanguage: "gtlc",
      ...shapeBackend(infer, fmtOptions),
    },
  ],
  "Compiler IR": [
    {
      title: "Lift IR",
      editorLanguage: "liftIr",
      ...shapeBackend(irCompile, compileOptions),
    },
    {
      title: "Interpreter Execution",
      editorLanguage: "gtlc",
      ...shapeBackend(doEval, fmtOptions),
    },
  ],
  TypeScript: [
    {
      title: "TypeScript",
      editorLanguage: "typescript",
      ...shapeBackend(tsCompile, compileOptions),
    },
    {
      title: TS.title,
      editorLanguage: "javascript",
      ...shapeBackendP((input: string, width: number) => {
        const ts = tsCompile(input, true, width);
        return TS.eval(ts);
      }, fmtOptions),
    },
  ],
  C: [
    {
      title: "C",
      editorLanguage: "c",
      ...shapeBackend(cCompile, compileOptions),
    },
    {
      title: C.title,
      editorLanguage: "c",
      ...shapeBackendP((input: string, width: number) => {
        const c = cCompile(input, true, width);
        return C.eval(c);
      }, fmtOptions),
    },
  ],
};

const builtin_docs = docs;
const builtin_fns = builtin_docs.map((d) => d.name);

const grammar = (
  <ReactMarkdown>
    {`
**Expressions**

- \`x\`: variables
- \`100\`: numbers
- \`#t|#f\`: the true and false literals
- \`(\\|λ)x: t. e\`: a lambda expression with \`x\` bound to type \`t\`
- \`let x: t = e1 in e2\`: binds \`x\` to \`e1\` and then evaluates \`e2\`
- \`f a\`: application to a lambda
- \`if e1 then e2 else e3\`: an if-then-else expression
- \`ref e\`: boxes \`e\` and returns a reference to it
- \`!r\`: unboxes the value at reference \`r\`
- \`r := e\`: places the value of \`e\` in the reference \`r\`

All type annotations of form \`: t\` are optional. If not specified, the
type defaults to the unknown type \`?\`. There is also the special type marker
\`_\`, which instructs the compiler to infer a principal type.

**Types**

- \`?\`: the unknown type, admitting any value
- \`_\`: mark type to be inferred
- \`nat\`: the type of natural numbers
- \`bool\`: the type of booleans
- \`t1 -> t2\`: function type
- \`ref t\`: a reference type

**Builtin Functions**

${builtin_docs
        .map(({name, ty, doc}) => {
          return `- \`${name}: ${ty}\`\n\t- ${doc}`;
        })
        .join("\n")}

[Full Parser Specification](https://github.com/ayazhafiz/plts/blob/base/gtlc/parser.mly)
`.trim()}
  </ReactMarkdown>
);

const gtlcSyntax: monaco.languages.IMonarchLanguage = {
  defaultToken: "invalid",

  builtin_types: ["nat", "bool", "?", "_"],
  keywords: ["let", "in", "ref", "if", "then", "else", "\u03BB", "\\"].concat(
    builtin_fns
  ),
  symbols: /[_<>\\?\->.:=!;\u03BB]|(->)/,

  tokenizer: {
    root: [
      [/(Syntax error.*)/, "error"],
      [/(Parse error.*)/, "error"],
      [/`\(/, "infer", "@infer"],
      [/`(nat|bool|\?)/, "infer"],
      [/#[tf]/, "keyword"],
      [
        /[a-z][a-zA-Z0-9_'\w$]*/,
        {
          cases: {
            "@builtin_types": "keyword.type",
            "@keywords": "keyword",
            "@default": "identifier",
          },
        },
      ],
      [/\d+/, "number"],
      {include: "@whitespace"},
      [/[()]/, "@brackets"],
      [
        /@symbols/,
        {
          cases: {
            "@builtin_types": "keyword.type",
            "@keywords": "keyword",
            "@default": "operator",
          },
        },
      ],
    ],
    infer: [
      [/[^()]+/, "infer"],
      [/\(/, "infer", "@push"],
      [/\)/, "infer", "@pop"],
    ],
    whitespace: [
      [/(\uFF5C.*$)/, "annotation"],
      [/[ \t\r\n]+/, "white"],
      [/(--.*$)/, "comment"],
    ],
  },
};

const liftIrSyntax: monaco.languages.IMonarchLanguage = {
  defaultToken: "invalid",

  builtin_types: ["nat", "bool", "?", "Clos"],
  keywords: [
    "fn",
    "decl",
    "apply",
    "pack",
    "let",
    "in",
    "if",
    "then",
    "else",
    "\u03BB",
    "\\",
  ].concat(builtin_fns),
  symbols: /[,;<>\\?\->.:=\u03BB]+/,

  tokenizer: {
    root: [
      [/(Syntax error.*)/, "error"],
      [/(Parse error.*)/, "error"],
      [
        /[a-zA-Z_'][a-zA-Z_'\w$]*/,
        {
          cases: {
            "@builtin_types": "keyword.type",
            "@keywords": "keyword",
            "@default": "identifier",
          },
        },
      ],
      [/\d+/, "number"],
      {include: "@whitespace"},
      [/[{()}]/, "@brackets"],
      [
        /@symbols/,
        {
          cases: {
            "@builtin_types": "keyword.type",
            "@keywords": "keyword",
            "@default": "operator",
          },
        },
      ],
    ],
    whitespace: [
      [/[ \t\r\n]+/, "white"],
      [/(--.*$)/, "comment"],
    ],
  },
};

const languages: Record<"gtlc" | "liftIr", LanguageRegistration> = {
  gtlc: {
    syntax: gtlcSyntax,
    hover:
      (m: typeof monaco) =>
        (model: monaco.editor.ITextModel, pos: monaco.Position) => {
          const program = model.getValue();
          const hover = getHover(program, pos.lineNumber, pos.column);
          if (hover === null) return null;
          const {
            info,
            range: {startPos, endPos},
          } = hover;
          return {
            range: new m.Range(
              startPos.line,
              startPos.col,
              endPos.line,
              endPos.col
            ),
            contents: info.map((value) => {
              return {value};
            }),
          };
        },
  },
  liftIr: {
    syntax: liftIrSyntax,
  },
};

const GtlcPlayground = () => (
  <Playground
    title="GTLC Playground"
    language="gtlc"
    source="https://github.com/ayazhafiz/plts/tree/base/gtlc"
    grammar={grammar}
    languageRegistrations={languages}
    backends={backends}
    defaultBackend="C"
    examples={examples}
    defaultExample="Fixpoint Combinator"
  />
);
export default GtlcPlayground;
