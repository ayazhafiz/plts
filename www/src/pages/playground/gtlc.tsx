import type * as monaco from "monaco-editor";
import * as React from "react";
import ReactMarkdown from "react-markdown";
import Playground from "../../components/playground";
import type { Backend, LanguageRegistration } from "../../common/types";
import { C, TS } from "../../common/evaluator";
import { promisify, uncurry } from "../../common/util";
import { createHoverProvider } from "../../common/hover";
import * as gtlc from "gtlc";

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
let fib = λn.
  let go' = λgo. λi. λa. λb.
    if eqn i n
    then a
    else go (succ i) b (add a b)
  in
  let go = fix go' in
  go 0 0 1
in
fib 23
`.trim(),
};

const compileOptions: [string, boolean][] = [["optimize", true]];

const backends: {
  [K in "Compiler IR" | "TypeScript" | "C"]: [Backend, Backend];
} = {
  "Compiler IR": [
    {
      title: "Lift IR",
      do: promisify(uncurry(gtlc.irCompile)),
      options: compileOptions,
      editorLanguage: "liftIr",
    },
    {
      title: "Interpreter Execution",
      do: promisify(gtlc.doEval),
      options: [],
      editorLanguage: "gtlc",
    },
  ],
  TypeScript: [
    {
      title: "TypeScript",
      do: promisify(uncurry(gtlc.tsCompile)),
      options: compileOptions,
      editorLanguage: "typescript",
    },
    {
      title: TS.title,
      do(input: string, optimize: boolean) {
        const ts = gtlc.tsCompile(input)(optimize);
        return TS.eval(ts);
      },
      options: [],
      editorLanguage: "javascript",
    },
  ],
  C: [
    {
      title: "C",
      do: promisify(uncurry(gtlc.cCompile)),
      options: compileOptions,
      editorLanguage: "c",
    },
    {
      title: C.title,
      do(input: string, optimize: boolean) {
        const c = gtlc.cCompile(input)(optimize);
        return C.eval(c);
      },
      options: [],
      editorLanguage: "c",
    },
  ],
};

const builtin_docs = gtlc.docs;
const builtin_fns = builtin_docs.map((d) => d.name);

const grammar = (
  <ReactMarkdown
    children={`
**Expressions**

- \`x\`: variables
- \`100\`: numbers
- \`#t|#f\`: the true and false literals
- \`(\\|λ)x: t. e\`: a lambda expression with \`x\` bound to type \`t\`
- \`let x: t = e1 in e2\`: binds \`x\` to \`e1\` and then evaluates \`e2\`
- \`f a\`: application to a lambda
- \`if e1 then e2 else e3\`: an if-then-else expression

All type annotations of form \`: t\` are optional. If not specified, the
type defaults to the unknown type \`?\`.

**Types**

- \`?\`: the unknown type, admitting any value
- \`nat\`: the type of natural numbers
- \`bool\`: the type of booleans
- \`t1 -> t2\`: function type

**Builtin Functions**

${builtin_docs
  .map(({ name, ty, doc }) => {
    return `- \`${name}: ${ty}\`\n\t- ${doc}`;
  })
  .join("\n")}

[Full Parser Specification](https://github.com/ayazhafiz/plts/blob/base/gtlc/parser.mly)
`.trim()}
  />
);

const gtlcSyntax: monaco.languages.IMonarchLanguage = {
  defaultToken: "invalid",

  builtin_types: ["nat", "bool", "?"],
  keywords: ["let", "in", "if", "then", "else", "\u03BB", "\\"].concat(builtin_fns),
  symbols: /[<>\\?\->.:=\u03BB]|(->)/,

  tokenizer: {
    root: [
      [/(Syntax error.*)/, "error"],
      [/(Parse error.*)/, "error"],
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
      { include: "@whitespace" },
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
    whitespace: [
      [/(\uFF5C.*$)/, "annotation"],
      [/[ \t\r\n]+/, "white"],
      [/(--.*$)/, "comment"],
    ],
  },
};

function gtlcGetHoverContent(word: string) {
  if (word === "?") {
    return [{ value: "The unknown type" }];
  }
  for (const { name, ty, doc } of builtin_docs) {
    if (word === name) {
      return [{ value: `\`\`\`gtlc\n${name}: ${ty}\n\`\`\`` }, { value: doc }];
    }
  }
  return null;
}

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
      { include: "@whitespace" },
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
    hover: createHoverProvider("gtlc", gtlcGetHoverContent),
  },
  liftIr: {
    syntax: liftIrSyntax,
  },
};

const GtlcPlayground = () => (
  <Playground
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
