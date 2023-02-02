import type * as monaco from "monaco-editor";
import * as React from "react";
import ReactMarkdown from "react-markdown";
import Playground from "../../components/playground";
import type { Backend, LanguageRegistration } from "../../common/types";
import { shapeBackend } from "../../common/util";
import { compile, phases, emits } from "co_lc";

const examples = {
  "Fibonacci": `
let rec fib = \\n ->
  yield;
  if n < 2
  then n
  else (fib (n - 1)) + (fib (n - 2))
in

let rec exec = \\state ->
  stat state.0
  | \`Pending ->
    let fib1 = resume state.0 in
    exec {fib1, 0, state.2 + 1}
  | \`Done n -> {state.0, n, state.2}

in
let runFib = spawn (fib 28) in
let result = exec {runFib, 0, 0} in
result
`.trim(),
};

const fmtOptions: [["width", number]] = [["width", 55]];

const backends: {
  Infer: [Backend];
}  = {
  Infer: [
    {
      title: "Infer",
      editorLanguage: "gtlc",
      ...shapeBackend(infer, fmtOptions),
    },
  ],
};

const builtin_docs = docs;
const builtin_fns = builtin_docs.map((d) => d.name);

const grammar = (
  <ReactMarkdown
    children={`
**TODO**
`.trim()}
  />
);

const gtlcSyntax: monaco.languages.IMonarchLanguage = {
  defaultToken: "invalid",

  builtin_types: ["nat", "bool", "?", "_"],
  keywords: ["let", "in", "ref", "if", "then", "else", "\u03BB", "\\"].concat(builtin_fns),
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

const languages: Record<"co_lc", LanguageRegistration> = {
  co_lc: {
    syntax: coLcSyntax,
    hover: (m: typeof monaco) => (model: monaco.editor.ITextModel, pos: monaco.Position) => {
      const program = model.getValue();
      const hover = getHover(program, pos.lineNumber, pos.column);
      if (hover === null) return null;
      const {
        info,
        range: { startPos, endPos },
      } = hover;
      return {
        range: new m.Range(startPos.line, startPos.col, endPos.line, endPos.col),
        contents: info.map((value) => {
          return { value };
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
    title="co_lc Playground"
    language="co_lc"
    source="https://github.com/ayazhafiz/plts/tree/base/co_lc"
    grammar={grammar}
    languageRegistrations={languages}
    backends={backends}
    defaultBackend="Infer"
    examples={examples}
    defaultExample="Fibonacci"
  />
);
export default GtlcPlayground;
