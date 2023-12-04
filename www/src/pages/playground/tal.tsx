import type * as monaco from "monaco-editor";
import * as React from "react";
import Playground from "../../components/playground";
import type {Backend} from "../../common/types";
import {promisify} from "../../common/util";
import {ASM_SYNTAX} from "../../common/syntax/asm";
import * as tal from "tal";
import ReactMarkdown from "react-markdown";

const examples = {
  Fibonacci: `
let $fib =
  (fix fib (n: int): int.
    if0 n then 0
    else if0 (n - 1) then 1   # n = 1
    else fib (n - 1) + fib (n - 2)) in
let $twice =
  (Λa. fix inf(f: a->a): a->a.
       fix inx(x: a): a.
         f (f x)) in
let $double =
  (fix double(n: int): int.
    ($twice<int>) (fix inx(x: int): int. x + n) 0) in
$double ($fib 8)
`.trim(),
};

const backends: {
  [K in "TAL" | "x86"]: [Backend, Backend];
} = {
  TAL: [
    {
      title: "TAL",
      do: promisify(tal.talCompile),
      options: [],
      editorLanguage: "tal",
    },
    {
      title: "TAL Execution",
      do: promisify(tal.talEval),
      options: [],
      editorLanguage: "tal",
    },
  ],
  x86: [
    {
      title: "x86",
      do: promisify(tal.x86Compile),
      options: [],
      info: [
        [
          "Codegen info",
          <ReactMarkdown key={1}>
            Register allocation is based on the [Linear Scan Register
            Allocation](http://web.cs.ucla.edu/~palsberg/course/cs132/linearscan.pdf)
            of Poletto and Sarkar (1999). We number instructions as they appear
            in the TAL IR, not in the order they would execute. This poor
            ordering is simple to implement but results in many spills, as can
            be seen in the generated code. The large number of spills demands
            multiple passes of register allocation, slowing down compilation
            time. Spilled variables live at the very top of the stack, with
            space allocated for them before any code is executed.
          </ReactMarkdown>,
        ],
      ],
      editorLanguage: "asm",
    },
    {
      title: "x86 Emulation",
      do: promisify(tal.x86Emulate),
      options: [],
      editorLanguage: "tal",
    },
  ],
};

const talSyntax: monaco.languages.IMonarchLanguage = {
  defaultToken: "invalid",

  registers: /%?\b(r[0-9a-z]+)\b/,

  keywords: ["pack", "code", "halt", "as"],

  tokenizer: {
    root: [
      // Error document
      [/^<.*>$/, {token: "annotation"}],
      // Label definition
      [/^[.a-zA-Z0-9_$?@].*:/, {token: "type.identifier"}],
      // Constant definition
      [/^[.a-zA-Z0-9_$?@][^=]*=/, {token: "type.identifier"}],
      // opcode
      [/[.a-zA-Z_][.a-zA-Z_0-9']*/, {token: "keyword", next: "@rest"}],
      [/[(){}[\]\-<>]/, {token: "operator", next: "@rest"}],
      [/\d+/, "number"],

      // whitespace
      {include: "@whitespace"},
    ],

    rest: [
      // pop at the beginning of the next line and rematch
      [/^.*$/, {token: "@rematch", next: "@pop"}],

      [/@registers/, "variable.predefined"],

      [/\d+/, "number"],

      [/[,():,[\]\-<>∃∀+]/, "operator"],
      // Assume anything else is a label reference
      [
        /%?[.?_$a-zA-Z@][.?_$a-zA-Z0-9'@]*/,
        {cases: {"@keywords": "keyword", "@default": "type.identifier"}},
      ],

      // whitespace
      {include: "@whitespace"},
    ],

    comment: [
      [/[^/*]+/, "comment"],
      [/\/\*/, "comment", "@push"], // nested comment
      ["\\*/", "comment", "@pop"],
      [/[/*]/, "comment"],
    ],

    whitespace: [
      [/[ \t\r\n]+/, "white"],
      [/\/\*/, "comment", "@comment"],
      [/\/\/.*$/, "comment"],
      [/[#;\\@].*$/, "comment"],
    ],
  },
};

const systemFSyntax: monaco.languages.IMonarchLanguage = {
  defaultToken: "invalid",
  keywords: ["fix", "if0", "then", "else", "Λ", "let", "in"],

  typeKeywords: ["int", "∀"],

  symbols: /[=><!~?:&|+\-*/^%.,]+/,
  tokenizer: {
    root: [
      [
        /[a-z_A-Z][\w0-9$]*|[∀Λ]/,
        {
          cases: {
            "@typeKeywords": "keyword",
            "@keywords": "keyword",
            "@default": "identifier",
          },
        },
      ],
      {include: "@whitespace"},
      [/[{}()[\]]/, "@brackets"],

      [/[%$][a-z_A-Z0-9]*/, "annotation"],
      [/@symbols/, "operator"],
      // numbers
      [/\d+/, "number"],
    ],
    whitespace: [
      [/[ \t\r\n]+/, "white"],
      [/(^#.*$)/, "comment"],
    ],
  },
};

const languages = {
  systemF: {
    syntax: systemFSyntax,
  },
  tal: {
    syntax: talSyntax,
  },
  asm: {
    syntax: ASM_SYNTAX,
  },
};

const FtPlayground = () => (
  <Playground
    title="TAL Playground"
    language="systemF"
    source="https://github.com/ayazhafiz/plts/tree/base/TAL"
    grammar="https://github.com/ayazhafiz/plts/blob/base/TAL/parser.mly"
    languageRegistrations={languages}
    backends={backends}
    defaultBackend="TAL"
    examples={examples}
    defaultExample="Fibonacci"
  />
);
export default FtPlayground;
