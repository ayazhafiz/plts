import type * as monaco from "monaco-editor";
import * as React from "react";
import Playground from "../../components/playground";
import type { Backend } from "../../common/types";
import { promisify } from "../../common/util";
import * as tiger from "tiger";
import { ASM_SYNTAX } from "../../common/syntax/asm";

const examples = {
  "Eight Queens": `
let
  var N := 8

  type intArray = array of int

  var row := intArray [ N ] of 0
  var col := intArray [ N ] of 0
  var diag1 := intArray [N+N-1] of 0
  var diag2 := intArray [N+N-1] of 0

  function printboard() =
    (for i := 0 to N-1 do
     (for j := 0 to N-1 do
        print(if col[i]=j then " O" else " .");
        print("\\n"));
      print("\\n"))

  function try(c:int) = (
    if c=N
    then printboard()
    else for r := 0 to N-1 do
      if row[r]=0 & diag1[r+c]=0 & diag2[r+7-c]=0
          then (
            row[r]:=1;
            diag1[r+c]:=1;
            diag2[r+7-c]:=1;
            col[c]:=r;
            try(c+1);
            row[r]:=0;
            diag1[r+c]:=0;
            diag2[r+7-c]:=0 ) )
in
  try(0);
  0
end
`.trim(),
};

const backends: {
  [K in "Compiler IR" | "X86 Assembly"]: [Backend];
} = {
  "Compiler IR": [
    {
      title: "Compiler IR",
      do: promisify(tiger.compileIR),
      options: [],
      editorLanguage: "tiger",
    },
  ],
  "X86 Assembly": [
    {
      title: "X86 Assembly",
      do: promisify(tiger.compileX86),
      options: [],
      editorLanguage: "asm",
    },
  ],
};

const tigerSyntax: monaco.languages.IMonarchLanguage = {
  defaultToken: "invalid",

  builtin_types: ["nat", "bool", "?", "_"],
  keywords: [
    "if",
    "then",
    "else",
    "for",
    "to",
    "while",
    "do",
    "type",
    "function",
    "var",
    "break",
    "let",
    "in",
    "end",
    "array",
    "of",
    "nil",
    "int",
    "string",
    "print",
    "flush",
    "get_char",
    "ord",
    "chr",
    "size",
    "substring",
    "concat",
    "not",
    "exit",
  ],
  operators: [":=", "+", "-", "*", "/", "|", "&"],
  symbols: /[=><!~?:&|+\-*\/\^%]+/,
  escapes: /\\(?:[abfnrtv\\"']|x[0-9A-Fa-f]{1,4}|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})/,

  tokenizer: {
    root: [
      [/(Syntax error.*)/, "error"],
      [/(Parse error.*)/, "error"],
      [
        /[a-zA-Z_][a-zA-Z_0-9\w$]*/,
        {
          cases: {
            "@builtin_types": "keyword.type",
            "@keywords": "keyword",
            "@default": "identifier",
          },
        },
      ],

      // identifiers and keywords
      [
        /[a-z_$][\w$]*/,
        {
          cases: { "@builtin_types": "keyword", "@keywords": "keyword", "@default": "identifier" },
        },
      ],

      // whitespace
      { include: "@whitespace" },

      // delimiters and operators
      [/[{}()\[\]]/, "@brackets"],
      [/[<>](?!@symbols)/, "@brackets"],
      [/@symbols/, { cases: { "@operators": "operator", "@default": "" } }],

      // numbers
      [/\d*\.\d+([eE][\-+]?\d+)?/, "number.float"],
      [/0[xX][0-9a-fA-F]+/, "number.hex"],
      [/\d+/, "number"],

      // delimiter: after number because of .\d floats
      [/[;,.]/, "delimiter"],

      // strings
      [/"([^"\\]|\\.)*$/, "string.invalid"], // non-teminated string
      [/"/, { token: "string.quote", bracket: "@open", next: "@string" }],

      // characters
      [/'[^\\']'/, "string"],
      [/(')(@escapes)(')/, ["string", "string.escape", "string"]],
      [/'/, "string.invalid"],
    ],
    comment: [
      [/[^\/*]+/, "comment"],
      [/\/\*/, "comment", "@push"], // nested comment
      ["\\*/", "comment", "@pop"],
      [/[\/*]/, "comment"],
    ],
    string: [
      [/[^\\"]+/, "string"],
      [/@escapes/, "string.escape"],
      [/\\./, "string.escape.invalid"],
      [/"/, { token: "string.quote", bracket: "@close", next: "@pop" }],
    ],
    whitespace: [
      [/[ \t\r\n]+/, "white"],
      [/\/\*/, "comment", "@comment"],
      [/\/\/.*$/, "comment"],
    ],
  },
};

const languages = {
  tiger: {
    syntax: tigerSyntax,
  },
  asm: {
    syntax: ASM_SYNTAX,
  },
};

const FtPlayground = () => (
  <Playground
    title="Tiger Playground"
    language="tiger"
    source="https://github.com/ayazhafiz/tiger"
    grammar="https://github.com/ayazhafiz/tiger/blob/base/lib/front/parser.mly"
    languageRegistrations={languages}
    backends={backends}
    defaultBackend="X86 Assembly"
    examples={examples}
    defaultExample="Eight Queens"
  />
);
export default FtPlayground;
