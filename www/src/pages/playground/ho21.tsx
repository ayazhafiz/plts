import type * as monaco from "monaco-editor";
import * as React from "react";
import Playground from "../../components/playground";
import { createHoverProvider } from "../../common/hover";
import type { Backend, LanguageRegistration } from "../../common/types";
import { promisify } from "../../common/util";
import ReactMarkdown from "react-markdown";
import * as ho21 from "ho21";

const examples = {
  Queries: `
⊥ ?? ⊤
P∧Q ?? P∨Q
P∧(Q∨R) ?? (P∧Q)∨(P∧R)∨(P∧Q∧R)
(A1→A2)∧(B1→B2) ?? A1∧B1→A2∧B2
(A1→B)∧(A2→B) ?? A1∨A2→B
(A∨B)∧Int ?? A∨(Int∧B)
Int ?? Double
`.trim(),
};

const backends: {
  [K in "Check"]: [Backend];
} = {
  Check: [
    {
      title: "Judgements",
      do: promisify(ho21.judge),
      options: [
        ["Print Derivations", true],
        ["Prettify Symbols", true],
      ],
      editorLanguage: "ho21",
    },
  ],
};

const ho21Syntax: monaco.languages.IMonarchLanguage = {
  defaultToken: "invalid",

  constant_types: ["!", "*", "\u22A4", "\u22A5"],
  queries: ["??"],
  judgements: ["<:", ":>", "~=", "#", "\u227A", "\u227B", "\u2245"],
  operators: ["&", "|", "->", "\u2227", "\u2228", "\u2192"],
  symbols: /[=><!~?:&|+\-*\/\^%#\u22A4\u22A5\u2227\u2228\u2192\u227A\u227B\u2245]+/,

  tokenizer: {
    root: [
      [/(Syntax error.*)/, "error"],
      [/(Parse error.*)/, "error"],
      [/[A-Z][a-zA-Z0-9_'\w$]*/, "type.identifier"],
      { include: "@whitespace" },
      [/[()]/, "@brackets"],
      [
        /@symbols/,
        {
          cases: {
            "@constant_types": "keyword",
            "@queries": "annotation",
            "@judgements": "keyword",
            "@operators": "operator",
          },
        },
      ],
    ],
    whitespace: [
      [/(\uFF5C.*$)/, "annotation"],
      [/[ \t\r\n]+/, "white"],
      [/(--.*$)/, "comment"],
      [/(\\.*$)/, "annotation"],
    ],
  },
};

const languageContent = [
  { aliases: ["!", "⊥"], kind: "⊥", text: "The bottom type", binary: false },
  { aliases: ["*", "⊤"], kind: "⊤", text: "The top type", binary: false },
  {
    aliases: ["&", "∧"],
    kind: "Operator",
    text: "An intersection type",
    binary: true,
  },
  { aliases: ["|", "∨"], kind: "Operator", text: "A union type", binary: true },
  {
    aliases: ["->", "→"],
    kind: "Operator",
    text: "An arrow (function) type",
    binary: true,
  },
  {
    aliases: ["??"],
    kind: "Query",
    text: "A judgement query for the relationship between A and B, which will be answered in the output editor.",
    binary: true,
  },
  {
    aliases: ["<:", "≺"],
    kind: "Judgement",
    text: "`S <: T` means `S` is a subtype of `T`",
    outputOnly: true,
  },
  {
    aliases: [":>", "≻"],
    kind: "Judgement",
    text: "`T :> S` means `T` is a supertype of `S`",
    outputOnly: true,
  },
  {
    aliases: ["~=", "≅"],
    kind: "Judgement",
    text: "`T ~= S` means `T` is isomorphic to `S`",
    outputOnly: true,
  },
  {
    aliases: ["#"],
    kind: "Judgement",
    text: "`T # U` means `T` and `U` are incomparable",
    outputOnly: true,
  },
];

function getLanguageGrammarInfo() {
  const inputItems = languageContent.filter((lc) => !lc.outputOnly);
  const itemDocs = inputItems.map(({ aliases, text, binary }) => {
    const examples = aliases
      .map((alias) => {
        const example = binary ? `A${alias}B` : alias;
        return `\`${example}\``;
      })
      .join("/");
    return `- ${examples}: ${text}`;
  });
  itemDocs.push(
    `- Other types: Identifiers beginning with an uppercase letter are treated as primitive types.j`
  );
  const listDocs = itemDocs.join("\n");
  return `${listDocs}\n[Full Parser Specification](https://github.com/ayazhafiz/plts/blob/base/huang_oliveira_21/parser.mly)`;
}

const grammar = <ReactMarkdown children={getLanguageGrammarInfo()} />;

function ho21GetHoverContent(word: string) {
  for (const { aliases, text } of languageContent) {
    for (const alias of aliases) {
      if (word === alias) {
        const content = [
          // {value: `**${kind}**`},
          { value: text },
        ];
        if (aliases.length > 1) {
          content.push({ value: `*Aliases*: ${aliases.join(", ")}` });
        }
        return content;
      }
    }
  }
  if (word[0] === word[0].toUpperCase() && word[0] !== word[0].toLowerCase()) {
    return [{ value: "**Primitive**" }, { value: `Type primitive \`${word}\`` }];
  }
  return null;
}

function ho21FormatQueries(
  model: monaco.editor.IModel
): monaco.languages.ProviderResult<monaco.languages.TextEdit[]> {
  const text = ho21.formatQueries(model.getValue(), true);
  return [
    {
      text,
      range: model.getFullModelRange(),
    },
  ];
}

function ho21OnTypeFormatQueries(
  model: monaco.editor.IModel
): monaco.languages.ProviderResult<monaco.languages.TextEdit[]> {
  const text = ho21.formatQueries(model.getValue(), true);
  return [
    {
      text,
      range: model.getFullModelRange(),
    },
  ];
}

function getHo21AutoFormatCharacters() {
  const all = [];
  all.push(..."abcdefghijklmnopqrstuvwxyz".split(""));
  all.push(..."ABCDEFGHIJKLMNOPQRSTUVWXYZ".split(""));
  all.push(..."0123456789".split(""));
  all.push(..."()_'".split(""));
  all.push(...ho21Syntax["constant_types"].join("").split(""));
  all.push(...ho21Syntax["queries"].join("").split(""));
  all.push(...ho21Syntax["judgements"].join("").split(""));
  all.push(...ho21Syntax["operators"].join("").split(""));
  return all;
}

const languages: Record<string, LanguageRegistration> = {
  ho21: {
    syntax: ho21Syntax,
    hover: createHoverProvider("ho21", ho21GetHoverContent),
    format: ho21FormatQueries,
    autoFormat: {
      format: ho21OnTypeFormatQueries,
      triggerCharacters: getHo21AutoFormatCharacters(),
    },
  },
};

const FtPlayground = () => (
  <Playground
    title="Huang-Oliveira (2021) Playground"
    language="ho21"
    source="https://github.com/ayazhafiz/plts/tree/base/ho21"
    grammar={grammar}
    languageRegistrations={languages}
    backends={backends}
    defaultBackend="Check"
    examples={examples}
    defaultExample="Queries"
  />
);
export default FtPlayground;
