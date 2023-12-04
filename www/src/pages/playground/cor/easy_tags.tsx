import type * as monaco from "monaco-editor";
import * as React from "react";
import {LanguageRegistration} from "../../../common/types";
import CorPlayground from "../../../components/cor";

const easyTags = "easy_tags";

const TYPE_SYNTAXES: [RegExp, string, string][] = [
  [/- .*$/, "tag", "@popall"], // red
  [/\+ .*$/, "type.identifier", "@popall"], // green
  [/\. .*$/, "annotation", "@popall"], // grey
];

const easyTagsSyntax: monaco.languages.IMonarchLanguage = {
  defaultToken: "invalid",

  keywords: ["let", "in", "when", "is", "as"],
  symbols: /[,_{}|<>\\?\->.=!;[\]+]|(->)/,
  lower: /[a-z][a-zA-Z0-9_'\w$]*/,

  tokenizer: {
    root: [
      [/(.*error.*)/, "error"],
      [
        /@lower/,
        {
          cases: {
            "@keywords": "keyword",
            "@default": "identifier",
          },
        },
      ],
      [/[A-Z][a-zA-Z0-9_'\w$]*/, "constructor"],
      {include: "@whitespace"},
      [/[()]/, "@brackets"],
      [/`\d+/, "tag"],
      [/~\d+/, "tag"],
      [
        /@symbols/,
        {
          cases: {
            "@keywords": "keyword",
            "@default": "operator",
          },
        },
      ],
      [/:\s*/, "operator", "@type"],
      ...TYPE_SYNTAXES,
    ],
    whitespace: [
      [/[ \t\r\n]+/, "white"],
      [/#\s*[\^]+$/, "comment"],
      [/#\s*([\^]+\s*)+$/, "comment"],
      [/#\s*[\^]+/, "comment", "@type"],
      [/#.*$/, "comment"],
      ...TYPE_SYNTAXES,
    ],
    type: [...TYPE_SYNTAXES],
  },
};

const languageRegistrations: Record<typeof easyTags, LanguageRegistration> = {
  [easyTags]: {
    syntax: easyTagsSyntax,
  },
};

const RefinePlayground: React.FC = () =>
  CorPlayground({
    experiment: easyTags,
    defaultPhase: "solve",
    defaultEmit: "elab",
    languageRegistrations,
  });

export default RefinePlayground;
