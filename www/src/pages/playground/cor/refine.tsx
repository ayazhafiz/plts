import type * as monaco from "monaco-editor";
import * as React from "react";
import { LanguageRegistration } from "../../../common/types";
import CorPlayground from "../../../components/cor";

const refine = "refine";

const refineSyntax: monaco.languages.IMonarchLanguage = {
  defaultToken: "invalid",

  keywords: ["let", "in", "when", "is", "as"],
  symbols: /[,_\{\}\|<>\\?\->.=!;\[\]+]|(->)/,
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
      { include: "@whitespace" },
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
    ],
    whitespace: [
      [/[ \t\r\n]+/, "white"],
      [/#\s*[\^]+$/, "comment"],
      [/#\s*([\^]+\s*)+$/, "comment"],
      [/#\s*[\^]+/, "comment", "@type"],
      [/#.*$/, "comment"],
    ],
    type: [
      [/\]$/, "keyword.type", "@popall"],
      [/\]/, "keyword.type", "@popall"],
      [/\[/, "keyword.type"],
      [/,/, "keyword.type"],
      [/([`?]\d+)$/, "tag", "@popall"],
      [/([`?]\d+)/, "tag"],
      [/[a-zA-Z][a-zA-Z0-9_']*$/, "type", "@popall"],
      [/[a-zA-Z][a-zA-Z0-9_']*/, "type"],
      [/[ \t]*$/, "@whitespace", "@popall"],
      [/[ \t]+/, "@whitespace"],
    ],
  },
};

const languageRegistrations: Record<typeof refine, LanguageRegistration> = {
  [refine]: {
    syntax: refineSyntax,
  },
};

const RefinePlayground: React.FC<{}> = ({}) =>
  CorPlayground({
    experiment: refine,
    defaultPhase: "solve",
    defaultEmit: "elab",
    languageRegistrations,
  });

export default RefinePlayground;
