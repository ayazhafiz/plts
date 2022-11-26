import type * as monaco from "monaco-editor";
import * as React from "react";
import { BackendOverrides, LanguageRegistration } from "../../../common/types";
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

const irSyntax: monaco.languages.IMonarchLanguage = {
  defaultToken: "invalid",

  keywords: ["let", "in", "when", "is", "as", "switch", "feed"],
  symbols: /[,\{\}=;:]/,
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
      [/%\d+/, "identifier"],
      [/\d+/, "number"],
      [/@[a-z_]*/, "keyword"],
      [/ :\s*/, "operator", "@type"],
      { include: "@whitespace" },
      [
        /@symbols/,
        {
          cases: {
            "@keywords": "keyword",
            "@default": "operator",
          },
        },
      ],
    ],
    whitespace: [
      [/[ \t\r\n]+/, "white"],
    ],
    type: [
      [/\]$/, "keyword.type", "@popall"],
      [/\]/, "keyword.type", "@pop"],
      [/\[/, "keyword.type", "@push"],
      [/\}$/, "keyword.type", "@popall"],
      [/\}/, "keyword.type", "@pop"],
      [/\{/, "keyword.type", "@push"],
      [/,/, "keyword.type"],
      [/`\d+$/, "tag", "@pop"],
      [/`\d+/, "tag"],
      [/int/, "keyword.type"],
      [/void/, "keyword.type"],
      [/\s*$/, "@whitespace", "@pop"],
      [/\s+/, "@whitespace"],
      [/=/, "default", "@pop"],
    ],
  },
};

const languageRegistrations: Record<typeof refine|"ir", LanguageRegistration> = {
  [refine]: {
    syntax: refineSyntax,
  },
  ir: {
    syntax: irSyntax,
  }
};

const backendOverrides: Record<string, BackendOverrides> = {
  ir: {
    editorLanguage: "ir",
  },
};

const RefinePlayground: React.FC<{}> = ({}) =>
  CorPlayground({
    experiment: refine,
    defaultPhase: "solve",
    defaultEmit: "elab",
    languageRegistrations,
    backendOverrides,
  });

export default RefinePlayground;
