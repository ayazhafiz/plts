import type * as monaco from "monaco-editor";
import * as React from "react";
import {LanguageRegistration} from "../../../common/types";
import CorPlayground from "../../../components/cor";

const compose_fx = "compose_fx";

const composeFxSyntax: monaco.languages.IMonarchLanguage = {
  defaultToken: "invalid",

  keywords: ["sig", "let", "run", "in", "\\"],
  symbols: /[_{}|<>\\?\->.:=!;[\]+]|(->)/,
  lower: /[a-z][a-zA-Z0-9_'\w$]*/,

  tokenizer: {
    root: [
      [/(.*error.*)/, "error"],
      [/proto\s*/, "keyword", "@proto"],
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
    ],
    proto: [[/@lower/, "identifier", "@protoArg"]],
    protoArg: [
      [/@lower/, "type", "@protoArg"],
      [/:/, "operator", "@type"],
    ],
    whitespace: [
      [/[ \t\r\n]+/, "white"],
      [/#\s+[\^]+$/, "comment"],
      [/#\s+[\^]+/, "comment", "@type"],
      [/#.*$/, "comment"],
    ],
    type: [
      [/\(\)$/, "keyword.type", "@popall"],
      [/\(\)/, "keyword.type"],
      [/->|[+,]/, "operator"],
      [/-\[/, "type"],
      [/\]->$/, "type", "@popall"],
      [/\]->/, "type"],
      [/([`?]\d+)$/, "tag", "@popall"],
      [/([`?]\d+)/, "tag"],
      [
        /(~\d+)(:)([a-z]+|\?\d+)(:)([a-z][a-zA-Z0-9]*)/,
        ["tag", "operator", "type", "operator", "identifier"],
      ],
      [/[a-zA-Z][a-zA-Z0-9_']*$/, "type", "@popall"],
      [/[a-zA-Z][a-zA-Z0-9_']*/, "type"],
      [/[()[\]]$/, "@brackets", "@popall"],
      [/[()[\]]/, "@brackets"],
      [/[ \t]*$/, "@whitespace", "@popall"],
      [/[ \t]+/, "@whitespace"],
    ],
  },
};

const languageRegistrations: Record<typeof compose_fx, LanguageRegistration> = {
  [compose_fx]: {
    syntax: composeFxSyntax,
  },
};

const ComposeFxPlayground: React.FC = () =>
  CorPlayground({
    experiment: compose_fx,
    defaultPhase: "solve",
    defaultEmit: "elab",
    languageRegistrations,
  });

export default ComposeFxPlayground;
