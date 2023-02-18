import type * as monaco from "monaco-editor";
import * as React from "react";
import Playground from "../../components/playground";
import type { Backend, BackendKind, BackendOverrides, LanguageRegistration, StringOptions } from "../../common/types";
import { shapeBackend } from "../../common/util";
import * as co_lc from "co_lc";
import {graphql, useStaticQuery} from "gatsby";

function buildExamples() {
  const allExamples = useStaticQuery(graphql`
    {
      allFile(filter: { extension: { eq: "co" } }) {
        nodes {
          publicURL
          relativePath
        }
      }
    }
  `);

  const examples: Record<string, string> = {};
  for (const file of allExamples.allFile.nodes) {
    const exampleName = file.relativePath.split("/").at(-1).split(".co")[0];
    const [content, setContent] = React.useState("");
    const base = process.env["HOST"];
    fetch(new URL(file.publicURL, base))
      .then((r) => r.text())
      .then((s) => {
        return co_lc.userProgram(s);
      })
      .then(setContent)
      .catch(() => {
        console.log("failed to fetch", base, file.publicURL);
      });
    examples[exampleName] = content;
  }

  return examples;
}

function getBackends(
  defaultEmit: string,
  overrides: Record<string, BackendOverrides>,
): Record<string, BackendKind> {
  const backends: Record<string, BackendKind> = {};
  for (const phase of co_lc.phases) {
    let doit = (prog: string, emit: string) =>
      co_lc.compile(prog, phase, emit);
    let options: [[string, StringOptions]] = [
      ["emit", { value: defaultEmit, options: co_lc.emits }],
    ];

    let backend: Backend = {
      title: phase,
      editorLanguage: overrides[phase]?.editorLanguage ?? 'co_lc',
      ...shapeBackend(doit, options),
    };

    if (phase === 'ir') {
      const evalP = 'eval';
      let doEmit = (prog: string, emit: string) =>
        co_lc.compile(prog, evalP, emit);
      backends[phase] = [backend, {
        title: evalP,
        editorLanguage: overrides[phase]?.editorLanguage ?? 'co_lc',
        ...shapeBackend(doEmit, options),
      }];
    } else {
      backends[phase] = [backend];
    }
  }
  return backends;
}

const coLcSyntax: monaco.languages.IMonarchLanguage = {
  defaultToken: "invalid",

  keywords: ["let", "in", "yield", "spawn", "resume", "if", "then", "else", "stat", "\\"],
  symbols: /[*\+_\{\}\|<>,\\?\->.:=!;\[\]+]|(->)/,
  lower: /[a-z][a-zA-Z0-9_'\w$]*/,

  tokenizer: {
    root: [
      [/(.*error.*)/, "error"],
      [/\d+/, "number"],
      [
        /@lower/,
        {
          cases: {
            "@keywords": "keyword",
            "@default": "identifier",
          },
        },
      ],
      [/`Pending|`Done/, "keyword"],
      [/[A-Z][a-zA-Z0-9_'\w$]*/, "constructor"],
      { include: "@whitespace" },
      [/: \s*/, "operator", "@type"],
      [/[()]/, "@brackets"],
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
      [/#\s+[\^]+$/, "comment"],
      [/#\s+[\^]+/, "comment", "@type"],
      [/#.*$/, "comment"],
    ],
    type: [
      [/\]$/, "keyword.type", "@popall"],
      [/\]/, "keyword.type", "@pop"],
      [/\[/, "keyword.type", "@push"],
      [/\}$/, "keyword.type", "@popall"],
      [/\}/, "keyword.type", "@pop"],
      [/\{/, "keyword.type", "@push"],
      [/,/, "keyword.type"],
      [/;/, "keyword.type"],
      [/[A-z][a-zA-Z]*$/, "tag", "@pop"],
      [/[A-z][a-zA-Z]*\s/, "tag", "@pop"],
      [/int/, "keyword.type"],
      [/void/, "keyword.type"],
      [/\s*$/, "@whitespace", "@pop"],
      [/\s+/, "@whitespace"],
      [/=/, "default", "@pop"],
    ],
  },
};

const vmSyntax: monaco.languages.IMonarchLanguage = {
  defaultToken: "invalid",

  keywords: [
  ],
  symbols: /[,;<>\\?\->.:=\u03BB]+/,

  tokenizer: {
    root: [
      // Label definition
      [/^[.a-zA-Z0-9_$?@].*:/, {token: 'type.identifier'}],
      // instr
      [/[a-z][-_a-z0-9]*/, {token: 'keyword', next: '@rest'}],
      [/[<=]/, {token: 'keyword', next: '@rest'}],
      [/\d+/, "number"],
      { include: "@whitespace" },
      [/[{()}]/, "@brackets"],
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
    rest: [
        // pop at the beginning of the next line and rematch
        [/^.*$/, {token: '@rematch', next: '@pop'}],

        // brackets
        [/[{}<>()[\]]/, '@brackets'],

        // numbers
        [/-?\d+/, 'number'],

        // label reference
        [/[a-z][-_a-zA-Z]*/, 'type.identifier'],

        // whitespace
        {include: '@whitespace'},
    ],
    whitespace: [
      [/[ \t\r\n]+/, "white"],
      [/(%.*$)/, "comment"],
    ],
  },
};

const languages: Record<"co_lc" | "vm", LanguageRegistration> = {
  co_lc: {
    syntax: coLcSyntax,
    hover: (m: typeof monaco) => (model: monaco.editor.ITextModel, pos: monaco.Position) => {
      const program = model.getValue();
      const hover = co_lc.hover(program, pos.lineNumber, pos.column);
      if (hover === null) return null;
      const {
        info,
        range: { start, fin },
      } = hover;
      return {
        range: new m.Range(start.line, start.col, fin.line, fin.col),
        contents: info.map((value) => {
          return { value };
        }),
      };
    },
  },
  vm: {
    syntax: vmSyntax,
  },
};

const CoLcPlayground = () => { 
  const examples = buildExamples();

  const backends = getBackends('print', {
    'ir': {
      editorLanguage: 'vm'
    }
  });

  return (
    <Playground
    title="co_lc Playground"
    language="co_lc"
    source="https://github.com/ayazhafiz/plts/tree/base/co_lc"
      grammar={`https://github.com/ayazhafiz/plts/blob/base/co_lc/ast_parser.mly`}
      languageRegistrations={languages}
    backends={backends}
    defaultBackend="ir"
    examples={examples}
    defaultExample={"fib"}
    />
  )
}
export default CoLcPlayground;
