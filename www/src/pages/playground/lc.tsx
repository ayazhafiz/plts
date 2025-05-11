import type * as monaco from "monaco-editor";
import * as React from "react";
import Playground from "../../components/playground";
import type {
  Backend,
  BackendKind,
  BackendOverrides,
  LanguageRegistration,
  StringOptions,
} from "../../common/types";
import {shapeBackend} from "../../common/util";
import * as lc from "lc";
import {graphql, useStaticQuery} from "gatsby";

function buildExamples() {
  const allExamples = useStaticQuery(graphql`
    {
      allFile(filter: { extension: { eq: "lc" } }) {
        nodes {
          publicURL
          relativePath
        }
      }
    }
  `);

  const examples: Record<string, string> = {};
  for (const file of allExamples.allFile.nodes) {
    const exampleName = file.relativePath.split("/").at(-1).split(".lc")[0];
    const [content, setContent] = React.useState("");
    const base = process.env["HOST"];
    fetch(new URL(file.publicURL, base))
      .then((r) => r.text())
      .then((s) => {
        return lc.userProgram(s);
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
  for (const phase of lc.phases) {
    let doit = (prog: string, emit: string) => lc.compile(prog, phase, emit);
    let options: [[string, StringOptions]] = [
      ["emit", {value: defaultEmit, options: lc.emits}],
    ];

    let backend: Backend = {
      title: phase,
      editorLanguage: overrides[phase]?.editorLanguage ?? "lc",
      ...shapeBackend(doit, options),
    };

    if (phase === "ir") {
      const evalP = "eval";
      let doEmit = (prog: string, emit: string) => {
        const result = lc.compile(prog, evalP, emit);
        console.log({result});
        return result;
      };
      backends[phase] = [
        backend,
        {
          title: evalP,
          editorLanguage: overrides[phase]?.editorLanguage ?? "lc",
          ...shapeBackend(doEmit, options),
        },
      ];
    } else {
      backends[phase] = [backend];
    }
  }
  return backends;
}

const lcSyntax: monaco.languages.IMonarchLanguage = {
  defaultToken: "invalid",

  keywords: ["\\"],
  symbols: /[*\+_\{\}\|<>,\\?\->.:=!;\[\]+]|(->)/,
  lower: /[a-z][a-zA-Z0-9_']*/,

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
      [/\\/, "keyword"],
      [/->/, "keyword"],
      {include: "@whitespace"},
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
      [/\s*$/, "@whitespace", "@pop"],
      [/\s+/, "@whitespace"],
      [/->/, "keyword.type"],
      [/t\d+/, "type.identifier"],
      [/[()]/, "@brackets"],
    ],
  },
};

const vmSyntax: monaco.languages.IMonarchLanguage = {
  defaultToken: "invalid",

  keywords: [
    "PUSH_VAR",
    "PUSH_FUN",
    "APPLY",
    "RETURN",
    "ACCESS",
    "CLOSURE",
    "SLIDE",
    "GRAB",
    "HALT",
  ],
  symbols: /[,;:]/,

  tokenizer: {
    root: [
      // Line numbers (e.g., "0: PUSH_VAR 1")
      [/^\d+:/, "comment"],

      // VM Instructions
      [
        /PUSH_VAR|PUSH_FUN|APPLY|RETURN|ACCESS|CLOSURE|SLIDE|GRAB|HALT/,
        "keyword",
      ],

      // Numbers (for instruction arguments)
      [/\d+/, "number"],

      // VM Values
      [/VClosure|VNeutral/, "type.identifier"],

      // Brackets and parentheses
      [/[\[\]()]/, "@brackets"],

      // Whitespace
      {include: "@whitespace"},

      // Other symbols
      [
        /@symbols/,
        {
          cases: {
            "@default": "operator",
          },
        },
      ],
    ],
    whitespace: [
      [/[ \t\r\n]+/, "white"],
      [/(@.*$)/, "comment"],
    ],
  },
};

const languages: Record<"lc" | "vm", LanguageRegistration> = {
  lc: {
    syntax: lcSyntax,
    hover:
      (m: typeof monaco) =>
        (model: monaco.editor.ITextModel, pos: monaco.Position) => {
          const program = model.getValue();
          const hover = lc.hover(program, pos.lineNumber, pos.column);
          if (hover === null) return null;
          const {
            info,
            range: {start, fin},
          } = hover;
          return {
            range: new m.Range(start.line, start.col, fin.line, fin.col),
            contents: info.map((value) => {
              return {value};
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

  const backends = getBackends("print", {
    ir: {
      editorLanguage: "vm",
    },
  });

  return (
    <Playground
      title="lc Playground"
      language="lc"
      source="https://github.com/ayazhafiz/plts/tree/base/lc"
      grammar={`https://github.com/ayazhafiz/plts/blob/base/lc/ast_parser.mly`}
      languageRegistrations={languages}
      backends={backends}
      defaultBackend="ir"
      examples={examples}
      defaultExample={"y_combinator"}
    />
  );
};
export default CoLcPlayground;
