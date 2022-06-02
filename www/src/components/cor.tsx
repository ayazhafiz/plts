import type * as monaco from "monaco-editor";
import * as React from "react";
import Playground from "./playground";
import type {
  Backend,
  LanguageRegistration,
  StringOptions,
} from "../common/types";
import { shapeBackend } from "../common/util";
import * as cor from "cor";
import { useStaticQuery, graphql } from "gatsby";

function getBackends(
  lang: string,
  defaultEmit: string
): Record<string, [Backend]> {
  const backends: Record<string, [Backend]> = {};
  for (const phase of cor.phases) {
    let doit = (prog: string, emit: string) =>
      cor.compile(prog, lang, phase, emit);
    let options: [[string, StringOptions]] = [
      ["emit", { value: defaultEmit, options: cor.emits }],
    ];
    let backend: Backend = {
      title: phase,
      editorLanguage: lang,
      ...shapeBackend(doit, options),
    };
    backends[phase] = [backend];
  }
  return backends;
}

function createHover(lang: string): LanguageRegistration["hover"] {
  return (m: typeof monaco) =>
    (model: monaco.editor.ITextModel, pos: monaco.Position) => {
      const program = model.getValue();
      const hover = cor.hover(program, lang, pos.lineNumber, pos.column);
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
    };
}

const CorPlayground: React.FC<{
  experiment: string;
  defaultPhase: string;
  defaultEmit: string;
  languageRegistrations?: Record<string, LanguageRegistration>;
}> = ({
  experiment,
  defaultPhase,
  defaultEmit,
  languageRegistrations = {},
}) => {
  if (languageRegistrations[experiment]) {
    languageRegistrations[experiment].hover = createHover(experiment);
  }

  const allExamples = useStaticQuery(graphql`
    {
      allFile(filter: { extension: { eq: "roc" } }) {
        nodes {
          publicURL
          relativePath
        }
      }
    }
  `);

  const examples: Record<string, string> = {};
  for (const file of allExamples.allFile.nodes) {
    if (file.relativePath.includes(`/${experiment}/`)) {
      const exampleName = file.relativePath.split("/").at(-1).split(".roc")[0];
      const [content, setContent] = React.useState("");
      const base = process.env["HOST"];
      fetch(new URL(file.publicURL, base))
        .then((r) => r.text())
        .then((s) => {
          return cor.userProgram(s);
        })
        .then(setContent);
      examples[exampleName] = content;
    }
  }

  return (
    <Playground
      title={`cor/${experiment} Playground`}
      language={experiment}
      source={`https://github.com/ayazhafiz/cor/tree/base/experiments/${experiment}`}
      grammar={`https://github.com/ayazhafiz/cor/blob/base/experiments/${experiment}/parser.mly`}
      languageRegistrations={languageRegistrations}
      backends={getBackends(experiment, defaultEmit)}
      defaultBackend={defaultPhase}
      examples={examples}
      defaultExample={Object.keys(examples)[0]}
    />
  );
};

export default CorPlayground;
