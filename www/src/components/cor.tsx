import type * as monaco from "monaco-editor";
import * as React from "react";
import Playground from "./playground";
import type {
  Backend,
  BackendOverrides,
  LanguageRegistration,
  StringOptions,
} from "../common/types";
import {shapeBackend} from "../common/util";
import * as cor from "cor";
import {useStaticQuery, graphql} from "gatsby";

function getBackends(
  lang: string,
  defaultEmit: string,
  overrides: Record<string, BackendOverrides>
): Record<string, [Backend]> {
  const backends: Record<string, [Backend]> = {};
  for (const phase of cor.phases) {
    const doit = (prog: string, emit: string) =>
      cor.compile(prog, lang, phase, emit);
    const options: [[string, StringOptions]] = [
      ["emit", {value: defaultEmit, options: cor.emits}],
    ];
    const backend: Backend = {
      title: phase,
      editorLanguage: overrides[phase]?.editorLanguage ?? lang,
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
        range: {start, fin},
      } = hover;
      return {
        range: new m.Range(start.line, start.col, fin.line, fin.col),
        contents: info.map((value) => {
          return {value};
        }),
      };
    };
}

const CorPlayground: React.FC<{
  experiment: string;
  defaultPhase: string;
  defaultEmit: string;
  backendOverrides?: Record<string, BackendOverrides>;
  languageRegistrations?: Record<string, LanguageRegistration>;
}> = ({
  experiment,
  defaultPhase,
  defaultEmit,
  backendOverrides = {},
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

    const [examples, setExamples] = React.useState<Record<string, string>>({});

    React.useEffect(() => {
      async function go() {
        const newExamples: Record<string, string> = {};
        for (const file of allExamples.allFile.nodes) {
          if (file.relativePath.includes(`/${experiment}/`)) {
            const exampleName = file.relativePath
              .split("/")
              .at(-1)
              .split(".roc")[0];
            const base = process.env["HOST"];
            const s = await fetch(new URL(file.publicURL, base))
              .then((r) => r.text())
              .then((s) => {
                return cor.userProgram(s);
              });
            newExamples[exampleName] = s;
          }
        }
        setExamples(newExamples);
      }
      go();
    }, [allExamples.allFile.nodes, experiment]);

    return (
      <Playground
        title={`cor/${experiment} Playground`}
        language={experiment}
        source={`https://github.com/ayazhafiz/cor/tree/base/experiments/${experiment}`}
        grammar={`https://github.com/ayazhafiz/cor/blob/base/experiments/${experiment}/parser.mly`}
        languageRegistrations={languageRegistrations}
        backends={getBackends(experiment, defaultEmit, backendOverrides)}
        defaultBackend={defaultPhase}
        examples={examples}
        defaultExample={Object.keys(examples)[0]}
      />
    );
  };

export default CorPlayground;
