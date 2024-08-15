import type * as monaco from "monaco-editor";
import * as React from "react";
import type {
  Backend,
  BackendOverrides,
  LanguageRegistration,
  StringOptions,
} from "../common/types";
import {shapeBackend} from "../common/util";
import * as cor from "cor";
import {useStaticQuery, graphql} from "gatsby";
import Playground2 from "./playground2";
import {Spinner} from "@primer/react";

function getBackends(
  lang: string,
  defaultEmit: string,
  overrides: Record<string, BackendOverrides>,
  phases: string[],
  emits: string[],
): Record<string, Backend> {
  const backends: Record<string, Backend> = {};
  for (const phase of phases) {
    let doit = (prog: string, emit: string) =>
      cor.compile(prog, lang, phase, emit);
    let options: [[string, StringOptions]] = [
      ["emit", {value: defaultEmit, options: emits}],
    ];
    let backend: Backend = {
      title: phase,
      editorLanguage: overrides[phase]?.editorLanguage ?? "text",
      ...shapeBackend(doit, options),
    };
    backends[phase] = backend;
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

const defaultPhases = ["parse", "can", "solve", "mono", "ir", "elab"];
const defaultEmits = ["print", "elab"];

const CorPlayground: React.FC<{
  experiment: string;
  defaultPhase: string;
  defaultEmit: string;
  phases?: string[];
  emits?: string[];
  backendOverrides?: Record<string, BackendOverrides>;
  languageRegistrations?: Record<string, LanguageRegistration>;
  defaultExample?: string;
}> = ({
  experiment,
  defaultPhase,
  defaultEmit,
  phases = defaultPhases,
  emits = defaultEmits,
  backendOverrides = {},
  languageRegistrations = {},
  defaultExample,
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

    const examplePromises: Promise<string[] | undefined>[] = [];
    for (const file of allExamples.allFile.nodes) {
      if (file.relativePath.includes(`/${experiment}/`)) {
        const exampleName = file.relativePath
          .split(`${experiment}/test/`)
          .at(-1)
          .split(".roc")[0];
        const base = process.env["HOST"];
        const promise = fetch(new URL(file.publicURL, base))
          .then((r) => r.text())
          .then((s) => cor.userProgram(s))
          .then((text) => [exampleName as string, text.trimStart()])
          .catch(() => {
            console.log("failed to fetch", base, file.publicURL);
            return undefined;
          });
        examplePromises.push(promise);
      }
    }

    Promise.all(examplePromises).then((examples) => {
      const exampleMap: Record<string, string> = {};
      for (const example of examples) {
        if (example) {
          exampleMap[example[0]] = example[1];
        }
      }
      setExamples(exampleMap);
    });

    return Object.keys(examples).length > 0 ? (
      <Playground2
        title={`cor/${experiment} Playground`}
        language={experiment}
        source={`https://github.com/ayazhafiz/cor/tree/base/experiments/${experiment}`}
        grammar={`https://github.com/ayazhafiz/cor/blob/base/experiments/${experiment}/parser.mly`}
        languageRegistrations={languageRegistrations}
        backends={getBackends(
          experiment,
          defaultEmit,
          backendOverrides,
          phases,
          emits,
        )}
        defaultBackend={defaultPhase}
        emits={emits}
        defaultEmit={defaultEmit}
        examples={examples}
        defaultExample={defaultExample ?? Object.keys(examples)[0]}
      />
    ) : (
      <Spinner size="large" />
    );
  };

export default CorPlayground;
