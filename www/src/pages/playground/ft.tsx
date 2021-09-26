import * as React from "react";
import Playground from "../../components/playground";
import type { Backend } from "../../common/types";
import { promisify } from "../../common/util";
import * as ft from "ft";

const examples = {
  "Simple Example": `
fn f(x) = 
  if x is int then 1 else (0, 0)
in (f 1, f (1, 2))
`.trim(),
};

const backends: {
  [K in "Typecheck" | "Infer and Elaborate" | "Subtyping Calculator"]: [Backend];
} = {
  Typecheck: [
    {
      title: "Annotations",
      do: promisify(ft.ftCheck),
      options: [["infer", true]],
      editorLanguage: "text",
    },
  ],
  "Infer and Elaborate": [
    {
      title: "Annotations",
      do: promisify(ft.ftInfer),
      options: [],
      editorLanguage: "text",
    },
  ],
  "Subtyping Calculator": [
    {
      title: "Result",
      do: promisify(ft.subtypeCheck),
      options: [],
      editorLanguage: "text",
    },
  ],
};

const FtPlayground = () => (
  <Playground
    language="text"
    source="https://github.com/ayazhafiz/plts/tree/base/ft"
    grammar="https://github.com/ayazhafiz/plts/blob/base/ft/parser.mly"
    languageRegistrations={{}}
    backends={backends}
    defaultBackend="Typecheck"
    examples={examples}
    defaultExample="Simple Example"
  />
);
export default FtPlayground;
