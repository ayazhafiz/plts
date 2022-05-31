import * as React from "react";
import CorPlayground from "../../../components/cor";

const UlsPlayground: React.FC<{}> = ({}) =>
  CorPlayground({
    experiment: "uls",
    defaultPhase: "solve",
    defaultEmit: "elab",
  });

export default UlsPlayground;
