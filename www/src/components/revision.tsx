import * as React from "react";
import { Label } from "@primer/react";
import styled from "styled-components";

const Link = styled.a`
  text-decoration: none;

  &:focus,
  &:hover,
  &:visited,
  &:link,
  &:active {
    text-decoration: none;
  }
`;

const Revision: React.FC<{}> = () => {
  console.log(process.env);
  if (process.env["GATSBY_GIT_PORCELAIN"]) {
    const sha = process.env["GATSBY_GIT_SHA"];
    return (
      <Label sx={{ verticalAlign: "middle" }} variant="accent">
        <Link href={`https://github.com/ayazhafiz/plts/commit/${sha}`}>
          {sha}
        </Link>
      </Label>
    );
  } else {
    return (
      <Label sx={{ verticalAlign: "middle" }} variant="attention">
        devel
      </Label>
    );
  }
};

export default Revision;
