import * as React from "react";
import ReactMarkdown from "react-markdown";
import MdWrapper from "../components/md-wrapper";
import Box from "@primer/components/lib/Box";

const Index: React.FC<{}> = () => {
  const [content, setContent] = React.useState("");
  React.useEffect(() => {
    fetch("/index.md")
      .then((r) => r.text())
      .then((r) => setContent(r));
  });
  return (
    <MdWrapper>
      <Box m={[4, 4, 10]}>
        <ReactMarkdown children={content} />
      </Box>
    </MdWrapper>
  );
};

export default Index;