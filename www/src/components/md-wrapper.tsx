import * as React from "react";
import Helmet from "react-helmet";
import Box from "@primer/components/lib/Box";
import ThemeProvider from "@primer/components/lib/ThemeProvider";

const MdWrapper: React.FC<{ title: string }> = (props) => (
  <>
    <Helmet>
      <title>{props.title}</title>
      <link rel="stylesheet" href="https://unpkg.com/@primer/css/dist/primer.css" />
    </Helmet>
    <ThemeProvider>
      <Box className="markdown-body">{props.children}</Box>
    </ThemeProvider>
  </>
);

export default MdWrapper;
