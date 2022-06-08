import * as React from "react";
import Helmet from "react-helmet";
import { Box, ThemeProvider } from "@primer/react";

const MdWrapper: React.FC<{
  children: React.ReactNode;
  title: string;
  margin: [number, number, number];
}> = ({ title = "plts", children, margin = [4, 4, 10] }) => (
  <>
    <Helmet>
      <title>{title}</title>
      <link
        rel="stylesheet"
        href="https://unpkg.com/@primer/css/dist/primer.css"
      />
    </Helmet>
    <ThemeProvider>
      <Box className="markdown-body">
        <Box m={margin}>{children}</Box>
      </Box>
    </ThemeProvider>
  </>
);

export default MdWrapper;
