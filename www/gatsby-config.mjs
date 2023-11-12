import path from "path"
import {fileURLToPath} from "url"
import {createRequire} from "module"

const __dirname = path.dirname(fileURLToPath(import.meta.url))
const require = createRequire(import.meta.url)

const config = {
  pathPrefix: process.env.PUBLISH ? '/plts' : '',
  flags: {
    DEV_SSR: false,
  },
  siteMetadata: {
    siteUrl: 'https://ayazhafiz.com',
    title: 'plts',
  },
  plugins: [
    {
      resolve: 'gatsby-plugin-typescript',
      options: {
        isTSX: true,
        jsxPragma: `jsx`,
        allExtensions: true,
      },
    },
    {
      resolve: 'gatsby-source-filesystem',
      options: {
        name: 'cor',
        path: path.resolve(__dirname, '../cor/'),
        ignore: ['**/dune', '**/dune-project', '**/*.!(roc)']
      },
    },
    {
      resolve: 'gatsby-source-filesystem',
      options: {
        name: 'co_lc',
        path: path.resolve(__dirname, '../co_lc/'),
        ignore: ['**/dune', '**/dune-project', '**/*.!(co)']
      },
    },
    {
      resolve: 'gatsby-source-filesystem',
      options: {
        name: 'pages',
        path: path.resolve(__dirname, './src/pages/')
      },
    },
    {
      resolve: `gatsby-plugin-mdx`,
    },
  ],
};

export default config;
