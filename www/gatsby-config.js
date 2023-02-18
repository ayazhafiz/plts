const path = require('path');

module.exports = {
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
      resolve: `gatsby-plugin-mdx`,
      options: {
        defaultLayouts: {
          default: require.resolve('./src/components/md-wrapper.tsx'),
        }
      }
    },
  ],
};
