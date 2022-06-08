const path = require('path');

module.exports = {
  pathPrefix: process.env.NODE_ENV === 'production' ? '/plts' : '',
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
    // {
    //   resolve: 'gatsby-plugin-typescript-checker',
    // },
  ],
};
