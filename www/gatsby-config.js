module.exports = {
  pathPrefix: '/plts',
  flags: {
    DEV_SSR: false,
  },
  siteMetadata: {
    siteUrl: 'https://www.yourdomain.tld',
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
      resolve: 'gatsby-plugin-typescript-checker',
    },
  ],
};
