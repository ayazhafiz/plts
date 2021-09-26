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
      resolve: `gatsby-plugin-typescript`,
      options: {
        isTSX: true,          // defaults to false
        jsxPragma: `jsx`,     // defaults to "React"
        allExtensions: true,  // defaults to false
      },
    },
  ],
};
