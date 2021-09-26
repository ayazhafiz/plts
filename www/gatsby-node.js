const MonacoWebpackPlugin = require('monaco-editor-webpack-plugin');

exports.onCreateWebpackConfig = ({
  stage,
  rules,
  loaders,
  plugins,
  actions,
}) => {
  actions.setWebpackConfig({
    module: {
      rules: [],
    },
    plugins: [
      new MonacoWebpackPlugin({languages: ['c', 'typescript', 'javascript']}),
    ],
    resolve: {
      fallback: {
        constants: false,
        fs: false,
        module: false,
        pnpapi: false,
      },
    },
  })
}
