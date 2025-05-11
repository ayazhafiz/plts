const MonacoWebpackPlugin = require('monaco-editor-webpack-plugin');
const webpack = require('webpack');
const path = require('path');

exports.onCreateWebpackConfig = ({
  stage,
  rules,
  loaders,
  plugins,
  actions,
}) => {
  actions.setWebpackConfig({
    plugins: [
      new MonacoWebpackPlugin({languages: ['c', 'typescript', 'javascript']}),
      // Simple solution: Replace all node: imports with empty objects
      new webpack.NormalModuleReplacementPlugin(
        /^node:/,
        (resource) => {
          // Replace with an empty module
          resource.request = path.resolve(__dirname, 'src/utils/empty-module.js');
        }
      ),
    ],
    resolve: {
      fallback: {
        constants: false,
        fs: false,
        module: false,
        pnpapi: false,
        node: false
      },
    },
  })
}
