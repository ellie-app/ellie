var path = require("path");
var webpack = require("webpack");

module.exports = {
    entry: {
      ElmCompiler: [path.join(__dirname, 'dll/ElmCompiler.js')],
    },
    target: 'web',
    node: {
      fs: 'empty',
      child_process: 'empty',
    },
    module: {
      loaders: [
        {
          test: /\.json$/,
          loader: 'json-loader'
        }
      ]
    },
    output: {
      path: path.join(__dirname, "build/dll"),
  		filename: "Dll.[name].js",
  		library: "[name]_[hash]"
    },
    plugins: [
      new webpack.optimize.UglifyJsPlugin({
        compress: {
          warnings: false,
          dead_code: true,
        }
      }),
      new webpack.DllPlugin({
        context: path.join(__dirname, 'dll'),
        path: path.join(__dirname, "build/dll/", "[name]-manifest.json"),
        name: "[name]_[hash]"
      }),
    ]
};
