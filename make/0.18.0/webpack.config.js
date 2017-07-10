var path = require("path")
var webpack = require("webpack")
var UglifyJsPlugin = require('uglifyjs-webpack-plugin')
var StringReplacePlugin = require('string-replace-webpack-plugin')

module.exports = {
    entry: {
      Make0180: [path.join(__dirname, 'src/index.js')],
    },
    target: 'web',
    node: {
      fs: 'empty',
      child_process: 'empty',
    },
    module: {
      noParse: /\.elm$/,
      loaders: [
        {
          test: /\.js$/,
          exclude: /(node_modules|bower_components)/,
          use: {
            loader: 'babel-loader',
            options: {
              presets: [
                'es2017',
                ['env', { 'targets': { 'uglify': true } }]
              ],
              plugins: ['dynamic-import-webpack']
            }
          }
        },{
          test:    /Main\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          loaders:  [
            StringReplacePlugin.replace({
              replacements: [
                { pattern: /\%CDN_BASE\%/g, replacement: () => process.env.CDN_BASE },
              ]
            }),
            'elm-webpack-loader?yes',
          ]
        },
        {
          test: /\.json$/,
          loader: 'json-loader'
        }
      ]
    },
    output: {
      path: path.join(__dirname, 'build'),
  		filename: "[name].js",
  		library: '[name]',
      libraryTarget: 'umd'
    },
    plugins: [
      new webpack.DefinePlugin({
        'CDN_BASE': JSON.stringify(process.env.CDN_BASE)
      }),
      // new UglifyJsPlugin({
      //   compress: {
      //     warnings: false,
      //     dead_code: true,
      //     pure_funcs: [...Array(8).keys()].map(z => 'A' + (z + 2)),
      //     pure_getters: true,
      //     passes: 3,
      //     keep_fargs: false,
      //     toplevel: true
      //   }
      // })
    ]
};
