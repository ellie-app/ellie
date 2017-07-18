const path = require("path")
const webpack = require('webpack')
const StringReplacePlugin = require('string-replace-webpack-plugin')
const ExtractTextPlugin = require('extract-text-webpack-plugin')
const ManifestPlugin = require('webpack-manifest-plugin')
const Md5Hash = require('webpack-md5-hash')
const CopyPlugin = require('copy-webpack-plugin')

module.exports = {
  cache: true,
  target: 'web',

  externals: {
    'fs': '__fileSystem'
  },

  entry: {
    editor: ['es6-promise', path.join(__dirname, 'client/src/Pages/Editor/index.js')],
    embed: ['es6-promise', path.join(__dirname, 'client/src/Pages/Embed/index.js')],
  },

  output: {
    path: path.resolve(__dirname + '/build'),
    filename: '[name].[chunkhash:8].js',
    chunkFilename: 'chunk.[name].[chunkhash:8].js',
    publicPath: process.env.CDN_BASE + '/assets/'
  },

  resolve: {
    alias: {
      'Make/0.18.0$': path.resolve(__dirname, 'make/0.18.0/build/Make0180.js')
    }
  },

  module: {
    rules: [
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: 'babel-loader',
          options: {
            presets: [
              ['es2017'],
              ['env', { 'targets': { 'uglify': true } }]
            ],
            plugins: ['syntax-dynamic-import']
          }
        }
      },
      {
        test: /\.css$/,
        loader: ExtractTextPlugin.extract({
          fallback: 'style-loader',
          use: 'css-loader!postcss-loader'
        })
      },
      { test: /\.json$/,
        loader: 'json-loader'
      },
      {
        test: /Editor\/Stylesheets\.elm$/,
        loader: ExtractTextPlugin.extract({
          fallback: 'style-loader',
          use: [
            'css-loader',
            'postcss-loader',
            {
              loader: 'elm-css-webpack-loader',
              options: {
                module: 'Pages.Editor.Stylesheets'
              }
            }
          ]
        }),
      },
      {
        test: /Embed\/Stylesheets\.elm$/,
        loader: ExtractTextPlugin.extract({
          fallback: 'style-loader',
          use: [
            'css-loader',
            'postcss-loader',
            {
              loader: 'elm-css-webpack-loader',
              options: {
                module: 'Pages.Embed.Stylesheets'
              }
            }
          ]
        }),
      },
      {
        test:    /Main\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loaders:  [
          StringReplacePlugin.replace({
            replacements: [
              { pattern: /\%CDN_BASE\%/g, replacement: () => process.env.CDN_BASE },
              { pattern: /\%SERVER_ORIGIN\%/g, replacement: () => process.env.SERVER_HOSTNAME },
              { pattern: /\%CARBON_ZONE_ID\%/g, replacement: () => process.env.CARBON_ZONE_ID },
              { pattern: /\%CARBON_SERVE\%/g, replacement: () => process.env.CARBON_SERVE },
              { pattern: /\%CARBON_PLACEMENT\%/g, replacement: () => process.env.CARBON_PLACEMENT },
              { pattern: /\%ENV\%/g, replacement: () => 'production' },
            ]
          }),
          'elm-webpack-loader?yes',
        ]
      },
    ]
  },

  plugins: [
    new Md5Hash(),
    new webpack.DefinePlugin({
      SERVER_ORIGIN: JSON.stringify(process.env.SERVER_HOSTNAME),
      CDN_BASE: JSON.stringify(process.env.CDN_BASE),
      'process.env.NODE_ENV': JSON.stringify('production'),
    }),
    new webpack.optimize.OccurrenceOrderPlugin(),
    new webpack.optimize.UglifyJsPlugin({
      compress: {
        screw_ie8: true,
        warnings: false,
        dead_code: true,
        pure_funcs: ['A2', 'A3', 'A4', 'A5', 'A6', 'A7', 'A8', 'A9', '_elm_lang$core$Native_Utils.update', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8', 'F9'],
        passes: 2
      },
      mangle: {
        screw_ie8: true
      },
      output: {
        comments: false,
        screw_ie8: true
      }
    }),
    new ManifestPlugin(),
    new StringReplacePlugin(),
    new ExtractTextPlugin('[name].[chunkhash:8].css'),
    new CopyPlugin([
      { from: path.join(__dirname, 'images'), to: 'images', toType: 'dir' }
    ])
  ]
}
