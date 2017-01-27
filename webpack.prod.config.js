var path = require("path");
var webpack = require('webpack');
var DashboardPlugin = require('webpack-dashboard/plugin');
var StringReplacePlugin = require('string-replace-webpack-plugin');
var ExtractTextPlugin = require('extract-text-webpack-plugin');
var WebpackMd5Hash = require('webpack-md5-hash');
var HtmlWebpackPlugin = require('html-webpack-plugin');

var target = process.env.BUILD_TARGET || 'editor'

var entries = {
  editor: './js/Apps/Editor/Main.js',
  embed: './js/Apps/Embed/Main.js'
}

module.exports = {
  context: path.join(__dirname, 'src'),

  entry: {
    app: [ entries[target] ]
  },

  output: {
    path: path.resolve(__dirname + '/build', target),
    filename: '[name].[chunkhash:8].js',
    chunkFilename: 'chunk.[name].[chunkhash:8].js',
  },

  module: {
    // noParse: /Stylesheets\.elm$/,
    loaders: [
      {
        test: /\.css$/,
        loader: 'style!css',
      },
      {
        test: /Stylesheets\.elm$/,
        loader: ExtractTextPlugin.extract('css-loader?minimize!postcss-loader!elm-css-webpack-loader?cache=false'),
        exclude: /node_modules/,
      },
      {
        test: /ServiceWorker\.js$/,
        exclude: /node_modules/,
        loader: 'serviceworker',
      },
      {
        test:    /Main\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loaders:  [
          StringReplacePlugin.replace({
            replacements: [
              { pattern: /\%API_BASE\%/g, replacement: () => process.env.API_BASE },
              { pattern: /\%CDN_BASE\%/g, replacement: () => process.env.CDN_BASE },
              { pattern: /\%EMBED_BASE\%/g, replacement: () => process.env.EMBED_BASE },
              { pattern: /\%EDITOR_BASE\%/g, replacement: () => process.env.EDITOR_BASE },
            ]
          }),
          'elm-webpack-loader?yes',
        ]
      },
      {
        test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
        loader: 'url-loader?limit=10000&mimetype=application/font-woff',
      },
      {
        test: /\.(ttf|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
        loader: 'file-loader',
      },
    ]
  },

  plugins: [
    new WebpackMd5Hash(),
    new webpack.DefinePlugin({
      API_BASE: JSON.stringify(process.env.API_BASE || 'http://localhost:1337'),
      'process.env.NODE_ENV': JSON.stringify('production')
    }),
    new webpack.optimize.OccurrenceOrderPlugin(),
    new webpack.optimize.DedupePlugin(),
    new webpack.optimize.UglifyJsPlugin({
      compress: {
        screw_ie8: true,
        warnings: false,
        dead_code: true,
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
    new StringReplacePlugin(),
    new ExtractTextPlugin('main.[chunkhash:8].css'),
    new HtmlWebpackPlugin({
      inject: true,
      template: path.join(__dirname, 'src/index.ejs'),
      minify: {
        removeComments: true,
        collapseWhitespace: true,
        removeRedundantAttributes: true,
        useShortDoctype: true,
        removeEmptyAttributes: true,
        removeStyleLinkTypeAttributes: true,
        keepClosingSlash: true,
        minifyJS: true,
        minifyCSS: true,
        minifyURLs: true
      }
    })
  ]
};
