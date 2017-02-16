var path = require("path");
var webpack = require('webpack');
var DashboardPlugin = require('webpack-dashboard/plugin');
var StringReplacePlugin = require('string-replace-webpack-plugin');
var HtmlWebpackPlugin = require('html-webpack-plugin');

var target = process.env.BUILD_TARGET || 'editor'

var entries = {
  editor: ['webpack-dev-server/client?http://localhost:8000/', './js/Apps/Editor/Main.js'],
  embed: ['webpack-dev-server/client?http://localhost:8001/', './js/Apps/Embed/Main.js']
}

module.exports = {
  context: path.join(__dirname, 'src'),

  entry: {
    app: entries[target]
  },

  output: {
    path: path.resolve(__dirname + '/dist', target),
    publicPath: 'http://localhost:8000/',
    filename: '[name].js',
  },

  module: {
    loaders: [
      {
        test: /\.css$/,
        loader: 'style!css',
      },
      {
        test: /Stylesheets\.elm$/,
        loader: 'style!css!elm-css-webpack',
        exclude: [/node_modules/]
      },
      {
        test:    /\.html$/,
        exclude: /node_modules/,
        loader:  'file?name=[name].[ext]',
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
              { pattern: /\%API_BASE\%/g, replacement: () => 'http://localhost:1337' },
              { pattern: /\%CDN_BASE\%/g, replacement: () => 'http://localhost:1337/cdn-proxy' },
              { pattern: /\%EMBED_BASE\%/g, replacement: () => 'http://localhost:8001' },
              { pattern: /\%EDITOR_BASE\%/g, replacement: () => 'http://localhost:8000' },
            ]
          }),
          'elm-webpack-loader?yes&debug',
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
    new webpack.DefinePlugin({
      API_BASE: JSON.stringify(process.env.API_BASE || 'http://localhost:1337'),
      'process.env.NODE_ENV': JSON.stringify(process.env.NODE_ENV)
    }),
    new DashboardPlugin(),
    new StringReplacePlugin(),
    new HtmlWebpackPlugin({
      inject: true,
      template: path.join(__dirname, 'src/index.ejs'),
      data: {
        production: false,
        gtmId: '',
      },
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

  ],

  devServer: {
    inline: true,
    stats: { colors: true },
    historyApiFallback: true,
    port: target === 'editor' ? '8000' : '8001',
    watchOptions: {
      aggregateTimeout: 300,
      poll: 1000
    },
  },
};
