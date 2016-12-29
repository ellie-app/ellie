var path = require("path");
var webpack = require('webpack');
var DashboardPlugin = require('webpack-dashboard/plugin');
var StringReplacePlugin = require('string-replace-webpack-plugin');
var ExtractTextPlugin = require('extract-text-webpack-plugin');

module.exports = {
  context: path.join(__dirname, 'src'),

  entry: {
    app: [
      './js/Main.js'
    ]
  },

  output: {
    path: path.resolve(__dirname + '/build'),
    filename: '[name].js',
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
        loader: ExtractTextPlugin.extract('css-loader?minimize!postcss-loader!elm-css-webpack-loader'),
        exclude: [/node_modules/]
      },
      {
        test:    /\.html$/,
        exclude: /node_modules/,
        loader:  'file?name=[name].[ext]',
      },
      {
        test:    /Main\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loaders:  [
          StringReplacePlugin.replace({
            replacements: [
              { pattern: /\%API_BASE\%/g, replacement: () => process.env.API_BASE || 'http://localhost:1337' }
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
    new ExtractTextPlugin('main.css')
  ]
};
