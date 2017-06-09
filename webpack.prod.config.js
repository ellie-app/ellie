var path = require("path");
var webpack = require('webpack');
var StringReplacePlugin = require('string-replace-webpack-plugin');
var ExtractTextPlugin = require('extract-text-webpack-plugin');
var HtmlWebpackPlugin = require('html-webpack-plugin');
var ManifestPlugin = require('webpack-manifest-plugin');
var syncRequest = require('sync-request');
const Md5Hash = require('webpack-md5-hash');

var target = process.env.BUILD_TARGET || 'editor'

var entries = {
  editor: ['es6-promise', path.join(__dirname, 'src/js/Shared/Polyfills'), path.join(__dirname, 'src/js/Apps/Editor/Main.js')],
  embed: ['es6-promise', path.join(__dirname, 'src/js/Shared/Polyfills'), path.join(__dirname, 'src/js/Apps/Embed/Main.js')],
}

module.exports = {
  cache: true,
  target: 'web',

  externals: {
    'fs': '__fileSystem'
  },

  entry: {
    app: entries[target]
  },

  output: {
    path: path.resolve(__dirname + '/build', target),
    filename: '[name].[chunkhash:8].js',
    chunkFilename: 'chunk.[name].[chunkhash:8].js',
    publicPath: process.env.CDN_BASE + '/assets/' + target + '/'
  },

  module: {
    rules: [
      {
        test: /\.js$/,
        exclude: /(node_modules|bower_components)/,
        use: {
          loader: 'babel-loader',
          options: {
            presets: [
              ['es2015', { modules: false }],
              'flow',
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
        test: /Stylesheets\.elm$/,
        loader: ExtractTextPlugin.extract({
          fallback: 'style-loader',
          use: 'css-loader!postcss-loader!elm-css-webpack-loader'
        }),
        exclude: /node_modules/,
      },
      {
        test:    /\.html$/,
        exclude: /node_modules/,
        loader:  'file-loader?name=[name].[ext]',
      },
      {
        test: /\.worker\.js$/,
        loader: 'worker-loader?inline'
      },
      {
        test:    /Main\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loaders:  [
          StringReplacePlugin.replace({
            replacements: [
              { pattern: /\%API_PATH\%/g, replacement: () => 'api' },
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
    new webpack.LoaderOptionsPlugin({
      options: {
        worker: {
          plugins: [
            new webpack.DllReferencePlugin({
              scope: 'Dll',
              manifest: JSON.parse(syncRequest('GET', process.env.CDN_BASE + '/elm-compilers/0.18.0-manifest.json').body),
              context: path.join(__dirname, 'dll')
            })
          ]
        }
      }
    }),
    new webpack.DefinePlugin({
      SERVER_ORIGIN: JSON.stringify(process.env.SERVER_HOSTNAME),
      CDN_BASE: JSON.stringify(process.env.CDN_BASE),
      'process.env.NODE_ENV': JSON.stringify('production'),
      'process.version': '"v0.8"'
    }),
    new webpack.optimize.OccurrenceOrderPlugin(),
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
    new ManifestPlugin(),
    new StringReplacePlugin(),
    new ExtractTextPlugin('app.[chunkhash:8].css'),
  ]
};
