var path = require("path");
var webpack = require('webpack');
var StringReplacePlugin = require('string-replace-webpack-plugin');
var HtmlWebpackPlugin = require('html-webpack-plugin');
var CopyPlugin = require('copy-webpack-plugin');

var target = process.env.BUILD_TARGET || 'editor'

var entries = {
  editor: ['webpack-dev-server/client?http://localhost:8000/', path.join(__dirname, 'src/js/Apps/Editor/Main.js')],
  embed: ['webpack-dev-server/client?http://localhost:8001/', path.join(__dirname, 'src/js/Apps/Embed/Main.js')]
}

const loggingPoxy = obj => new Proxy(obj, {
  get(target, prop) {
    console.log('get', prop)
    return typeof target[prop] === 'object' ?
      loggingPoxy(target[prop]) :
      target[prop]
  }
})

module.exports = {
  cache: true,
  target: 'web',

  node: {
    fs: 'empty'
  },

  entry: {
    app: entries[target]
  },

  output: {
    path: path.resolve(__dirname + '/dist', target),
    publicPath: target === 'editor' ? 'http://localhost:8000/' : 'http://localhost:8001/',
    filename: '[name].js',
  },

  module: {
    loaders: [
      {
        test: /\.css$/,
        loader: 'style-loader!css-loader',
      },
      { test: /\.json$/,
        loader: 'json-loader'
      },
      {
        test: /Stylesheets\.elm$/,
        loader: 'style-loader!css-loader!elm-css-webpack-loader',
        exclude: [/node_modules/]
      },
      {
        test:    /\.html$/,
        exclude: /node_modules/,
        loader:  'file-loader?name=[name].[ext]',
      },
      {
        test: /ServiceWorker\.js$/,
        exclude: /node_modules/,
        loader: 'serviceworker-loader',
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
              { pattern: /\%API_ORIGIN\%/g, replacement: () => 'http://localhost:1337' },
              { pattern: /\%CDN_BASE\%/g, replacement: () => 'http://localhost:1337/cdn-proxy' },
              { pattern: /\%EMBED_BASE\%/g, replacement: () => 'http://localhost:8001' },
              { pattern: /\%EDITOR_BASE\%/g, replacement: () => 'http://localhost:1338' },
              { pattern: /\%CARBON_ZONE_ID\%/g, replacement: () => 'test' },
              { pattern: /\%CARBON_SERVE\%/g, replacement: () => 'test' },
              { pattern: /\%CARBON_PLACEMENT\%/g, replacement: () => 'test' },
              { pattern: /\%ENV\%/g, replacement: () => 'development' },
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
    new webpack.LoaderOptionsPlugin({
      options: {
        worker: {
          plugins: [
            new webpack.DllReferencePlugin({
              scope: 'Dll',
              manifest: require("./build/dll/ElmCompiler-manifest.json"),
              context: path.join(__dirname, 'dll')
            })
          ]
        }
      }
    }),
    new webpack.DefinePlugin({
      API_ORIGIN: JSON.stringify(process.env.API_ORIGIN || 'http://localhost:1337'),
      'process.env.NODE_ENV': JSON.stringify(process.env.NODE_ENV)
    }),
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
    }),
    new CopyPlugin([
      { from: path.join(__dirname, 'build/dll/*.js'), flatten: true }
    ])
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
