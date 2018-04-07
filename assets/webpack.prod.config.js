const path = require("path")
const webpack = require('webpack')
const StringReplacePlugin = require('string-replace-webpack-plugin')
const ManifestPlugin = require('webpack-manifest-plugin')


module.exports = {
  cache: true,
  target: 'web',

  externals: {
    'fs': '__fileSystem'
  },

  entry: {
    editor: ['es6-promise', path.join(__dirname, 'src/Pages/Editor/index.js')],
    embed: ['es6-promise', path.join(__dirname, 'src/Pages/Embed/index.js')],
  },

  output: {
    path: path.resolve(__dirname + '/build'),
    filename: '[name].[chunkhash:8].js',
    chunkFilename: 'chunk.[name].[chunkhash:8].js',
    publicPath: process.env.CDN_BASE + '/assets/'
  },

  module: {
    rules: [
      {
        test: /\.svg$/,
        use: {
          loader: 'svg-inline-loader'
        }
      },
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
        loader: 'style-loader!css-loader'
      },
      {
        test:    /Main\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loaders:  [
          StringReplacePlugin.replace({
            replacements: [
              { pattern: /\%CDN_BASE\%/g, replacement: () => process.env.CDN_BASE },
              { pattern: /\%SERVER_ORIGIN\%/g, replacement: () => process.env.SERVER_HOSTNAME },
              { pattern: /\%ENV\%/g, replacement: () => process.env.NODE_ENV },
            ]
          }),
          {
            loader: 'babel-loader',
            options: {
              presets: [
                [ 'env', { 'targets': { 'uglify': true } } ]
              ],
              plugins: ['elm-pre-minify']
            },
          },
          {
            loader: 'elm-webpack-loader',
            options: {
              yes: true,
              debug: false,
              cwd: path.join(__dirname),
            }
          }
        ]
      },
    ]
  },

  plugins: [
    new webpack.HashedModuleIdsPlugin(),
    new webpack.DefinePlugin({
      SERVER_ORIGIN: JSON.stringify(process.env.SERVER_HOSTNAME),
      CDN_BASE: JSON.stringify(process.env.CDN_BASE),
      OPBEAT_APP_ID: JSON.stringify(process.env.OPBEAT_FRONTEND_APP_ID),
      OPBEAT_ORGANIZATION_ID: JSON.stringify(process.env.OPBEAT_ORGANIZATION_ID),
      'process.env.NODE_ENV': JSON.stringify(process.env.NODE_ENV),
    }),
    new webpack.optimize.OccurrenceOrderPlugin(),
    new webpack.optimize.UglifyJsPlugin({
      compress: true,
      mangle: true
    }),
    new ManifestPlugin(),
    new StringReplacePlugin()
  ]
}
