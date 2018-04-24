const path = require("path")
const webpack = require('webpack')
const StringReplacePlugin = require('string-replace-webpack-plugin')

module.exports = {
  cache: true,
  target: 'web',

  entry: {
    editor: ['es6-promise/auto', path.join(__dirname, 'src/Pages/Editor/index.js')],
    embed: ['es6-promise/auto', path.join(__dirname, 'src/Pages/Embed/index.js')],
  },

  output: {
    path: path.resolve(__dirname + '/../priv/static'),
    filename: '[name].js',
    chunkFilename: 'chunk.[name].js',
    publicPath: '/assets/'
  },

  module: {
    rules: [
      {
        test: /\.svg$/,
        use: { loader: 'svg-inline-loader' }
      },
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: 'babel-loader',
          options: {
            presets: [
              ['stage-2'],
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
        test:    /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loaders:  [
          StringReplacePlugin.replace({
            replacements: [
              { pattern: /\%SERVER_ORIGIN\%/g, replacement: () => process.env.SERVER_ORIGIN },
              { pattern: /\%SOCKET_ORIGIN\%/g, replacement: () => process.env.SOCKET_ORIGIN },
              { pattern: /\%ENV\%/g, replacement: () => process.env.NODE_ENV },
              { pattern: /\%PACKAGE_SITE\%/g, replacement: () => process.env.PACKAGE_SITE },
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
            options: { yes: true, debug: false, cwd: path.join(__dirname) }
          }
        ]
      },
    ]
  },

  plugins: [
    new webpack.DefinePlugin({
      SERVER_ORIGIN: JSON.stringify(process.env.SERVER_ORIGIN),
      CDN_BASE: JSON.stringify(process.env.CDN_BASE),
      OPBEAT_APP_ID: JSON.stringify(process.env.OPBEAT_FRONTEND_APP_ID),
      OPBEAT_ORGANIZATION_ID: JSON.stringify(process.env.OPBEAT_ORGANIZATION_ID),
      'process.env.NODE_ENV': JSON.stringify(process.env.NODE_ENV),
    }),
    new webpack.optimize.OccurrenceOrderPlugin(),
    // new webpack.optimize.UglifyJsPlugin({ compress: true, mangle: true }),
    new StringReplacePlugin()
  ]
}
