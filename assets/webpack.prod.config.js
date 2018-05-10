const path = require("path")
const webpack = require('webpack')
const StringReplacePlugin = require('string-replace-webpack-plugin')

module.exports = {
  cache: true,
  target: 'web',

  entry: {
    editor: path.join(__dirname, 'src/Pages/Editor/index.js'),
    embed: path.join(__dirname, 'src/Pages/Embed/index.js'),
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
              [ '@babel/preset-env', { 'targets': { 'uglify': true } } ],
              [ '@babel/preset-stage-2', { decoratorsLegacy: true } ]
            ]
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
              { pattern: /'\%HOSTNAME\%'/g, replacement: () => 'window.location.host' },
              { pattern: /'\%PROTOCOL\%'/g, replacement: () => 'window.location.protocol' },
              { pattern: /\%SERVER_ORIGIN\%/g, replacement: () => 'https://' + process.env.SERVER_HOST },
              { pattern: /\%SOCKET_ORIGIN\%/g, replacement: () => 'wss://' + process.env.SERVER_HOST },
              { pattern: /\%ENV\%/g, replacement: () => process.env.NODE_ENV },
              { pattern: /\%PACKAGE_SITE\%/g, replacement: () => 'https://alpha.elm-lang.org' },
            ]
          }),
          {
            loader: 'babel-loader',
            options: {
              presets: [
                [ '@babel/preset-env', { 'targets': { 'uglify': true } } ]
              ],
              plugins: ['babel-plugin-elm-pre-minify']
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
      OPBEAT_APP_ID: JSON.stringify(process.env.OPBEAT_FRONTEND_APP_ID),
      OPBEAT_ORGANIZATION_ID: JSON.stringify(process.env.OPBEAT_ORGANIZATION_ID),
      'process.env.NODE_ENV': JSON.stringify(process.env.NODE_ENV),
    }),
    new webpack.optimize.OccurrenceOrderPlugin(),
    new webpack.optimize.UglifyJsPlugin({ compress: true, mangle: true }),
    new StringReplacePlugin()
  ]
}
