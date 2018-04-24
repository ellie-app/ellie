const path = require('path')
const webpack = require('webpack')
const StringReplacePlugin = require('string-replace-webpack-plugin')

module.exports = {
  cache: true,
  target: 'web',
  devtool: 'inline-source-map',
  context: __dirname,
  entry: {
    editor: [
      'es6-promise/auto',
      'webpack-dev-server/client?http://localhost:8080/',
      path.join(__dirname, 'src/Pages/Editor/index.js')
    ],
    embed: [
      'es6-promise/auto',
      'webpack-dev-server/client?http://localhost:8080/',
      path.join(__dirname, 'src/Pages/Embed/index.js')
    ]
  },

  output: {
    path: path.resolve(__dirname, "../priv/static"),
    publicPath: 'http://localhost:8080/assets/',
    filename: '[name].js',
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
        exclude: /(node_modules|make)/,
        use: {
          loader: 'babel-loader',
          options: {
            presets: ['es2017'],
            plugins: ['syntax-dynamic-import'],
          },
        },
      },
      {
        test: /\.css$/,
        loader: 'style-loader!css-loader',
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loaders: [
          StringReplacePlugin.replace({
            replacements: [
              { pattern: /\%SERVER_ORIGIN\%/g, replacement: () => 'http://localhost:4000' },
              { pattern: /\&SOCKET_ORIGIN\%/g, replacement: () => 'ws://localhost:4000' },
              { pattern: /\%PACKAGE_SITE\%/g, replacement: () => process.env.PACKAGE_SITE },
              { pattern: /\%ENV\%/g, replacement: () => 'development' },
            ]
          }),
          {
            loader: 'elm-webpack-loader',
            options: {
              maxInstances: 1,
              forceWatch: true,
              cache: true,
              yes: true,
              debug: true,
              cwd: __dirname
            }
          }
        ]
      },
    ]
  },

  plugins: [
    new webpack.DefinePlugin({
      OPBEAT_APP_ID: JSON.stringify(process.env.OPBEAT_FRONTEND_APP_ID),
      OPBEAT_ORGANIZATION_ID: JSON.stringify(process.env.OPBEAT_ORGANIZATION_ID),
      'process.env.NODE_ENV': JSON.stringify('development')
    }),
  ],

  devServer: {
    inline: true,
    stats: { colors: true },
    historyApiFallback: true,
    port: '8080',
    allowedHosts: [
      'http://localhost:4000'
    ],
    headers: {
      'Access-Control-Allow-Origin': '*',
    },
    watchOptions: {
      aggregateTimeout: 300,
      poll: 1000
    },
  },
}
