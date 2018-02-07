require('dotenv').config({ path: '../.env' })
const path = require('path')
const webpack = require('webpack')
const StringReplacePlugin = require('string-replace-webpack-plugin')

module.exports = {
  cache: true,
  target: 'web',

  externals: {
    'fs': '__fileSystem'
  },

  entry: {
    editor: [
      'es6-promise/auto',
      'webpack-dev-server/client?http://localhost:1338/',
      path.join(__dirname, 'src/Pages/Editor/index.js')
    ],
    // embed: [
    //   'es6-promise/auto',
    //   'webpack-dev-server/client?http://localhost:1338/',
    //   path.join(__dirname, 'src/Pages/Embed/index.js')
    // ]
  },

  output: {
    path: path.resolve(__dirname + '/dist'),
    publicPath: 'http://localhost:1338/',
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
        test:    /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loaders:  [
          StringReplacePlugin.replace({
            replacements: [
              { pattern: /\%CDN_BASE\%/g, replacement: () => 'https://s3.us-east-2.amazonaws.com/development-cdn.ellie-app.com' },
              { pattern: /\%SERVER_ORIGIN\%/g, replacement: () => 'http://localhost:1337' },
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
      CDN_BASE: JSON.stringify('https://s3.us-east-2.amazonaws.com/development-cdn.ellie-app.com'),
      SERVER_ORIGIN: JSON.stringify('http://localhost:1338'),
      OPBEAT_APP_ID: JSON.stringify(process.env.OPBEAT_FRONTEND_APP_ID),
      OPBEAT_ORGANIZATION_ID: JSON.stringify(process.env.OPBEAT_ORGANIZATION_ID),
      'process.env.NODE_ENV': JSON.stringify('development')
    }),
  ],

  devServer: {
    inline: true,
    stats: { colors: true },
    historyApiFallback: true,
    port: '1338',
    allowedHosts: [
      'http://localhost:1337'
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
