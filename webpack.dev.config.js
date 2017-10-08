const path = require('path')
const webpack = require('webpack')
const StringReplacePlugin = require('string-replace-webpack-plugin')

const generatedElmCss = path.resolve(__dirname, 'client/elm-stuff/generated-code/rtfeldman/elm-css/output.css')

module.exports = {
  cache: true,
  target: 'web',

  externals: {
    'fs': '__fileSystem'
  },

  entry: {
    editor: ['es6-promise/auto', generatedElmCss, 'webpack-dev-server/client?http://localhost:8000/', path.join(__dirname, 'client/src/Pages/Editor/index.js')],
    embed: ['es6-promise/auto', generatedElmCss, 'webpack-dev-server/client?http://localhost:8000/', path.join(__dirname, 'client/src/Pages/Embed/index.js')]
  },

  output: {
    path: path.resolve(__dirname + '/dist'),
    publicPath: 'http://localhost:8000/',
    filename: '[name].js',
  },

  resolve: {
    alias: {
      'Make/0.18.0$': path.resolve(__dirname, 'make/0.18.0/build/Make0180.js')
    }
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
            presets: ['es2017'],
            plugins: ['syntax-dynamic-import']
          }
        }
      },
      {
        test: /\.css$/,
        loader: 'style-loader!css-loader',
      },
      { test: /\.json$/,
        loader: 'json-loader'
      },
      {
        test:    /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loaders:  [
          StringReplacePlugin.replace({
            replacements: [
              { pattern: /\%CDN_BASE\%/g, replacement: () => 'https://s3.us-east-2.amazonaws.com/development-cdn.ellie-app.com' },
              { pattern: /\%SERVER_ORIGIN\%/g, replacement: () => 'http://localhost:5000' },
              { pattern: /\%CARBON_ZONE_ID\%/g, replacement: () => process.env.CARBON_ZONE_ID },
              { pattern: /\%CARBON_SERVE\%/g, replacement: () => process.env.CARBON_SERVE },
              { pattern: /\%CARBON_PLACEMENT\%/g, replacement: () => process.env.CARBON_PLACEMENT },
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
              cwd: path.join(__dirname, 'client'),
              ignore: /generated-code/
            }
          }
        ]
      },
    ]
  },

  plugins: [
    new webpack.DefinePlugin({
      CDN_BASE: JSON.stringify('https://s3.us-east-2.amazonaws.com/development-cdn.ellie-app.com'),
      SERVER_ORIGIN: JSON.stringify('http://localhost:5000'),
      OPBEAT_APP_ID: JSON.stringify(process.env.OPBEAT_FRONTEND_APP_ID),
      OPBEAT_ORGANIZATION_ID: JSON.stringify(process.env.OPBEAT_ORGANIZATION_ID),
      'process.env.NODE_ENV': JSON.stringify('development')
    }),
    new StringReplacePlugin(),
  ],

  devServer: {
    inline: true,
    stats: { colors: true },
    historyApiFallback: true,
    port: '8000',
    allowedHosts: [
      'http://localhost:5000'
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
