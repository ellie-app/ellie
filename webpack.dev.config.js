const path = require('path')
const webpack = require('webpack')
const StringReplacePlugin = require('string-replace-webpack-plugin')
const CopyPlugin = require('copy-webpack-plugin')

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
        test: /Editor\/Stylesheets\.elm$/,
        use: [
          'style-loader',
          'css-loader',
          {
            loader: 'elm-css-webpack-loader',
            options: {
              module: 'Pages.Editor.Stylesheets'
            }
          }
        ]
      },
      {
        test: /Embed\/Stylesheets\.elm$/,
        use: [
          'style-loader',
          'css-loader',
          {
            loader: 'elm-css-webpack-loader',
            options: {
              module: 'Pages.Embed.Stylesheets'
            }
          }
        ]
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
              { pattern: /\%CDN_BASE\%/g, replacement: () => 'https://s3.us-east-2.amazonaws.com/development-cdn.ellie-app.com' },
              { pattern: /\%SERVER_ORIGIN\%/g, replacement: () => 'http://localhost:5000' },
              { pattern: /\%CARBON_ZONE_ID\%/g, replacement: () => 'test' },
              { pattern: /\%CARBON_SERVE\%/g, replacement: () => 'test' },
              { pattern: /\%CARBON_PLACEMENT\%/g, replacement: () => 'test' },
              { pattern: /\%ENV\%/g, replacement: () => 'development' },
            ]
          }),
          `elm-webpack-loader?yes&debug&cwd=${path.join(__dirname, 'client')}`,
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
      CDN_BASE: JSON.stringify('https://s3.us-east-2.amazonaws.com/development-cdn.ellie-app.com'),
      SERVER_ORIGIN: JSON.stringify('http://localhost:5000'),
      OPBEAT_APP_ID: JSON.stringify(process.env.OPBEAT_FRONTEND_APP_ID),
      OPBEAT_ORGANIZATION_ID: JSON.stringify(process.env.OPBEAT_ORGANIZATION_ID),
      'process.env.NODE_ENV': JSON.stringify('development')
    }),
    new StringReplacePlugin(),
    new CopyPlugin([
      { from: path.join(__dirname, 'images'), to: 'images', toType: 'dir' }
    ], {
      copyUnmodified: true
    })
  ],

  devServer: {
    inline: true,
    stats: { colors: true },
    historyApiFallback: true,
    port: '8000',
    watchOptions: {
      aggregateTimeout: 300,
      poll: 1000
    },
  },
}
