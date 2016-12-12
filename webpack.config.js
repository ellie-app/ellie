var path = require("path");
var DashboardPlugin = require('webpack-dashboard/plugin');

module.exports = {
  context: path.join(__dirname, 'src'),

  entry: {
    app: [
      './js/Main.js'
    ]
  },

  output: {
    path: path.resolve(__dirname + '/dist'),
    filename: '[name].js',
  },

  module: {
    loaders: [
      {
        test: /\.css$/,
        loader: 'style!css',
      },
      {
        test: /Stylesheets\.elm$/,
        loader: 'style!css!elm-css-webpack',
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
        loader:  'elm-webpack',
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
    new DashboardPlugin()
  ],

  devServer: {
    inline: true,
    stats: { colors: true },
  },
};
