const path = require("path");
const webpack = require("webpack");
const StringReplacePlugin = require("string-replace-webpack-plugin");
const CopyPlugin = require("copy-webpack-plugin");

module.exports = {
  cache: true,
  target: "web",
  devtool: "inline-source-map",
  context: __dirname,
  entry: {
    editor: [
      "webpack-dev-server/client?http://localhost:8080/",
      path.join(__dirname, "src/Pages/Editor/index.js")
    ],
    "editor-sw": [path.join(__dirname, "src/Pages/Editor/ServiceWorker.js")],
    embed: [
      "webpack-dev-server/client?http://localhost:8080/",
      path.join(__dirname, "src/Pages/Embed/index.js")
    ]
  },

  output: {
    path: path.resolve(__dirname, "../priv/static"),
    publicPath: "http://localhost:8080/assets/",
    filename: "[name].js"
  },

  module: {
    rules: [
      {
        test: /\.svg$/,
        use: {
          loader: "svg-inline-loader"
        }
      },
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: "babel-loader",
          options: {
            presets: [
              "@babel/preset-env",
              ["@babel/preset-stage-2", { decoratorsLegacy: true }]
            ]
          }
        }
      },
      {
        test: /\.css$/,
        loader: "style-loader!css-loader"
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loaders: [
          StringReplacePlugin.replace({
            replacements: [
              {
                pattern: /'\%HOSTNAME\%'/g,
                replacement: () => "window.location.host"
              },
              {
                pattern: /'\%PROTOCOL\%'/g,
                replacement: () => "window.location.protocol"
              },
              {
                pattern: /\%SERVER_ORIGIN\%/g,
                replacement: () => "http://localhost:4000"
              },
              {
                pattern: /\%SOCKET_ORIGIN\%/g,
                replacement: () => "ws://localhost:4000"
              },
              {
                pattern: /\%PACKAGE_SITE\%/g,
                replacement: () => "https://package.elm-lang.org"
              },
              { pattern: /\%ENV\%/g, replacement: () => "development" }
            ]
          }),
          {
            loader: "elm-webpack-loader",
            options: {
              cwd: __dirname,
              runtimeOptions: "-A128m -H128m -n8m",
              debug: true
            }
          }
        ]
      }
    ]
  },

  plugins: [
    new webpack.DefinePlugin({
      OPBEAT_APP_ID: JSON.stringify(process.env.OPBEAT_FRONTEND_APP_ID),
      OPBEAT_ORGANIZATION_ID: JSON.stringify(
        process.env.OPBEAT_ORGANIZATION_ID
      ),
      "process.env.NODE_ENV": JSON.stringify("development")
    }),
    new CopyPlugin(["images/*"])
  ],

  devServer: {
    inline: true,
    stats: { colors: true },
    historyApiFallback: true,
    overlay: true,
    port: "8080",
    host: "0.0.0.0",
    allowedHosts: ["http://localhost:4000"],
    headers: {
      "Access-Control-Allow-Origin": "*",
      "Service-Worker-Allowed": "/"
    },
    watchOptions: {
      aggregateTimeout: 300,
      poll: 1000
    }
  }
};
