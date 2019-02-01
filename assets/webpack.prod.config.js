const path = require("path");
const webpack = require("webpack");
const StringReplacePlugin = require("string-replace-webpack-plugin");
const ManifestPlugin = require("webpack-manifest-plugin");
const CompressionPlugin = require("compression-webpack-plugin");
const CopyPlugin = require("copy-webpack-plugin");
const TerserPlugin = require("terser-webpack-plugin");

module.exports = {
  cache: true,

  entry: {
    editor: path.join(__dirname, "src/Pages/Editor/index.js"),
    embed: path.join(__dirname, "src/Pages/Embed/index.js")
  },

  output: {
    path: path.resolve(__dirname + "/../priv/static"),
    filename: "[name]-[chunkhash].js",
    chunkFilename: "chunk.[name]-[chunkhash].js",
    publicPath: "/assets/"
  },

  module: {
    rules: [
      {
        test: /\.svg$/,
        use: { loader: "svg-inline-loader" }
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
                replacement: () => "https://" + process.env.SERVER_HOST
              },
              {
                pattern: /\%SOCKET_ORIGIN\%/g,
                replacement: () => "wss://" + process.env.SERVER_HOST
              },
              { pattern: /\%ENV\%/g, replacement: () => process.env.NODE_ENV },
              {
                pattern: /\%PACKAGE_SITE\%/g,
                replacement: () => "https://package.elm-lang.org"
              }
            ]
          }),
          {
            loader: "elm-webpack-loader",
            options: {
              cwd: __dirname,
              runtimeOptions: "-A128m -H128m -n8m",
              optimize: true
            }
          }
        ]
      }
    ]
  },

  optimization: {
    minimizer: [
      new TerserPlugin({
        cache: true,
        parallel: true,
        terserOptions: {
          mangle: false,
          compress: {
            pure_funcs: [
              "F2",
              "F3",
              "F4",
              "F5",
              "F6",
              "F7",
              "F8",
              "F9",
              "A2",
              "A3",
              "A4",
              "A5",
              "A6",
              "A7",
              "A8",
              "A9"
            ],
            pure_getters: true,
            keep_fargs: false,
            unsafe_comps: true,
            unsafe: true
          }
        }
      }),
      new TerserPlugin({
        cache: true,
        parallel: true,
        terserOptions: { mangle: true }
      })
    ]
  },

  plugins: [
    new webpack.DefinePlugin({
      OPBEAT_APP_ID: JSON.stringify(process.env.OPBEAT_FRONTEND_APP_ID),
      OPBEAT_ORGANIZATION_ID: JSON.stringify(
        process.env.OPBEAT_ORGANIZATION_ID
      ),
      "process.env.NODE_ENV": JSON.stringify(process.env.NODE_ENV)
    }),
    new StringReplacePlugin(),
    new ManifestPlugin({
      generate: (_, files) => {
        return {
          version: 1,
          latest: files.reduce((memo, next) => {
            if (next.isInitial) {
              memo["assets/" + next.name] = next.path.substr(1);
            }
            return memo;
          }, {})
        };
      }
    }),
    new CompressionPlugin({
      test: /\.js$/
    }),
    new CopyPlugin(["images/*"])
  ]
};
