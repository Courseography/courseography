const path = require("path")
const MiniCssExtractPlugin = require("mini-css-extract-plugin")
const webpack = require("webpack")

const presets = [
  ["@babel/preset-env", { useBuiltIns: "usage", corejs: 3 }],
  ["@babel/preset-react"],
]

module.exports = {
  entry: {
    "js/search/app": "./js/components/search/search.js.jsx",
    "js/grid/app": "./js/components/grid/grid.js.jsx",
    "js/graph/app": "./js/components/graph/main.js",
    "js/post/app": "./js/components/post/post.js.jsx",
    "js/draw/app": "./js/components/draw/main.js",
    "js/generate/app": "./js/components/generate/generate.js",
    "style/app": "./style/app.js",
  },
  output: {
    path: path.resolve(__dirname, "public"),
    publicPath: "./public",
    filename: "[name].js",
  },
  module: {
    rules: [
      {
        test: /\.js$/,
        use: {
          loader: "babel-loader",
          options: { presets },
        },
        exclude: /node_modules/,
      },
      {
        test: /\.jsx$/,
        use: {
          loader: "babel-loader",
          options: { presets },
        },
        exclude: /node_modules/,
      },
      {
        test: /\.css$/i,
        use: [MiniCssExtractPlugin.loader, "css-loader"],
      },
      {
        test: /\.s[ac]ss$/i,
        use: [MiniCssExtractPlugin.loader, "css-loader", "sass-loader"],
      },
    ],
  },
  plugins: [
    new MiniCssExtractPlugin({
      // Options similar to the same options in webpackOptions.output
      // both options are optional
      filename: "[name].css",
      chunkFilename: "[id].css",
    }),
    new webpack.ProvidePlugin({
      $: "jquery",
      jQuery: "jquery",
    }),
  ],
  mode: "development",
}
