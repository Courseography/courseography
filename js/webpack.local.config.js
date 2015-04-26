'use strict';

var path = require('path');
var webpack = require('webpack');

/**
 * This is the Webpack configuration file for local development. It contains
 * local-specific configuration such as eval sourcemaps, as well as:
 *
 * - The entry point of the application
 * - Where the output file should be
 * - Which loaders to use on what files to properly transpile the source
 *
 * For more information, see: http://webpack.github.io/docs/configuration.html
 */
module.exports = {

  // Resolve file names relative to js/
  context: __dirname,

  // Efficiently evaluate modules with source maps
  devtool: 'source-map',

  // Set entry points for individual pages
  entry: {
    graph: './pages/graph'
  },

  // Output to public/js/pages/[name].bundle.js
  output: {
    path: path.join(__dirname, '../public/js/pages'),
    filename: '[name].bundle.js'
  },

  // Transform source code using Babel
  module: {
    loaders: [
      { test: /\.jsx?$/, exclude: /node_modules/, loader: 'babel-loader'}
    ]
  },

  // Automatically transform files with these extensions
  resolve: {
    extensions: ['', '.js', '.jsx']
  },

  // Watch files and re-bundle upon changes
  watch: true
}
