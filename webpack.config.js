const path = require('path');

module.exports = {
  entry: {
    'js/search/timetable': './js/timetable.js.jsx'
  },
  output: {
    path: path.resolve(__dirname, 'public'),
    publicPath: './public',
    filename: '[name].js'
  },
  module: {
    loaders: [
      { test: /\.js$/, loader: 'babel-loader', exclude: /node_modules/ },
      { test: /\.jsx$/, loader: 'babel-loader', exclude: /node_modules/ }
    ]
  }
};
