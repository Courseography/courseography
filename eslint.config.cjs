const { includeIgnoreFile } = require("@eslint/compat")
const path = require("node:path")
const globals = require("globals")
const js = require("@eslint/js")
const reactPlugin = require("eslint-plugin-react")
const eslintPluginPrettierRecommended = require("eslint-plugin-prettier/recommended")
const babelParser = require("@babel/eslint-parser")

const ignorePath = path.resolve(__dirname, ".prettierignore")

module.exports = [
  includeIgnoreFile(ignorePath),
  js.configs.recommended,
  reactPlugin.configs.flat.recommended,
  eslintPluginPrettierRecommended,
  {
    languageOptions: {
      globals: {
        ...globals.browser,
        ...globals.node,
        ...globals.jest,
        ...globals.es2015,
        $: "readonly",
        getURLParameter: "readonly",
      },
      parser: babelParser,
    },
    rules: {
      "no-console": "off",
      "react/no-find-dom-node": "off",
      "react/no-render-return-value": "off",
      "react/no-string-refs": "off",
      "react/react-in-jsx-scope": "off",
    },
    settings: {
      react: {
        version: "detect",
      },
    },
  },
  {
    files: ["cypress/**/*.js"],
    languageOptions: {
      globals: {
        cy: "readonly",
      },
    },
  },
  {
    files: ["tests/**/*.js"],
    languageOptions: {
      globals: {
        QUnit: "readonly",
        setCookie: "readonly",
        getCookie: "readonly",
      },
    },
  },
]
