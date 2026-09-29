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
        // eslint-plugin-react's "detect" option calls the removed
        // context.getFilename() API and crashes under ESLint 10
        // (https://github.com/jsx-eslint/eslint-plugin-react/issues/3977).
        // Hardcode the version from package.json until upstream ships a fix.
        version: require("./package.json").dependencies.react,
      },
    },
  },
  {
    // @babel/eslint-parser converts TypeScript syntax to an ESTree-shaped
    // AST that core rules don't fully understand:
    //  - `no-undef` flags type-only identifiers as undefined.
    //  - `no-unused-vars` misses identifiers used only in a type position
    //    (e.g. `const x: SomeType[] = ...`), reporting their import as unused.
    // TypeScript's own compiler (`pnpm run typecheck`, with `noUnusedLocals`/
    // `noUnusedParameters` enabled in tsconfig.json) already catches both of
    // these correctly since it fully understands type positions, so disable
    // both rules here — this is the same guidance typescript-eslint gives
    // for `.ts`/`.tsx` files.
    files: ["**/*.ts", "**/*.tsx"],
    rules: {
      "no-undef": "off",
      "no-unused-vars": "off",
    },
  },
  {
    // Ambient declaration files declare symbols for consumers elsewhere in
    // the program, not for use within the file itself.
    files: ["**/*.d.ts"],
    rules: {
      "no-unused-vars": "off",
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
