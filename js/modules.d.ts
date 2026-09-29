// Markdown files are imported as pre-rendered HTML strings via markdown-loader
// (see webpack.common.js).
//
// This must stay its own ambient script file (no top-level import/export) —
// TypeScript 7.0.2's project-mode compiler fails to register a wildcard
// `declare module` pattern when it lives in the same file as a
// `declare global` block, since that block's `export {}` (required to make
// `declare global` valid) turns the whole file into a module.
declare module "*.md" {
  const content: string
  export default content
}
