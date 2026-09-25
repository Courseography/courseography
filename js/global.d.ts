// jQuery is injected as a global by webpack's ProvidePlugin (see webpack.common.js)
// rather than imported, so it needs an explicit ambient declaration here.
declare global {
  const $: JQueryStatic
  const jQuery: JQueryStatic
}

export {}
