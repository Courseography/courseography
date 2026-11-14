const { defineConfig } = require("cypress")

module.exports = defineConfig({
  allowCypressEnv: false,
  e2e: {
    baseUrl: "http://localhost:8000",
    specPattern: "cypress/integration/**/*_spec.js",
    excludeSpecPattern: "**/examples/*.js",
  },
})
