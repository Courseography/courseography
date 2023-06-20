module.exports = {
  setupFiles: ["<rootDir>/js/setupTests.js"],
  snapshotSerializers: ["enzyme-to-json/serializer"],
  testPathIgnorePatterns: [
    "<rootDir>/node_modules/",
    "<rootDir>/cypress/",
    "<rootDir>/js/components/graph/__tests__/TestGraph.js",
    "<rootDir>/js/components/graph/__tests__/TestSidebar.js",
    "<rootDir>/js/components/graph/__tests__/TestContainer.js",
    "<rootDir>/js/components/graph/__tests__/TestFocusBar.js",
    "<rootDir>/js/components/graph/__tests__/cleanup-after-each.js",
  ],
  setupFilesAfterEnv: [
    "<rootDir>/js/setupTestsAfterEnv.js",
    "<rootDir>/js/components/graph/__tests__/cleanup-after-each.js",
  ],
  moduleFileExtensions: ["js", "jsx"],
  moduleDirectories: ["node_modules"],
  moduleNameMapper: {
    "\\.(css|less|sass|scss)$": "<rootDir>/js/components/graph/__mocks__/styleMock.js",
  },
  transform: {
    "\\.jsx?$": "babel-jest",
  },
  extensionsToTreatAsEsm: [".jsx"],
  testEnvironment: "jsdom",
}
