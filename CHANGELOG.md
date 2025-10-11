# Changelog

## Unreleased

### 🚨 Breaking changes

### ✨ New features/enhancements

- Updated the Zoom In/Zoom Out buttons on the graph page to Awesome Icons from +/-
- Added the React ErrorBoundary library and implemented an ErrorBoundary for the Graph component in the Container component and `js/components/draw/main.js`
- Updated the export button on the graph and grid pages to Awesome Icons; also added highlight effect and tooltip popup on hover
- Added an autocomplete feature to the search bar `js/components/generate/GenerateForm`, also rewrote tests related to this feature
- Added a route to access all POST codes stored in the database with test coverage
- Added the `js/components/generate/AutocompleteDropdown.js` component to the program field of Generate
- Updated the colour of the rendered options for the AutocompleteDropdown component

### 🐛 Bug fixes

- Fixed a bug that was causing the focus info popup to appear blank
- Fixed a bug on the generate page causing extraneous ellipses to appear when hovering over a course to highlight its prerequisites
- Fixed a bug on the generate page where an extraneous info popup would appear when hovering over the top left corner of the graph viewing window
- Fixed a bug that led code to crash when parsing all pre-generated graphs from svg (i.e., program graphs)
- Fixed a bug where redundant boolean nodes were being generated

### 🔧 Internal changes

- Upgraded Jest to v30
- Upgraded `jest-environment-jsdom` to v30
- Refactored navigation bar into a React component (for the graph, grid and generate pages only - the about page navigation bar is still rendered using Blaze)
- Refactored backend tests to use `tasty` and `tasty-hunit` instead of `HUnit`
- Refactored course insertion and query functions to align with MVC architectural principles
- Renamed functions that query the database for post/program information from post -> program
- Modified CircleCI configuration so that Jest tests now emit coverage reports which are then uploaded to Coveralls
- Refactored export modal behaviour such that it is handled entirely through React state, rather than through DOM manipulation with vanilla JS
- Added test cases for the `ExportModal` component in `js/components/common`
- Updated backend tests to use `tasty-discover`
- Added documentation for running a subset of the backend tests
- Deleted `app/Response/Image` file and refactored `app/Util/Helpers` to include `returnImageData`
- Added test cases for the retrieveProgram function in `Controllers/Program`

## [0.7.1] - 2025-06-16

### ✨ New features/enhancements

- Updated configuration for 2025-26

## [0.7.0] - 2025-06-15

### ✨ New features/enhancements

- Added text warning when there's no timetable information available to display
- Updated course info modal to display field labels if and only if they are non-empty
- Ensured corequisites and exclusions for courses are displayed in the course info modal, if available
- Added text warnings and validations for each field in `GenerateForm`
- Redesigned generate form UI
- Allow option to log server request accesses to a file
- Added search by program to create graphs on Generate Page
- Updated Computer Science graph for 2025-26

### 🐛 Bug fixes

- Fix React state mutation
- Fixed timetable parsing issue, querying page numbers rather than course codes
- Fixed bug to count FCE correctly
- Fixed parsing of graph transformations
- Fixed choppiness of the graphs generated in the export modal
- Fixed matrix transformation and intersection checking bug when sending graph data to front end
- Fixed inverse matrix storage from column-major to row-major order
- Fixed the issue with transformation matricies not being applied to SVG attributes when generating PDFs in the export modal
- Fixed bug where region label transforms were not correctly applied

### 🔧 Internal changes

- Upgraded to React v18
- Upgraded the Testing Library from 12.0 to 16.0
- Replace `react-test-renderer` (deprecated) for snapshot testing
- Use `user-event` for user interactions (instead of `fireEvent`)
- Add `ag-grid` group to dependabot configuration
- Refactored `strictRoutes` by removing query parameters into functions for symmetry
- Refactored graph and generation related route functionality into controllers
- Moved the routes related to downloads to the `Timetable` controller to be triggered
- Refactored the HTTPS response generation from `App/Database/CourseQueries` to the controller `App/Controllers/`
- Moved config values from .hs file to .yaml file
- Changed representation from translation to matrix in `App/Svg/Parser`
- Optimized about page using Webpack and new dependencies `html-loader` and `markdown-loader`
- Removed `privacy` route and codes related to it
- Improved general testing infrastructure
- Added test cases for the index function in `Controllers/Course`
- Added test cases for the depts function in `Controllers/Course`
- Added test cases for the courseInfo function in `Controllers/Course`
- Removed unused components `Search` and `Post`, backend routes/functions used exclusively by these components, and references to the components in the webpack files
- Refactored the class components in `/grid` folder to function components
- Update stylelint integration with development environment and fix existing stylelint violations
- Use `magick` command instead of `convert` when serving graph images
- Restructured code base for coverage reports
- Reorganized `.cabal` file
- Added coverage report to test suite
- Added test cases for the retrieveCourse function in `Controllers/Course`
- Fixed CSS issues: modal padding, "and/or" centering, and FCE Count arrow alignment
- ⁠Fixed issue with "Info" popup disappearing when hovering over it and reduced the delay before it disappears
- ⁠Fixed issue where scrolling in the course modal triggered graph zoom
- Added dependency `formik` and replaced standard React forms with Formik in `GenerateForm`
- Added parsers for all SVG transformations in `App/Svg/Parser`
- Added intersection tests for buildRect, buildEllipse, intersectsWithPoint and buildPath
- Allowed lowercase courses and departments on the Generate page
- Added Debugging Guide
- Added Haskell Language Server (HLS) Guide
- Upgraded to Stackage lts-23.18
- Added `stylish-haskell` dependency and ran it on `.hs` source files
- Upgraded to React v19
- Added dependabot group for `react` and `react-dom`
- Switched from Yarn PnP to node-modules to fix issues running tests with Node v20+
- Switched connection for timetable parsing from `RequireEMS` to `AllowEMS`
- Fixed dependency caching in CircleCI configuration
- Added dependency groups to Dependabot configuration
- Added tests for the index function in `Controllers/Graph`
- Added `Models` folder and a new `Course.hs` module within it
- Added the `Graph.hs` module in the `App/Models`
- Updated eslint configuration for eslint v9
- Removed calls to `head` and `tail`

## [0.6.0] - 2024-06-24

### 🚨 Breaking changes

- Update Computer Science graph for 2024
- Update course information for 2024
- Do not display room numbers in timetable information

### ✨ New features/enhancements

- Added button to deselect courses in "FCE count" side panel
- Increase font size for hybrid nodes in graphs
- Added warning message modal for when the user enters invalid courses in Generate
- Added spotlight for highlighting dependencies (when hovering over a course)

### 🐛 Bug fixes

- Fixed bug that causes FCE count to increase when toggling course via sidebar
- Fixed bug that forced users to input a course codes in a specific way when generating a dependency graph
- Correctly handle `'l'` directives in svg path attributes
- Fixed bug that prevented timetable information from loading in Grid page
- Fixed bug that prevented the course info modal from displaying in Grid page

### 🔧 Internal changes

- Started a changelog.
- Updated pull request template
- Refactored graph and course related route functionality into controllers

## [0.5.0] - 2023-08-13

See [commit history](https://github.com/Courseography/courseography/commits/master/) for changes at this release and earlier.
