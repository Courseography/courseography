# Changelog

## Unreleased

### 🚨 Breaking changes

### ✨ New features/enhancements

- Added text warning when there's no timetable information available to display

### 🐛 Bug fixes

- Fix React state mutation
- Fixed timetable parsing issue, querying page numbers rather than course codes
- Fixed bug to count FCE correctly
- Fixed parsing of graph transformations

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
- Refactored the class components in `/grid` folder to function components
- Update stylelint integration with development environment and fix existing stylelint violations
- Use `magick` command instead of `convert` when serving graph images
- Restructured code base for coverage reports
- Reorganized `.cabal` file

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
