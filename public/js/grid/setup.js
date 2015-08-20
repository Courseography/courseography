// All Global Variables for the Grid
//TODO: Are all of these used?
var courses = [];           //
var courseCache = [];       // All Courses that have been previously requested.
var selectedCourses = [];   // All selected Courses.
var selectedSections = [];  // All selected sections.
var courseObjects = [];     // All selected course JSON files.

/**
 * Renders the button that allows the user to clear
 * all selected courses.
 */
function renderClearAllButton() {
    'use strict';

    var clearAllItem = document.getElementById('clear-all');
    $(clearAllItem).click(function () {
        if (confirm('Clear all selected courses?')) {
            $.each(courseObjects.slice(0), function (i, course) {
                deselectCourse(course.name);
            });
        }
    });
}
