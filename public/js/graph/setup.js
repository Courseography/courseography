
// Globals
var timetable = 'static/res/timetable.html';
var clickedCourses = [];
var FCEs = 0;
var FCEs100 = 0;
var FCEs200 = 0;
var FCEs300 = 0;
var FCEs400 = 0;
var FCEsMAT = 0;

var activeFocus = '';         // The active focus from the 'Focuses' tab.
var timeouts = [];            // All timeouts. Used to remove timeouts later on
var FCEPrerequisiteCourses;   // Courses that have FCE based prerequisites,
                              // initialised at runtime
var courseCache = [];         // Cached Courses. Courses are cached to
                              // minimise AJAX requests

var nodes = [];               // List of all nodes

$(document).ready(function () {
    'use strict';

    loadGraph('graph-csc', 1);
    $('#fcecount').hide();
});

/**
 * Retrieves an SVG file.
 * @param {string} The relative filepath of the graph
 * @returns {string} An SVG string representing a graph. 
 */
function getRemote(filepath) {
    'use strict';

    var SVG = $.ajax({
        type: 'GET',
        url: 'static/res/graphs/' + filepath,
        async: false
    }).responseText;
    $('#graph').append(SVG);
}
