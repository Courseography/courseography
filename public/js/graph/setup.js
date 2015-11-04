
// Globals
var timetable = 'static/res/timetable.html';
var clickedCourses = [];

// FCEs for current graph
var currentFCEs = 0; 
var currentFCEs100 = 0;
var currentFCEs200 = 0;
var currentFCEs300 = 0;
var currentFCEs400 = 0;
var currentFCEsMAT = 0;

// Combined FCEs selected for all graphs
var totalFCEs = 0;

var activeFocus = '';         // The active focus from the 'Focuses' tab.
var timeouts = [];            // All timeouts. Used to remove timeouts later on
var FCEPrerequisiteCourses;   // Courses that have FCE based prerequisites,
                              // initialised at runtime

var nodes = [];               // List of all nodes

var graphs = [];            // List of graphs (jsons) that are parsed in the database

/**
 * Retrieves an SVG file.
 * @param {string} filepath The relative filepath of the graph
 * @returns {string} An SVG string representing a graph. 
 */
function getRemote(filepath) {
    'use strict';

    var SVG = $.ajax({
        type: 'GET',
        url: filepath,
        async: false
    }).responseText;
    $('#graph').append(SVG);
}
