
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

    getRemote();

    buildGraph();

    FCEPrerequisiteCourses = [csc318, csc454];

    // Set width of FCE count
    var w = $('.infoTabs').width() - $('.tabList').outerWidth() - 1;
    $('#FCECountDiv').width(w + 'px');

    // Create tabs
    createTabs();

    // Set mouse callbacks
    setMouseCallbacks();

    // Initialize interface
    initializeGraphSettings();
    
    // Update credit count in nav bar
    updateNavGraph();

    // Uncomment to enable the feedback form (must also be displayed in html)
    // activateFeedbackForm();
    // Uncomment to enable graph dragging
    // enableGraphDragging();
});


/**
 * Retrieves an SVG file.
 * @returns {string} An SVG string representing a graph. 
 */
function getRemote() {
    'use strict';

    var SVG = $.ajax({
        type: 'GET',
        url: 'static/res/graphs/CSC/csc_graph.svg',
        async: false
    }).responseText;
    $('#graph').append(SVG);
}
