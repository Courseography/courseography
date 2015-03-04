
// Globals
var timetable = 'static/res/timetable.html';
var clickedCourses = [];
var FCEs = 0;
var FCEs100 = 0;
var FCEs200 = 0;
var FCEs300 = 0;
var FCEs400 = 0;
var FCEsMAT = 0;

// Track active courses
var active400s = [];          // Active 400 series courses
var active300s = [];          // Active 300 series courses
var active200s = [];          // Active 200 series courses
var projectCourses = [];      // Active project courses (CSC49*)

// Specialist requirements
var numBCB = 0;               // Number of satisfied BCB courses
var cscReqTotal = 0;          // Total number of satisfied fields for
                              // the CSC specialist POSt
var matReqTotal = 0;          // Total number of satisfied MAT courses for
                              // the CSC specialist POSt
var elecTotal = 0;            // Total number of satisfied electives for
                              // the CSC specialist POSt
var postTotal = 0;
var cscReqSat = false;        // CSC requirements satisfied for specialist POSt
var matReqSat = false;        // MAT requirements satisfied for specialist POSt
var elec400sSat = false;      // 400 series course electives satisfied
var elecSat = false;          // Elective requirement satisfied for
                              // specialist POSt
var peySat = false;           // PEY satisfied for specialist POSt

// Major requirements
var cscReqSatMajor = false;   // CSC requirements satisfied for major POSt
var matReqSatMajor = false;   // MAT requirements satisfied for the CSC
                              // major POSt
var elecSatMajor = false;     // Elective requirement satisfied for the CSC
                              // major POSt
var peySatMajor = false;      // PEY satisfied for major POSt
var numBCBMajor = 0;          // Number of satisfied BCB courses for the CSC
                              // major POSt
var cscReqTotalMajor = 0;     // Total number of satisfied fields for the CSC
                              // major POSt
var matReqTotalMajor = 0;     // Total number of satisfied MAT courses for
                              // the CSC major POSt
var elec200sTotalMajor = 0;   // Total number of satisfied 200 series course
                              // electives for the CSC major POSt
var elec300sTotalMajor = 0;   // Total number of satisfied 300 series course
                              // electives for the CSC major POSt
var elecTotalMajor = 0;       // Total number of satisfied electives for
// the CSC major POSt
var postTotalMajor = 0;

// Minor requirements
var cscReqSatMinor = false;   // CSC requirements satisfied for minor POSt
var elecSatMinor = false;     // Elective requirement satisfied  for
                              // the CSC minor POSt
var cscReqTotalMinor = 0;     // Total number of satisfied fields for
                              // the CSC minor POSt
var elecTotalMinor = 0;       // Total number of satisfied electives
                              // for the CSC minor POSt
var postTotalMinor = 0;       // Total number of satisfied fields for
                              // the CSC minor POSt

var extraMajor = 0;           // Number of satisfied courses that are required
                              // for specialist, but are electives for major
var extraMinor = 0;           // Number of satisfied courses that are required
                              // for specialist, but are electives for minor

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

    FCEPrerequisiteCourses = [CSC318, CSC454];

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
        url: 'static/hs/Testfile.svg',
        async: false
    }).responseText;
    $('#graph').append(SVG);
}
