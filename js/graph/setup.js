// Globals
var timetable = 'res/timetable.html';
var clickedCourses = [];
var FCEs = 0;
var FCEs100 = 0;
var FCEs200 = 0;
var FCEs300 = 0;
var FCEs400 = 0;
var FCEsMAT = 0;

// Track active courses
var active400s = [];
var active300s = [];
var active200s = [];
var projectCourses = []; // CSC49x

// Specialist requirements
var numBCB = 0;
var cscReqTotal = 0;
var matReqTotal = 0;
var elecTotal = 0;
var postTotal = 0;
var cscReqSat = false;
var matReqSat = false;
var elec400sSat = false;
var elecSat = false;
var peySat = false;

// Major requirements
var cscReqSatMajor = false;
var matReqSatMajor = false;
var elecSatMajor = false;
var peySatMajor = false;
var numBCBMajor = 0;
var cscReqTotalMajor = 0;
var matReqTotalMajor = 0;
var elec200sTotalMajor = 0;
var elec300sTotalMajor = 0;
var elecTotalMajor = 0;
var postTotalMajor = 0;

var cscReqSatMinor = false;
var elecSatMinor = false;
var cscReqTotalMinor = 0;
var elecTotalMinor = 0;
var postTotalMinor = 0;

var extraMajor = 0;
var extraMinor = 0;

var activeFocus = '';

var timeouts = [];

var FCEPrerequisiteCourses;

var courseCache = [];

$(document).ready(function () {
    'use strict';
    var graphPage = getRemote();

    $("#graph").append(graphPage);

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

    // Uncomment to enable the feedback form (must also be displayed in html)
    // activateFeedbackForm();
    // Uncomment to enable graph dragging
    // enableGraphDragging();
});

function getRemote() {
    return $.ajax({
        type: "GET",
        url: 'static/hs/csc_graph.svg',
        async: false
    }).responseText;
}