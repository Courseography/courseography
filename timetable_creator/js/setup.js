/*jslint todo: true */
/*global $, console, jQuery*/
/*jslint browser:true */
/*jslint plusplus: true */
"use strict";
var result;
var i;
var contentString = "";
var courseSelect;
var xmlhttp;
var csvSplitNewline;
var splitLine;
var isACourse;
var notYetLogged;
var header;
var sections;
var entry;
var courses;
var searchList;

$(document).ready(function () {
    $("#dialog").fadeOut();
    courseSelect = document.getElementById("course-select");
    searchList = document.getElementById("search-list");
    restoreFromCookies();
    createTimetableSearch();
    courses = getVeryLargeCourseArray();
    trapScroll();
});

function getVeryLargeCourseArray() {
    var httpResponse;
    var splitArray;
    if (window.XMLHttpRequest) {
        xmlhttp = new XMLHttpRequest();
    } else {
        xmlhttp = new window.ActiveXObject("Microsoft.XMLHTTP");
    }
    xmlhttp.open("GET", "../../timetable_creator/js/courses.out", false);
    xmlhttp.send();
    httpResponse = xmlhttp.responseText;
    splitArray = httpResponse.split("\n");
    return splitArray;
}

function setupEntry(courseObject) {
    entry = document.createElement("li");
    entry.id = courseObject.name + "-li";
    header = document.createElement("h3");
    header.appendChild(document.createTextNode(courseObject.name));
    courseObject.header = header;
    sections = processSession(courseObject);
    entry.appendChild(header);
    $(sections).css("height", "100%");
    $(sections).css("width", "100%");
    entry.appendChild(sections);
    $(entry).accordion({heightStyle: "content", collapsible: true, active: false/*, event: "click hoverintent"*/});
    courseSelect.appendChild(entry);
}

function getCourse(courseCode) {
    $.ajax({
        url: "../../res/courses/" + courseCode,
        dataType: "json",
        async: false,
        success: function (data) {
            result = data;
        }
    });
    console.log("Getting course " + result + "from file ../../res/courses/" + courseCode);
    return result;
}

function addCourseToList(course) {
    var courseObject = getCourse(course);
    courseObject.selectedSession = null;
    courseObject.selected = false;
    courseObject.isLectureSelected = false;
    courseObject.isTutorialSelected = false;
    if (courseObject.manualTutorialEnrolment) {
        courseObject.satisfied = false;
    } else {
        courseObject.satisfied = true;
    }
    setupEntry(courseObject);
}

