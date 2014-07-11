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
    $("#dialog").css("visibility", "visible");
    $("td").each(function() {
        $(this).attr("in-conflict", "false");
        $(this).attr("satisfied", "true");
    });
    courseSelect = document.getElementById("course-select");
    searchList = document.getElementById("search-list");
    appendClearAllButton();
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
    xmlhttp.open("GET", "js/timetable/courses.txt", false);
    xmlhttp.send();
    httpResponse = xmlhttp.responseText;
    splitArray = httpResponse.split("\n");
    return splitArray;
}

function setupEntry(courseObject) {
    entry = document.createElement("li");
    var courseImg = document.createElement("img");
    $(courseImg).attr("src", "res/ico/close.ico");
    $(courseImg).addClass("close-icon");

    $(courseImg).click(function() {
        removeCourseFromList(courseObject.name);
    });

    entry.id = courseObject.name + "-li";
    header = document.createElement("h3");
    $(header).css("height", "100%");
    $(header).css("width", "100%");
    header.appendChild(courseImg);
    header.appendChild(document.createTextNode(courseObject.name));
    courseObject.header = header;
    sections = processSession(courseObject);
    entry.appendChild(header);
    entry.appendChild(sections);
    $(entry).accordion({heightStyle: "content", collapsible: true, active: false/*, event: "click hoverintent"*/});
    courseSelect.appendChild(entry);
}

function getCourse(courseCode) {
    $.ajax({
        url: "res/courses/" + courseCode,
        dataType: "json",
        async: false,
        success: function (data) {
            result = data;
        }
    });
    console.log("Getting course " + result + "from file ../../res/courses/" + courseCode);
    courseObjects.push(result);
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

function appendClearAllButton() {
    var clearAllItem = document.getElementById("clear-all");
    $(clearAllItem).click(function() {
        if (confirm("Clear all selected courses?")) {
            $.each(courseObjects, function() {
                removeCourseFromList(courseObjects[0].name);
            }); 
        }
    });
}
