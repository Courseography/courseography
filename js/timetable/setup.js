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
    restoreFromCookies();
    enableSearch();
    courses = getVeryLargeCourseArray();
    trapScroll();
});

function getVeryLargeCourseArray() {
    var httpResponse;
    var splitArray;

    $.ajax({
        url: "js/timetable/courses.txt",
        dataType: "text",
        async: false,
        success: function (data) {
            splitArray = data.split("\n").map(function (course, index) {
                return course.substring(0, 8);
            });
        }
    });

    return splitArray;
}

function setupEntry(courseObject) {
    entry = document.createElement("li");
    var courseImg = document.createElement("img");
    $(courseImg).attr("src", "res/ico/delete.ico")
                .addClass("close-icon")
                .click(function() {
                    removeCourseFromList(courseObject.name);
                });

    entry.id = courseObject.name + "-li";
    header = document.createElement("h3");
    header.appendChild(courseImg);
    header.innerHTML = courseObject.name;
    courseObject.header = header;
    sections = processSession(courseObject);
    entry.appendChild(header);
    entry.appendChild(sections);
    $(entry).accordion({heightStyle: "content", collapsible: true, active: false});
    courseSelect.appendChild(entry);
}

function getCourse(courseCode) {
    $.ajax({
        url: "res/courses/" + courseCode + ".txt",
        dataType: "json",
        async: false,
        success: function (data) {
            result = data;
        }
    });
    courseObjects.push(result);
    return result;
}