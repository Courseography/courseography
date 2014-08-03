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
    $("#dialog").fadeOut()
                .css("visibility", "visible");

    $("td").attr("in-conflict", "false")
           .attr("satisfied", "true");

    // .data attribute cannot be set for multiple elements through chaining.
    $("td").each(function() {
        $(this).data("conflictArray", []);
        $(this).data("typeArray", []);
    })


    courseSelect = document.getElementById("course-select");
    searchList = document.getElementById("search-list");
    appendClearAllButton();
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
    header.appendChild(document.createTextNode(courseObject.name));
    courseObject.header = header;
    sections = processSession(courseObject);
    entry.appendChild(header);
    entry.appendChild(sections);
    $(entry).accordion({ heightStyle: "content", collapsible: true, active: false });
    courseSelect.appendChild(entry);
}


// Will refactor with better algorithm once refactoring branch merged.
function fetchCourse(courseCode) {
    $.ajax({
        url: "res/courses/" + courseCode + ".txt",
        dataType: "json",
        async: false,
        success: function (data) {
            result = data;
        }
    });
    return result;
}

// Will refactor.
function getCourse(courseCode) {
    result = fetchCourse(courseCode);
    courseObjects.push(result);
    return result;
}

function appendClearAllButton() {
    var clearAllItem = document.getElementById("clear-all");
    $(clearAllItem).click(function() {
        if (confirm("Clear all selected courses?")) {
            $.each(courseObjects.slice(0), function(i, course) {
                removeCourseFromList(course.name);
            });
        }
    });
}
