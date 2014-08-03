var result;
var i;
var courseSelect;
var header;
var sections;
var entry;
var courses;
var searchList;
var courseCache = [];


$(document).ready(function () {
    var tdSelector = $("td");

    $("#dialog").fadeOut()
                .css("visibility", "visible");

    tdSelector.attr("in-conflict", "false")
           .attr("satisfied", "true");

    // .data attribute cannot be set for multiple elements through chaining.
    tdSelector.each(function () {
        $(this).data("conflictArray", []);
        $(this).data("typeArray", []);
    });


    courseSelect = document.getElementById("course-select");
    searchList = document.getElementById("search-list");
    appendClearAllButton();
    restoreFromCookies();
    enableSearch();
    courses = getVeryLargeCourseArray();
    trapScroll();
});


function getVeryLargeCourseArray() {
    var splitArray = undefined;

    $.ajax({
        url: "js/timetable/courses.txt",
        dataType: "text",
        async: false,
        success: function (data) {
            splitArray = data.split("\n").map(function (course) {
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
                .click(function () {
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
    $(entry).accordion({
        heightStyle: "content",
        collapsible: true,
        active: false
    });
    courseSelect.appendChild(entry);
}


function fetchCourse(courseCode) {
    var course = getCourseObject(courseCode, courseCache);
    if (typeof course !== "undefined") {
        console.log("found cached course!");
        return course;
    }
    $.ajax({
        url: "res/courses/" + courseCode + ".txt",
        dataType: "json",
        async: false,
        success: function (data) {
            course = data;
        }
    });
    courseCache.push(course);
    return course;
}


function getCourse(courseCode) {
    result = fetchCourse(courseCode);
    courseObjects.push(result);
    return result;
}


function appendClearAllButton() {
    var clearAllItem = document.getElementById("clear-all");
    $(clearAllItem).click(function () {
        if (confirm("Clear all selected courses?")) {
            $.each(courseObjects.slice(0), function (i, course) {
                removeCourseFromList(course.name);
            });
        }
    });
}
