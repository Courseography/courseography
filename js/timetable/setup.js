var result;
var i;
var courseSelect;
var sections;
var courses;
var searchList;
var courseCache = [];


$(document).ready(function () {
    var tdObjects = $("td");

    $("#dialog").fadeOut()
                .css("visibility", "visible");

    tdObjects.attr("in-conflict", "false")
           .attr("satisfied", "true");

    tdObjects.each(function () {
        $(this).data("conflictArray", [])
               .data("typeArray", []);
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


function fetchCourse(courseCode) {
    var course = getCourseObject(courseCode, courseCache);
    if (typeof course !== "undefined") {
        return course;
    }
    $.ajax({
        url: "res/courses/" + courseCode + ".txt",
        dataType: "json",
        async: false,
        success: function (data) {
            course = data;
        },
        error: function () {
            throw "No course file";
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
