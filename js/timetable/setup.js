var result;
var sections;
var courses;
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
