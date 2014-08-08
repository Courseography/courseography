"use strict";


/* Hover functions */
function setTdHover() {
    $("td").mouseover(function () {
        var courseName = $(this).html();
        if (courseName !== "") {
            var course = getCourseObject(courseName, courseObjects);
            if (course !== undefined) {
                $.each(course.sectionTimes(), function (i, time) {
                    $(time).addClass("hover-time");
                });

                var section = course.selected[$(this).attr("type")];
                displayCourseInformation(course);
                displaySectionInformation(section);
            }
        }

    }).mouseout(function () {
        var course = getCourseObject($(this).html(), courseObjects);
        if (course !== undefined) {
            $.each(course.sectionTimes(), function (i, time) {
                $(time).removeClass("hover-time");
            });

            clearCourseInformation();
        }
    });
}


/* Conflicts */
function renderConflicts(time, conflicts) {
    $(time).data("conflicts", conflicts)
           .attr("title", conflicts.map(function (section) {
                              return section.courseName;
                          })
            )
           .attr("in-conflict", "" + (conflicts.length > 0))
           .attr("status", conflicts.length > 0 ? "conflict" : "occupied")
           .attr("satisfied", getCourseObject($(time).html(), courseObjects).satisfied);

}


function getInConflict() {
    return $("td[in-conflict*=true]").length > 0;
}


function alertUserOfConflict() {
    var dialogSelector = $("#dialog");
    getInConflict() ? dialogSelector.fadeIn(750) :
                      dialogSelector.fadeOut(750);
}


/* Functions to manipuate grid DOM */
function renderClearTime(time) {
    $(time).html("")
           .attr("clicked", "false")
           .attr("satisfied", "true")
           .attr("type", "")
           .attr("hover", "off")
           .attr("status", "clear");
}


function renderClearHover(time) {
    if ($(time).attr("clicked") !== "true") {
            $(time).html("");
        }
    $(time).attr("hover", "off");
}


function renderAddHover(time, section) {
    if ($(time).attr("clicked") !== "true") {
        $(time).html(section.courseName);
        $(time).attr("hover", "good");
    } else if ($(time).html() === section.courseName &&
               $(time).attr("type") === section.type) {
        $(time).attr("hover", "remove");
    } else {
        $(time).attr("hover", "conflict");
    }
}
