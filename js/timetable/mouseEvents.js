"use strict";


function setSectionMouseEvents(section, sectionTimes, course) {
    setSectionOnClick(section, sectionTimes, course);
    setSectionMouseOver(section, sectionTimes, course);
    setSectionMouseOut(section, sectionTimes);
    setTdHover();
    setHeaderHover(course);
}


function setTdHover() {
    var tdObjects = $("td");

    tdObjects.mouseover(function () {
        var courseHtml = $(this).html();
        var course = getCourseObject(courseHtml, courseObjects);
        if (typeof course !== "undefined") {
            var sectionTimes = [];
            if (typeof course.selectedLectureTimes !== "undefined") {
                sectionTimes = sectionTimes.concat(course.selectedLectureTimes);
            }
            if (typeof course.selectedTutorialTimes !== "undefined") {
                sectionTimes = sectionTimes.concat(course.selectedTutorialTimes);
            }
            $.each(sectionTimes, function(i, time) {
                $(time).addClass("hover-time");
            });

            var section;
            if ($(this).attr("type") === "L") {
                section = course.selectedLecture;
            } else if ($(this).attr("type") === "T") {
                section = course.selectedTutorial;
            } else if ($(this).attr("type") === "P") {
                section = course.selectedPractical;
            }
            displayCourseInformation(course);
            displaySectionInformation($(section));
        }
    });

    tdObjects.mouseout(function () {
        var courseHtml = $(this).html();
        var course = getCourseObject(courseHtml, courseObjects);
        if (typeof course !== "undefined") {
            var sectionTimes = [];
            if (typeof course.selectedLectureTimes !== undefined) {
                sectionTimes = sectionTimes.concat(course.selectedLectureTimes);
            }
            if (typeof course.selectedTutorialTimes !== undefined) {
                sectionTimes = sectionTimes.concat(course.selectedTutorialTimes);
            }
            $.each(sectionTimes, function(i, time) {
                $(time).removeClass("hover-time");
            });
        }
        clearCourseInformation();
    });
}


function setSectionMouseOut(section, sectionTimes) {
    $(section).mouseout(function () {
        performMouseOut(sectionTimes);
        clearCourseInformation();
    });
}


function performMouseOut(sectionTimes) {
    $.each(sectionTimes, function (i, time) {
        if ($(time).attr("clicked") !== "true") {
            $(time).html("");
        }
        $(time).attr("hover", "off");
    });
}


function setSectionMouseOver(section, sectionTimes, course) {
    $(section).mouseover(function () {
        performMouseOver(sectionTimes, course, section);
        displayCourseInformation(course);
        displaySectionInformation($(this));
    });
}


function performMouseOver(sectionTimes, course, section) {
    $.each(sectionTimes, function (i, time) {
        if (getIsClicked(time)) {
            lightUpConflict(course, time, section);
        } else {
            lightUpTakeable(course, time);
        }
    });
}

function lightUpConflict(course, time, section) {
    if ($(time).html() === course.name &&
        $(time).attr("type") === getType(section)) {
        $(time).attr("hover", "remove");
    } else {
        $(time).attr("hover", "conflict");
    }
}


function lightUpTakeable(course, time) {
    $(time).html(course.name);
    $(time).attr("hover", "good");
}


function setHeaderHover(course) {
    $(course.header).mouseover(function() {
        displayCourseTitle(course);
    })
        .mouseout(function() {
            clearCourseInformation();
        });
}


function setSectionOnClick(section, sectionTimes, course) {
    $(section).click(function () {
        updateSelectedLectures($(section));

        course.clickSection(section, sectionTimes);

        course.updateSatisfaction();
        course.renderSatisfaction();
        course.renderUpdatedHeader();

        saveCookies(selectedCourses, selectedLectures);

        alertUserOfConflict();
    });
}


function selectUnselectedTimes(course, sectionTimes, section) {
    $.each(sectionTimes, function (i, time) {
        if (!getIsClicked(time)) {
            setClickedTime(section, time);
        } else {
            setClickedConflict(course, time, section);
        }
    });
}


function setClickedTime(section, time) {
    var type = getType(section);
    var name = section.id.substring(0, 8);
    $(time).html(name)
           .attr("clicked", "true")
           .attr("type", type);
}


/* Conflicts */
function getInConflict() {
    return $("td[in-conflict*=true]").length > 0;
}


function alertUserOfConflict() {
    var dialogSelector = $("#dialog");
    getInConflict() ? dialogSelector.fadeIn(750) :
                      dialogSelector.fadeOut(750);
}


function getIsClicked(time) {
    return $(time).attr("clicked") === "true";
}


function setClickedConflict(course, time, section) {
    var conflicts = $(time).data("conflicts");
    conflicts.push(course.selected[getType(section)]);
    renderConflicts(time, conflicts);
}


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


function removeClickedConflict(section, time) {
    var conflicts = $(time).data("conflicts");
    
    // Find section in conflicts
    var name = section.id.substr(0, 8);
    var type = getType(section);
    var index = -1;

    for (var i = 0; i < conflicts.length; i++) {
        if (conflicts[i].courseName == name && conflicts[i].type == type) {
            index = i;
            break;
        }
    }

    if (index === -1) {
        $(time).html(conflicts[0].courseName)
               .attr("type", conflicts[0].type);
        conflicts.splice(0, 1);
    } else {
        conflicts.splice(index, 1);
    }

    renderConflicts(time, conflicts);
}


/* Remove a section from locations in the grid. */
function removeSectionTimes(section, times) {
    var sectionTimes = times;
    console.log(times);
    $.each(times, function (i, time) {
        if ($(time).attr("in-conflict") === "true") {
            removeClickedConflict(section, time);
        } else {
            clearTime(time);
        }
    });
}


function clearTime(time) {
    $(time).html("")
           .attr("clicked", "false")
           .attr("satisfied", "true")
           .attr("type", "")
           .attr("status", "clear");
}
