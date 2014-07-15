/*jslint todo: true */
/*global $, console*/
/*jslint browser:true */
/*jslint plusplus: true */
"use strict";

function setSectionMouseEvents(section, sectionTimes, course) {
    setSectionOnClick(section, sectionTimes, course);
    setSectionMouseOver(section, sectionTimes, course);
    setSectionMouseOut(section, sectionTimes);
}

/** Mouse Out Direct Functions **/

function setSectionMouseOut(section, sectionTimes) {
    $(section).mouseout(function () {
        performMouseOut(sectionTimes);
    });
}

function performMouseOut(sectionTimes) {
    $.each(sectionTimes, function (i, time) {
        if ($(time).attr("clicked") !== "true") {
            $(time).html("");
        }
        $(time).removeClass("mouseOverConflict mouseOverGood " +
                                   "mouseOverTaken mouseOverRemove");
    });
}

function removeMouseOverClasses() {
    $("td").removeClass("mouseOverConflict mouseOverGood " +
                        "mouseOverTaken mouseOverRemove");
}

/** Mouse Over Direct Functions **/

function setSectionMouseOver(section, sectionTimes, course) {
    $(section).mouseover(function () {
        performMouseOver(sectionTimes, course);
        displayCourseInformation(course, $(this));
    });
}

function performMouseOver(sectionTimes, course) {
    $.each(sectionTimes, function (i, time) {
        if (getIsClicked(time)) {
            lightUpConflict(course, time);
        } else {
            lightUpTakeable(course, time);
        }
    });

}

function lightUpConflict(course, time) {
    if ($(time).html() === course.name) {
        $(time).addClass("mouseOverRemove");
    } else {
        $(time).addClass("mouseOverConflict");
    }
}

function lightUpTakeable(course, time) {
    if (course.taken) {
        // IAN-TODO: Highlight already taken section times.
        // I actually think that the hovered section
        // should look the same regardless of other sections,
        // so we should replace mouseOverTaken with mouseOverGood.
        $(time).addClass("mouseOverTaken");
    } else {
        $(time).addClass("mouseOverGood");
    }
    $(time).html(course.name);
}

// IAN-TODO: you'll need to break this into two separate functions
function displayCourseInformation(course, section) {
    $("#course-info-code").html(course.name);
    $("#course-info-title").html(course.title);
    $("#section-stats-section").html(section.html());
    $("#section-stats-instructor").html(section.data("instructor"));
}

/** Mouse Click Direct Functions **/

function setSectionOnClick(section, sectionTimes, course) {
    $(section).click(function () {
        var isLecture = section.innerHTML.charAt(0) === "L";
        var taken = false;
        var satisfied = true;
        var inConflict = false;
        // IAN-TODO: this is a bigger task, but I really don't think
        // we need separate functions for lectures and tutorials
        if (isLecture) {
            if (course.isLectureSelected) {
                selectAlreadySelectedLecture(course, section, sectionTimes);
            } else {
                setLectureSession(course, section);
                selectNewLecture(course, section, sectionTimes);
            }
        } else {
            if (course.isTutorialSelected) {
                selectAlreadySelectedTutorial(course, section, sectionTimes);
            } else {
                setTutorialSession(course, section);
                selectUnselectedTutorial(course, section, sectionTimes);
            }

        }

        // IAN-TODO: use jquery empty or something
        $("#" + course.name + "-li li[satisfied*='false']").each(function() {
            satisfied = false;
        });

        $("#" + course.name + "-li li[clicked*='true']").each(function() {
            if (satisfied) {
                $(this).addClass("clickedLectureTime");
            }

            var index = $.inArray($(this).attr("id"), selectedLectures);

            if ((index === -1)) {
                    selectedLectures.push($(this).attr("id"));
            }

            // IAN-TODO move out of function (like satisfied = false)
            taken = true;
        });

        $("#" + course.name + "-li li[clicked*='false']").each(function() {
            $(this).removeClass("clickedLectureTime");
            // IAN-TODO I'm tired of reading these. Let's create helper
            // functions that return booleans
            var index = $.inArray($(this).attr("id"), selectedLectures);

            if (index > -1) {
                selectedLectures.splice(index, 1);
            }

        });

        // IAN-TODO: for all of these .each calls
        // 1) use chaining,
        // 2) you don't need each (attr, *Class work on multiple)
        $("td[clicked*=false]").attr("satisfied", true)
                               .attr("type", "")
                               .html("")
                               .removeClass("clickedLectureTime clickedTutorialTime");

        $("td[satisfied*=false][in-conflict*=false]").addClass("clickedSectionUnsatisfied")
                                                     .removeClass("clickedLectureTime clickedTutorialTime");

        $("td[satisfied*=true]").removeClass("clickedSectionUnsatisfied");

        $("td[in-conflict*=true]").removeClass("clickedSectionUnsatisfied" +
                                               "clickedLectureTime clickedTutorialTime")
                                  .addClass("clickedConflictTime");


        $("td[in-conflict*=false]").removeClass("clickedConflictTime");

        $("td[in-conflict*=false][satisfied*=true][type*=lecture]").addClass("clickedLectureTime");

        $("td[in-conflict*=false][satisfied*=true][type*=tutorial]").addClass("clickedTutorialTime");

        // IAN-TODO Seems like taken and satisfied can be recovered
        // from course
        setHeader(course, taken, satisfied);
        setCookie("selected-lectures", JSON.stringify(selectedLectures));
        // IAN-TODO we had a problem with this before. Can't remember why.
        removeMouseOverClasses();

        alertUserOfConflict();

        if (course.satisfied) {
            $(course.selectedLecture).removeClass("clickedSectionUnsatisfied");
            $(course.selectedTutorial).removeClass("clickedSectionUnsatisfied");
            $("#" + course.name + "-li" + " li").attr("satisfied", true);
        } else {
            $(course.selectedLecture).addClass("clickedSectionUnsatisfied");
            $(course.selectedTutorial).addClass("clickedSectionUnsatisfied");
        }

        if (!course.isTutorialSelected) {
            $(course.selectedTutorial).removeClass("clickedSectionUnsatisfied");
        } else if (!course.satisfied) {
            $(course.selectedTutorial).addClass("clickedSectionUnsatisfied");
        }

        if (!course.isLectureSelected) {
            $(course.selectedLecture).removeClass("clickedSectionUnsatisfied");
        } else if (!course.satisfied) {
            $(course.selectedLecture).addClass("clickedSectionUnsatisfied");
        }

        if (!course.satisfied) {
            if (course.isTutorialSelected) {
                $(course.selectedTutorial).addClass("clickedSectionUnsatisfied");
            } else if (course.isLectureSelected) {
                $(course.selectedLecture).addClass("clickedSectionUnsatisfied");
            }
        }
    });
}

/** Utilities **/

// IAN-TODO I guess you want to switch to attributes
// There should really be just one status attribute.
// This seems just like how we handle the nodes in the graph.
function setHeader(course, taken, satisfied) {
    if (taken && satisfied) {
        $(course.header).removeClass("clickedSectionUnsatisfied")
                              .addClass("clicked-header");
        course.taken = true;
    } else if (!satisfied) {
        $(course.header).addClass("clickedSectionUnsatisfied");
    } else {
        $(course.header).removeClass("clickedSectionUnsatisfied clicked-header");
        course.taken = false;
    }
}

// IAN-TODO This is a one liner
function getInConflict() {
    var inConflict = false;
    $("td[class*=clickedConflictTime]").each(function() {
        inConflict = true;
    });

    return inConflict;
}

function alertUserOfConflict() {
    if (getInConflict()) {
        $("#dialog").fadeIn(750);
    } else {
        $("#dialog").fadeOut(750);
    }
}

function getIsClicked(time) {
    return $(time).attr("clicked") === "true";
}

function getSession(section) {
    if (getIsYearSection(section)) {
        return "Y";
    } else if (getIsFallSection(section)) {
        return "F";
    } else if (getIsSpringSection(section)) {
        return "S";
    }
}

function getIsYearSection(section) {
    return $(section.parentNode).hasClass("sectionList-year");
}

function getIsFallSection(section) {
    return $(section.parentNode).hasClass("sectionList-fall");
}

function getIsSpringSection(section) {
    return $(section.parentNode).hasClass("sectionList-spring");
}

// IAN-TODO you really shouldn't need "type" as a parameter
function setClickedConflict(course, time, section, type) {
    alert(time);
    var conflictArray = $(time).data("conflictArray");
    var typeArray = $(time).data("typeArray");
    console.log(time);
    console.log(conflictArray);
    conflictArray.push(course.name);
    typeArray.push(type);
    $(time).data("conflictArray", conflictArray)
           .data("typeArray", typeArray)
           .attr("title", conflictArray)
           .attr("in-conflict", "true");
}

function removeClickedConflict(course, time, section) {
    alert(time);
    var conflictArray = $(time).data("conflictArray");
    var typeArray = $(time).data("typeArray");
    console.log("removing conflict");
    if (typeof conflictArray === "undefined" || typeof typeArray === "undefined") {
        console.log("Unexpected case in removeClickedConflict()");
    } else {
        // IAN-TODO This code should be (basically) one big if-else block.
        // Either course.name is in the td, or it's in the list,
        // but not both.
        var index = conflictArray.indexOf(course.name);
        if ($(time).html() === course.name) {
            $(time).html(conflictArray[0])
                   .attr("type", typeArray[0]);
        }
        if (conflictArray.length === 1) {
            $(time).attr("in-conflict", "false");
        }
        console.log(conflictArray);
        conflictArray.splice(index, 1);
        typeArray.splice(index, 1);
        console.log(conflictArray);
        var newCourseObject = getCourseObject($(time).html());
        $(time).attr("satisfied", newCourseObject.satisfied)
               .data("conflictArray", conflictArray)
               .data("typeArray", typeArray)
               .attr("title", conflictArray);
    }
}


/** Lecture Functions **/

function setLectureSession(course, section) {
    course.selectedLectureSession = getSession(section);
}

function selectUnselectedLecture(course, section, sectionTimes) {
    $(section).attr("clicked", "true");
    setLectureSession(course, section);
    satisfyCourse(course, section);
    course.selectedLecture = section;
    course.isLectureSelected = true;
    selectUnselectedLectureTimes(sectionTimes, course, section);

    course.selectedLectureTimes = sectionTimes;
}

function selectAlreadySelectedLecture(course, section, sectionTimes) {
    var selectedSession;

    turnLectureOff(course, section, sectionTimes);

    selectedSession = getSession(section);

    if (course.selectedLecture.innerHTML !== section.innerHTML
        || course.selectedLectureSession !== selectedSession) {
        selectNewLecture(course, section, sectionTimes);
    } else {
        course.selectedLecture = null;
        course.selectedLectureSession = null;
        course.selectedLectureTimes = null;
    }
    satisfyCourse(course, section);
}

function turnLectureOff(course, section, sectionTimes) {
    course.isLectureSelected = false;

    $(course.selectedLecture).attr("clicked", "false");
    removeLecture(course, section);
}

function removeLecture(course, section) {
    $.each(course.selectedLectureTimes, function (i, time) {
        if ($(time).attr("in-conflict") === "true") {
        console.log("removing");
            removeClickedConflict(course, time, section);
        } else {
            $(time).html("")
                   .attr("clicked", "false");
        }
    });
}

function selectNewLecture(course, section, sectionTimes) {
    $(section).attr("clicked", "true");
    setLectureSession(course, section);
    course.isLectureSelected = true;
    course.selectedLecture = section;
    course.selectedLectureTimes = sectionTimes;
    satisfyCourse(course, section);
    selectUnselectedLectureTimes(sectionTimes, course, section);
}

function selectUnselectedLectureTimes(sectionTimes, course, section) {
    $.each(sectionTimes, function (i, time) {
        if (!getIsClicked(time)) {
            setLectureClicked(course, time, section);
        } else {
            setClickedConflict(course, time, section, "lecture");
        }
    });
}

function setLectureClicked(course, time, section) {
    $(time).attr("type", "lecture");

    if (!course.satisfied) {
        setSatisfaction(time, course.satisfied);
        $(course.selectedLecture).attr("satisfied", "false");
    }

    $(time).html(course.name)
           .attr("clicked", "true");
}

/** Tutorial Functions **/

function setTutorialSession(course, section) {
    course.selectedTutorialSession = getSession(section);
}

function selectUnselectedTutorial(course, section, sectionTimes) {
    $(section).attr("clicked", "true");
    setTutorialSession(course, section);
    course.selectedTutorial = section;
    satisfyCourse(course, section);
    course.isTutorialSelected = true;

    selectUnselectedTutorialTimes(course, section, sectionTimes);

    course.selectedTutorialTimes = sectionTimes;
}

function selectAlreadySelectedTutorial(course, section, sectionTimes) {
    var selectedSession;

    turnTutorialOff(course, section, sectionTimes);
    selectedSession = getSession(section);

    if (course.selectedTutorial.innerHTML !== section.innerHTML
        || course.selectedTutorialSession !== selectedSession) {
        selectNewTutorialSection(section, sectionTimes, course, selectedSession);
    } else {
        course.selectedTutorial = null;
        course.selectedTutorialSession = null;
        course.selectedTutorialTimes = null;
    }
    satisfyCourse(course, section);
}

function turnTutorialOff(course, section, sectionTimes) {
    course.isTutorialSelected = false;
    $(course.selectedTutorial).attr("clicked", "false");
    
    removeTutorial(course, section);
}

function removeTutorial(course, section) {
    $.each(course.selectedTutorialTimes, function (i, time) {
        if ($(time).attr("in-conflict") === "true") {
            removeClickedConflict(course, time, section);
        } else {
            $(time).html("")
                   .attr("clicked", "false");
        }
    });
}

function selectNewTutorialSection(section, sectionTimes, course, selectedSession) {
    $(section).attr("clicked", "true");

    if(course.selectedTutorialSession !== selectedSession) {
        course.selectedTutorialSession = selectedSession;
    }

    satisfyCourse(course, section);
    course.isTutorialSelected = true;
    course.selectedTutorial = section;
    course.selectedTutorialHeader = course.header;
    course.selectedTutorialTimes = sectionTimes;
    selectUnselectedTutorialTimes(course, section, sectionTimes);
}

function selectUnselectedTutorialTimes(course, section, sectionTimes) {
    $.each(sectionTimes, function (i, time) {
        if (!getIsClicked(time)) {
            setTutorialClicked(time, course);
        } else {
            setClickedConflict(course, time, section, "tutorial");
        }
    });
}

// IAN-TODO: combine this with setTutorialUnclicked.
// These actions should be symmetric.
function setTutorialClicked(time, course) {
    course.isTutorialSelected = true;

    $(time).attr("type", "tutorial")
                  .html(course.name)
                  .attr("clicked", "true");

    if (!course.satisfied) {
        setSatisfaction(time, course.satisfied);
        $(course.selectedTutorial).attr("satisfied", "false");
    }
}

function setTutorialUnclicked(time, course) {
    course.isTutorialSelected = false;
    $(time).attr("clicked", "false");
}

/** Course Satisfaction **/

// IAN-TODO
// 1. Split off all function calls that change the view
//    out of this function.
// 2. The only purpose of this function should be to update
//    course.satisfied. The logic for this is about
//    10 lines long.
// 3. Do something for non-manualTutorialEnrolment courses
function satisfyCourse(course, section) {
    if (course.manualTutorialEnrolment) {
        if (course.isTutorialSelected && ((course.selectedTutorialSession === course.selectedLectureSession))) {
            course.satisfied = true;
            setTutorialSatisfaction(course);

        } else if (course.isLectureSelected && ((course.selectedLectureSession === course.selectedTutorialSession))) {
            course.satisfied = true;
            setLectureSatisfaction(course);

        } else if (course.isTutorialSelected && (course.selectedTutorialSession !== course.selectedLectureSession)) {
            // IAN-TODO Move this into "satisfyCourseSections"
            course.satisfied = false;
            setTutorialSatisfaction(course)

        } else if (course.isLectureSelected && (course.selectedLectureSession !== course.selectedTutorialSession)) {
            course.satisfied = false;
            setLectureSatisfaction(course);

        }
        $(section).attr("satisfied", course.satisfied);
    }
}

// IAN-TODO
// Just use course.selected___Time.
function setLectureSatisfaction(course) {
    $.each(course.selectedLectureTimes, function (i, time) {
        setSatisfaction(time, course.satisfied);
    });
}

function setTutorialSatisfaction(course) {
    $.each(course.selectedTutorialTimes, function (i, time) {
        setSatisfaction(time, course.satisfied);
    });
}

function setSatisfaction(time, satisfied) {
    $(time).attr("satisfied", satisfied);
}
