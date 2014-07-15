/*jslint todo: true */
/*global $, console*/
/*jslint browser:true */
/*jslint plusplus: true */
"use strict";

function setSectionMouseEvents(section, sectionTimes, courseObject) {
    setSectionOnClick(section, sectionTimes, courseObject);
    setSectionMouseOver(section, sectionTimes, courseObject);
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

function setSectionMouseOver(section, sectionTimes, courseObject) {
    $(section).mouseover(function () {
        performMouseOver(sectionTimes, courseObject);
        displayCourseInformation(courseObject, $(this));
    });
}

function performMouseOver(sectionTimes, courseObject) {
    $.each(sectionTimes, function (i, time) {
        if (getIsClicked(time)) {
            lightUpConflict(courseObject, time);
        } else {
            lightUpTakeable(courseObject, time);
        }
    });

}

function lightUpConflict(courseObject, timeElement) {
    if ($(timeElement).html() === courseObject.name) {
        $(timeElement).addClass("mouseOverRemove");
    } else {
        $(timeElement).addClass("mouseOverConflict");
    }
}

function lightUpTakeable(courseObject, timeElement) {
    if (courseObject.taken) {
        // IAN-TODO: Highlight already taken section times.
        // I actually think that the hovered section
        // should look the same regardless of other sections,
        // so we should replace mouseOverTaken with mouseOverGood.
        $(timeElement).addClass("mouseOverTaken");
    } else {
        $(timeElement).addClass("mouseOverGood");
    }
    $(timeElement).html(courseObject.name);
}

// IAN-TODO: you'll need to break this into two separate functions
function displayCourseInformation(courseObject, section) {
    $("#course-info-code").html(courseObject.name);
    $("#course-info-title").html(courseObject.title);
    $("#section-stats-section").html(section.html());
    $("#section-stats-instructor").html(section.data("instructor"));
}

/** Mouse Click Direct Functions **/

function setSectionOnClick(section, sectionTimes, courseObject) {
    $(section).click(function () {
        var isLecture = section.innerHTML.charAt(0) === "L";
        var taken = false;
        var satisfied = true;
        var inConflict = false;
        // IAN-TODO: this is a bigger task, but I really don't think
        // we need separate functions for lectures and tutorials
        if (isLecture) {
            if (courseObject.isLectureSelected) {
                selectAlreadySelectedLecture(courseObject, section, sectionTimes);
            } else {
                setLectureSession(courseObject, section);
                selectNewLecture(courseObject, section, sectionTimes);
            }
        } else {
            if (courseObject.isTutorialSelected) {
                selectAlreadySelectedTutorial(courseObject, section, sectionTimes);
            } else {
                setTutorialSession(courseObject, section);
                selectUnselectedTutorial(courseObject, section, sectionTimes);
            }

        }

        // IAN-TODO: use jquery empty or something
        $("#" + courseObject.name + "-li li[satisfied*='false']").each(function() {
            satisfied = false;
        });

        $("#" + courseObject.name + "-li li[clicked*='true']").each(function() {
            if (satisfied) {
                $(this).addClass("clickedLectureTime");
            }

            var index = $.inArray($(this).attr("id"), selectedLectures);

            // IAN-TODO check index == -1
            if (!(index > -1)) {
                    selectedLectures.push($(this).attr("id"));
            }

            // IAN-TODO move out of function (like satisfied = false)
            taken = true;
        });

        $("#" + courseObject.name + "-li li[clicked*='false']").each(function() {
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
        $("td[clicked*=false]").each(function() {
            $(this).attr("satisfied", true);
            $(this).attr("type", "");
            $(this).removeClass("clickedLectureTime clickedTutorialTime");
        });

        $("td[satisfied*=false][in-conflict*=false]").each(function() {
            $(this).addClass("clickedSectionUnsatisfied");
            $(this).removeClass("clickedLectureTime clickedTutorialTime");
        });

        $("td[satisfied*=true]").each(function() {
            $(this).removeClass("clickedSectionUnsatisfied");
        });

        $("td[in-conflict*=true]").each(function() {
            $(this).removeClass("clickedSectionUnsatisfied clickedLectureTime clickedTutorialTime");
            $(this).addClass("clickedConflictTime");
        });

        $("td[in-conflict*=false]").each(function() {
            $(this).removeClass("clickedConflictTime");
        });

        $("td[in-conflict*=false][satisfied*=true][type*=lecture]").each(function() {
            $(this).addClass("clickedLectureTime");
        });

        $("td[in-conflict*=false][satisfied*=true][type*=tutorial]").each(function() {
            $(this).addClass("clickedTutorialTime");
        });

        // IAN-TODO Seems like taken and satisfied can be recovered
        // from courseObject
        setHeader(courseObject, taken, satisfied);
        setCookie("selected-lectures", JSON.stringify(selectedLectures));
        // IAN-TODO we had a problem with this before. Can't remember why.
        removeMouseOverClasses();

        // IAN-TODO Don't pass in inConflict
        inConflict = getInConflict(inConflict);
        alertUserOfConflict(inConflict);
    });
}

/** Utilities **/

// IAN-TODO I guess you want to switch to attributes
// There should really be just one status attribute.
// This seems just like how we handle the nodes in the graph.
function setHeader(courseObject, taken, satisfied) {
    if (taken && satisfied) {
        $(courseObject.header).removeClass("clickedSectionUnsatisfied");
        $(courseObject.header).addClass("clicked-header");
        courseObject.taken = true;
    } else if (!satisfied) {
        $(courseObject.header).addClass("clickedSectionUnsatisfied");
    } else {
        $(courseObject.header).removeClass("clickedSectionUnsatisfied");
        $(courseObject.header).removeClass("clicked-header");
        courseObject.taken = false;
    }
}

// IAN-TODO This is a one liner
function getInConflict(inConflict) {
    $("td[class*=clickedConflictTime]").each(function() {
        inConflict = true;
    });

    return inConflict;
}

function alertUserOfConflict(inConflict) {
    if (inConflict) {
        $("#dialog").fadeIn(750);
    } else {
        $("#dialog").fadeOut(750);
    }
}

function getIsClicked(timeElement) {
    return $(timeElement).attr("clicked") === "true";
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
function setClickedConflict(courseObject, timeElement, section, type) {
    var conflictArray = $(timeElement).data("conflictArray");
    var typeArray = $(timeElement).data("typeArray");
    // IAN-TODO Rather than check these, why not initialize them
    // when the page is first created?
    if (typeof conflictArray === "undefined") {
        conflictArray = [];
    }

    if (typeof typeArray === "undefined") {
        typeArray = [];
    }

    // IAN-TODO: if block doesn't belong in here
    if (!courseObject.satisfied) {
        if (courseObject.isTutorialSelected) {
            $(courseObject.selectedTutorial).addClass("clickedSectionUnsatisfied");
        } else if (courseObject.isLectureSelected) {
            $(courseObject.selectedLecture).addClass("clickedSectionUnsatisfied");
        } else {
            console.log("Unsuspected case in setClickedConflict()");
        }
    }

    conflictArray.push(courseObject.name);
    typeArray.push(type);
    $(timeElement).data("conflictArray", conflictArray);
    $(timeElement).data("typeArray", typeArray);
    $(timeElement).attr("title", conflictArray);
    // IAN-TODO Not sure, but should it be "true"?
    $(timeElement).attr("in-conflict", true);
}

function removeClickedConflict(courseObject, timeElement, section) {
    var conflictArray = $(timeElement).data("conflictArray");
    var typeArray = $(timeElement).data("typeArray");

    if (typeof conflictArray === "undefined" || typeof typeArray === "undefined") {
        console.log("Unexpected case in removeClickedConflict()");
    } else {
        // IAN-TODO This code should be (basically) one big if-else block.
        // Either courseObject.name is in the td, or it's in the list,
        // but not both.
        var index = conflictArray.indexOf(courseObject.name);
        if ($(timeElement).html() === courseObject.name) {
            $(timeElement).html(conflictArray[0]);
            $(timeElement).attr("type", typeArray[0]);
        }
        if (conflictArray.length === 1) {
            $(timeElement).attr("in-conflict", false);
        }
        conflictArray.splice(index, 1);
        typeArray.splice(index, 1);
        var newCourseObject = getCourseObject($(timeElement).html());
        $(timeElement).attr("satisfied", newCourseObject.satisfied);
        $(timeElement).data("conflictArray", conflictArray);
        $(timeElement).attr("title", conflictArray);
    }
}


/** Lecture Functions **/

function setLectureSession(courseObject, section) {
    courseObject.selectedLectureSession = getSession(section);
}

function selectUnselectedLecture(courseObject, section, sectionTimes) {
    $(section).attr("clicked", "true");
    setLectureSession(courseObject, section);
    satisfyCourse(courseObject, section);
    courseObject.selectedLecture = section;
    courseObject.isLectureSelected = true;
    selectUnselectedLectureTimes(sectionTimes, courseObject, section);

    courseObject.selectedTimes = sectionTimes;
}

function selectAlreadySelectedLecture(courseObject, section, sectionTimes) {
    var selectedSession;

    turnLectureOff(courseObject, section, sectionTimes);

    selectedSession = getSession(section);

    if (courseObject.selectedLecture.innerHTML !== section.innerHTML
        || courseObject.selectedLectureSession !== selectedSession) {
        selectNewLecture(courseObject, section, sectionTimes);
    } else {
        courseObject.selectedLecture = null;
        courseObject.selectedLectureSession = null;
        courseObject.selectedTimes = null;
    }
    satisfyCourse(courseObject, section);
}

function turnLectureOff(courseObject, section, sectionTimes) {
    courseObject.isLectureSelected = false;

    $(courseObject.selectedLecture).attr("clicked", "false");
    removeLecture(courseObject, section);

    $(courseObject.selectedLecture).removeClass("clickedSectionUnsatisfied");
}

function removeLecture(courseObject, section) {
    $.each(courseObject.selectedTimes, function (i, time) {
        if ($(time).hasClass("clickedConflictTime")) {
            removeClickedConflict(courseObject, time, section);
        } else {
            $(time).html("");
            $(time).attr("clicked", "false");
        }
    });
}

function selectNewLecture(courseObject, section, sectionTimes) {
    $(section).attr("clicked", "true");
    setLectureSession(courseObject, section);
    courseObject.isLectureSelected = true;
    courseObject.selectedLecture = section;
    courseObject.selectedTimes = sectionTimes;
    satisfyCourse(courseObject, section);
    selectUnselectedLectureTimes(sectionTimes, courseObject, section);
}

function selectUnselectedLectureTimes(sectionTimes, courseObject, section) {
    $.each(sectionTimes, function (i, time) {
        if (!getIsClicked(time)) {
            setLectureClicked(courseObject, time, section);
        } else {
            setClickedConflict(courseObject, time, section, "lecture");
        }
    });
}

function setLectureClicked(courseObject, timeElement, section) {
    $(timeElement).attr("type", "lecture");

    if (!courseObject.satisfied) {
        setSatisfaction(timeElement, courseObject.satisfied);
        $(courseObject.selectedLecture).attr("satisfied", "false");
        $(courseObject.selectedLecture).addClass("clickedSectionUnsatisfied");
    }

    $(timeElement).html(courseObject.name);
    $(timeElement).attr("clicked", "true");
}

/** Tutorial Functions **/

function setTutorialSession(courseObject, section) {
    courseObject.selectedTutorialSession = getSession(section);
}

function selectUnselectedTutorial(courseObject, section, sectionTimes) {
    $(section).attr("clicked", "true");
    setTutorialSession(courseObject, section);
    courseObject.selectedTutorial = section;
    satisfyCourse(courseObject, section);
    courseObject.isTutorialSelected = true;

    selectUnselectedTutorialTimes(courseObject, section, sectionTimes);

    courseObject.selectedTutorialTime = sectionTimes;
}

function selectAlreadySelectedTutorial(courseObject, section, sectionTimes) {
    var selectedSession;

    turnTutorialOff(courseObject, section, sectionTimes);
    selectedSession = getSession(section);

    if (courseObject.selectedTutorial.innerHTML !== section.innerHTML
        || courseObject.selectedTutorialSession !== selectedSession) {
        selectNewTutorialSection(section, sectionTimes, courseObject, selectedSession);
    } else {
        courseObject.selectedTutorial = null;
        courseObject.selectedTutorialSession = null;
        courseObject.selectedTutorialTime = null;
    }
    satisfyCourse(courseObject, section);
}

function turnTutorialOff(courseObject, section, sectionTimes) {
    courseObject.isTutorialSelected = false;
    $(courseObject.selectedTutorial).attr("clicked", "false");
    
    removeTutorial(courseObject, section);
    $(courseObject.selectedTutorial).removeClass("clickedSectionUnsatisfied");
}

function removeTutorial(courseObject, section) {
    $.each(courseObject.selectedTutorialTime, function (i, time) {
        if ($(time).hasClass("clickedConflictTime")) {
            removeClickedConflict(courseObject, time, section);
        } else {
            $(time).html("");
            $(time).attr("clicked", "false");
            $(time).removeClass("clickedTutorialTime");
        }
    });
}

function selectNewTutorialSection(section, sectionTimes, courseObject, selectedSession) {
    $(section).attr("clicked", "true");

    if(courseObject.selectedTutorialSession !== selectedSession) {
        courseObject.selectedTutorialSession = selectedSession;
    }

    satisfyCourse(courseObject, section);
    courseObject.isTutorialSelected = true;
    courseObject.selectedTutorial = section;
    courseObject.selectedTutorialHeader = courseObject.header;
    courseObject.selectedTutorialTime = sectionTimes;
    selectUnselectedTutorialTimes(courseObject, section, sectionTimes);
}

function selectUnselectedTutorialTimes(courseObject, section, sectionTimes) {
    $.each(sectionTimes, function (i, time) {
        if (!getIsClicked(time)) {
            setTutorialClicked(time, courseObject);
        } else {
            setClickedConflict(courseObject, time, section, "tutorial");
        }
    });
}

// IAN-TODO: combine this with setTutorialUnclicked.
// These actions should be symmetric.
function setTutorialClicked(timeElement, courseObject) {
    courseObject.isTutorialSelected = true;

    $(timeElement).attr("type", "tutorial")
                  .html(courseObject.name)
                  .attr("clicked", "true");

    if (!courseObject.satisfied) {
        setSatisfaction(timeElement, courseObject.satisfied);
        $(courseObject.selectedTutorial).addClass("clickedSectionUnsatisfied");
        $(courseObject.selectedTutorial).attr("satisfied", "false");
    }
}

function setTutorialUnclicked(timeElement, courseObject) {
    courseObject.isTutorialSelected = false;

    $(timeElement).html("")
                  .attr("clicked", "false")
                  .removeClass("clickedTutorialTime");

    $(courseObject.selectedTutorial).removeClass("clickedSectionUnsatisfied");
}

/** Course Satisfaction **/

// IAN-TODO
// 1. Split off all function calls that change the view
//    out of this function.
// 2. The only purpose of this function should be to update
//    courseObject.satisfied. The logic for this is about
//    10 lines long.
// 3. Do something for non-manualTutorialEnrolment courses
function satisfyCourse(courseObject, section) {
    if (courseObject.manualTutorialEnrolment) {
        if (courseObject.isTutorialSelected && ((courseObject.selectedTutorialSession === courseObject.selectedLectureSession))) {
            courseObject.satisfied = true;
            satisfyCourseSections(courseObject);
            setTutorialSatisfaction(courseObject);

        } else if (courseObject.isLectureSelected && ((courseObject.selectedLectureSession === courseObject.selectedTutorialSession))) {
            courseObject.satisfied = true;
            satisfyCourseSections(courseObject);
            setLectureSatisfaction(courseObject);

        } else if (courseObject.isTutorialSelected && (courseObject.selectedTutorialSession !== courseObject.selectedLectureSession)) {
            // IAN-TODO Move this into "satisfyCourseSections"
            courseObject.satisfied = false;
            $(courseObject.selectedTutorial).addClass("clickedSectionUnsatisfied");
            setTutorialSatisfaction(courseObject)

        } else if (courseObject.isLectureSelected && (courseObject.selectedLectureSession !== courseObject.selectedTutorialSession)) {
            courseObject.satisfied = false;
            $(courseObject.selectedLecture).addClass("clickedSectionUnsatisfied");
            setLectureSatisfaction(courseObject);

        } else {
            alert("Sat: Uncaught!");
        }
        $(section).attr("satisfied", courseObject.satisfied);
    }
}

// IAN-TODO
// Just use courseObject.selected___Time.
function setLectureSatisfaction(courseObject) {
    $.each(courseObject.selectedTimes, function (i, time) {
        setSatisfaction(time, courseObject.satisfied);
    });
}

function setTutorialSatisfaction(courseObject) {
    $.each(courseObject.selectedTutorialTime, function (i, time) {
        setSatisfaction(time, courseObject.satisfied);
    });
}


function satisfyCourseSections(courseObject) {
    $(courseObject.selectedLecture).removeClass("clickedSectionUnsatisfied");
    $(courseObject.selectedTutorial).removeClass("clickedSectionUnsatisfied");
    $("#" + courseObject.name + "-li" + " li").attr("satisfied", true);
}

function setSatisfaction(timeElement, satisfied) {
    $(timeElement).attr("satisfied", satisfied);
}
