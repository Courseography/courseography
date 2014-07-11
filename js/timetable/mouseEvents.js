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
        var timeSuffix = getTimeSuffix(section);
        performMouseOut(sectionTimes, timeSuffix);
    });
}

function performMouseOut(sectionTimes, timeSuffix) {
    if (timeSuffix === "-year") {
        performMouseOut(sectionTimes, "-fall");
        performMouseOut(sectionTimes, "-spring");
    } else {
        $.each(sectionTimes, function (i, time) {
            var timeElement = "#" + time + timeSuffix;
            if ($(timeElement).attr("clicked") !== "true") {
                $(timeElement).html("");
            }
            $(timeElement).removeClass("mouseOverConflict mouseOverGood " +
                                       "mouseOverTaken mouseOverRemove");
        });
    }
}

function removeMouseOverClasses() {
    $("td").removeClass("mouseOverConflict mouseOverGood " +
                        "mouseOverTaken mouseOverRemove");
}

/** Mouse Over Direct Functions **/

function setSectionMouseOver(section, sectionTimes, courseObject) {
    $(section).mouseover(function () {
        var timeSuffix = getTimeSuffix(section);
        performMouseOver(sectionTimes, timeSuffix, courseObject);
        displayCourseInformation(courseObject, $(this));
    });
}

function performMouseOver(sectionTimes, timeSuffix, courseObject) {
    if (timeSuffix === "-year") {
        performMouseOver(sectionTimes, "-fall", courseObject);
        performMouseOver(sectionTimes, "-spring", courseObject);
    } else {
        $.each(sectionTimes, function (i, time) {
            var timeElement = time + timeSuffix;

            if (getIsClicked(timeElement)) {
                lightUpConflict(courseObject, timeElement);
            } else {
                lightUpTakeable(courseObject, timeElement);
            }
        });
    }
}

function lightUpConflict(courseObject, timeElement) {
    if ($("#" + timeElement).html() === courseObject.name) {
        $("#" + timeElement).addClass("mouseOverRemove");
    } else {
        $("#" + timeElement).addClass("mouseOverConflict");
    }
}

function lightUpTakeable(courseObject, timeElement) {
    if (courseObject.taken) {
        // IAN-TODO: Highlight already taken section times.
        // I actually think that the hovered section
        // should look the same regardless of other sections,
        // so we should replace mouseOverTaken with mouseOverGood.
        $("#" + timeElement).addClass("mouseOverTaken");
    } else {
        $("#" + timeElement).addClass("mouseOverGood");
    }
    $("#" + timeElement).html(courseObject.name);
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
            $(this).removeClass("clickedLectureTime");
            $(this).removeClass("clickedTutorialTime");
        });

        $("td[satisfied*=false][in-conflict*=false]").each(function() {
            $(this).addClass("clickedSectionUnsatisfied");
            $(this).removeClass("clickedLectureTime");
            $(this).removeClass("clickedTutorialTime");
        });

        $("td[satisfied*=true]").each(function() {
            $(this).removeClass("clickedSectionUnsatisfied");
        });

        $("td[in-conflict*=true]").each(function() {
            $(this).removeClass("clickedSectionUnsatisfied");
            $(this).removeClass("clickedLectureTime");
            $(this).removeClass("clickedTutorialTime");
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

// IAN-TODO shorter animation
function alertUserOfConflict(inConflict) {
    if (inConflict) {
        $("#dialog").fadeIn(1500);
    } else {
        $("#dialog").fadeOut(1500);
    }
}

function getIsClicked(timeElement) {
    return $("#" + timeElement).attr("clicked") === "true";
}

// IAN-TODO Somewhere you redo this code, but you should call this function
// instead
function getSectionSessionFromSection(section) {
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

// IAN-TODO Combine with getSectionSessionFromSection
// or at least put them next to each other
function getTimeSuffix(section) {
    var timeSuffix;

    if (getIsFallSection(section)) {
        timeSuffix = "-fall";
    } else if (getIsSpringSection(section)) {
        timeSuffix = "-spring";
    } else {
        timeSuffix = "-year";
    }

    return timeSuffix;
}


// IAN-TODO you really shouldn't need "type" as a parameter
function setClickedConflict(courseObject, timeElement, section, type) {
    var conflictArray = $("#" + timeElement).data("conflictArray");
    var typeArray = $("#" + timeElement).data("typeArray");
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
    $("#" + timeElement).data("conflictArray", conflictArray);
    $("#" + timeElement).data("typeArray", typeArray);
    $("#" + timeElement).attr("title", conflictArray);
    // IAN-TODO Not sure, but should it be "true"?
    $("#" + timeElement).attr("in-conflict", true);
}


// IAN-TODO What's the point of passing courseObject here?
// It seems like this function doesn't do the right thing if
// courseObject.name !== $(...).html()
function removeClickedConflict(courseObject, timeElement, section) {
    var conflictArray = $("#" + timeElement).data("conflictArray");
    var typeArray = $("#" + timeElement).data("typeArray");

    if (typeof conflictArray === "undefined" || typeof typeArray === "undefined") {
        console.log("Unexpected case in removeClickedConflict()");
    } else {
        var index = conflictArray.indexOf(courseObject.name);
        if ($("#" + timeElement).html() === courseObject.name) {
            $("#" + timeElement).html(conflictArray[0]);
            $("#" + timeElement).attr("type", typeArray[0]);
        }
        if (conflictArray.length === 1) {
            $("#" + timeElement).attr("in-conflict", false);
        }
        conflictArray.splice(index, 1);
        typeArray.splice(index, 1);
        var newCourseObject = getCourseObject($("#" + timeElement).html());
        $("#" + timeElement).attr("satisfied", newCourseObject.satisfied);
        $("#" + timeElement).data("conflictArray", conflictArray);
        $("#" + timeElement).attr("title", conflictArray);
    }
}


/** Lecture Functions **/

function setLectureSession(courseObject, section) {
    courseObject.selectedLectureSession = getSectionSessionFromSection(section);
}

function selectUnselectedLecture(courseObject, section, sectionTimes) {
    $(section).attr("clicked", "true");
    setLectureSession(courseObject, section);
    satisfyCourse(courseObject, section);
    courseObject.selectedLecture = section;
    courseObject.isLectureSelected = true;
    var timeSuffix = getTimeSuffix(section);
    selectUnselectedLectureTimes(sectionTimes, timeSuffix, courseObject, section);

    if (getIsYearSection(section)) {
        timeSuffix = reverseTimeSuffix(timeSuffix);
        selectUnselectedLectureTimes(sectionTimes, timeSuffix, courseObject, section);
    }

    courseObject.selectedTimes = sectionTimes;
}

function selectAlreadySelectedLecture(courseObject, section, sectionTimes) {
    var selectedSession;

    turnLectureOff(courseObject, section, sectionTimes);

    if (getIsFallSection(section)) {
        selectedSession = "F";
    } else if (getIsSpringSection(section)) {
        selectedSession = "S";
    } else {
        selectedSession = "Y";
    }

    if (courseObject.selectedLecture.innerHTML !== section.innerHTML
        || courseObject.selectedLectureSession !== selectedSession) {
        selectNewLecture(courseObject, section, sectionTimes);
    } else {
        courseObject.selectedLecture = null;
        courseObject.selectedLectureSession = null;
        courseObject.selectedTimes = null;
    }
}

function turnLectureOff(courseObject, section, sectionTimes) {
    var timeSuffix;

    courseObject.isLectureSelected = false;
    unsatisfyCourse(courseObject, section);

    $(courseObject.selectedLecture).attr("clicked", "false");

    if (courseObject.selectedLectureSession === "F") {
        timeSuffix = "-fall";
    } else if (courseObject.selectedLectureSession === "S") {
        timeSuffix = "-spring";
    } else {
        timeSuffix = "-year";
    }

    removeLecture(courseObject, section, timeSuffix);

    $(courseObject.selectedLecture).removeClass("clickedSectionUnsatisfied");
}

function removeLecture(courseObject, section, timeSuffix) {
    if (timeSuffix === "-year") {
        removeLecture(courseObject, section, "-fall");
        removeLecture(courseObject, section, "-spring");
    } else {
        $.each(courseObject.selectedTimes, function (i, time) {
            timeElement = time + timeSuffix;

            if ($("#" + timeElement).hasClass("clickedConflictTime")) {
                removeClickedConflict(courseObject, timeElement, section);
            } else {
                $("#" + timeElement).html("");
                $("#" + timeElement).attr("clicked", "false");
            }
        });
    }
}

function selectNewLecture(courseObject, section, sectionTimes) {
    $(section).attr("clicked", "true");
    setLectureSession(courseObject, section);
    courseObject.isLectureSelected = true;
    courseObject.selectedLecture = section;
    courseObject.selectedTimes = sectionTimes;

    var timeSuffix = getTimeSuffix(section);

    satisfyCourse(courseObject, section);
    selectUnselectedLectureTimes(sectionTimes, timeSuffix, courseObject, section);
}

function selectUnselectedLectureTimes(sectionTimes, timeSuffix, courseObject, section) {
    if (timeSuffix === "-year") {
        selectUnselectedLectureTimes(sectionTimes, "-fall", courseObject, section);
        selectUnselectedLectureTimes(sectionTimes, "-spring", courseObject, section);
    } else {
        $.each(sectionTimes, function (i, time) {
            var timeElement = time + timeSuffix;

            if (!getIsClicked(timeElement)) {
                setLectureClicked(courseObject, timeElement, section);
            } else {
                setClickedConflict(courseObject, timeElement, section, "lecture");
            }

        });
    }
}

function setLectureClicked(courseObject, timeElement, section) {
    $("#" + timeElement).attr("type", "lecture");

    if (!courseObject.satisfied) {
        setSatisfaction(timeElement, courseObject.satisfied);
        $(courseObject.selectedLecture).attr("satisfied", "false");
        $(courseObject.selectedLecture).addClass("clickedSectionUnsatisfied");
    }

    $("#" + timeElement).html(courseObject.name);
    $("#" + timeElement).attr("clicked", "true");
}

/** Tutorial Functions **/

function setTutorialSession(courseObject, section) {
    courseObject.selectedTutorialSession = getSectionSessionFromSection(section);
}

function selectUnselectedTutorial(courseObject, section, sectionTimes) {
    $(section).attr("clicked", "true");
    setTutorialSession(courseObject, section);
    courseObject.selectedTutorial = section;
    satisfyCourse(courseObject, section);
    courseObject.isTutorialSelected = true;
    var timeSuffix = getTimeSuffix(section);

    selectUnselectedTutorialTimes(courseObject, section, sectionTimes, timeSuffix);

    if (getIsYearSection(section)) {
        timeSuffix = reverseTimeSuffix(timeSuffix);
        selectUnselectedTutorialTimes(courseObject, section, sectionTimes, timeSuffix);
    }

    courseObject.selectedTutorialTime = sectionTimes;
}

function selectAlreadySelectedTutorial(courseObject, section, sectionTimes) {
    var selectedSession;

    turnTutorialOff(courseObject, section, sectionTimes);

    if (getIsFallSection(section)) {
        selectedSession = "F";
    } else if (getIsSpringSection(section)) {
        selectedSession = "S";
    } else {
        selectedSession = "Y";
    }

    if (courseObject.selectedTutorial.innerHTML !== section.innerHTML
        || courseObject.selectedTutorialSession !== selectedSession) {
        selectNewTutorialSection(section, sectionTimes, courseObject, selectedSession);
    } else {
        courseObject.selectedTutorial = null;
        courseObject.selectedTutorialSession = null;
        courseObject.selectedTutorialTime = null;
    }
}

// IAN-TODO time suffix
function turnTutorialOff(courseObject, section, sectionTimes) {
    var timeSuffix;

    courseObject.isTutorialSelected = false;
    unsatisfyCourse(courseObject, section);

    $(courseObject.selectedTutorial).attr("clicked", "false");

    if (courseObject.selectedTutorialSession === "F") {
        timeSuffix = "-fall";
    } else {
        timeSuffix = "-spring";
    }

    $.each(courseObject.selectedTutorialTime, function (i, time) {
        var timeElement = time + timeSuffix;

        if ($("#" + timeElement).hasClass("clickedConflictTime")) {
            removeClickedConflict(courseObject, timeElement, section);
        } else {
            $("#" + timeElement).html("");
            $("#" + timeElement).attr("clicked", "false");
            $("#" + timeElement).removeClass("clickedTutorialTime");
        }
    });

    $(courseObject.selectedTutorial).removeClass("clickedSectionUnsatisfied");

    if (getIsYearSection(section)) {
        timeSuffix = reverseTimeSuffix(timeSuffix);

        $.each(courseObject.selectedTutorialTime, function (i, time) {
            timeElement = time + timeSuffix;

            if ($("#" + timeElement).hasClass("clickedConflictTime")) {
                removeClickedConflict(courseObject, timeElement, section);
            } else {
                $("#" + timeElement).html("");
                $("#" + timeElement).attr("clicked", "false");
                $("#" + timeElement).removeClass("clickedTutorialTime");
            }
        });
    }
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

    var timeSuffix = getTimeSuffix(section);
    selectUnselectedTutorialTimes(courseObject, section, sectionTimes, timeSuffix);

    if (getIsYearSection(section)) {
        timeSuffix = reverseTimeSuffix(timeSuffix);
        selectUnselectedTutorialTimes(courseObject, section, sectionTimes, timeSuffix);
    }
}

// IAN-TODO: timeSuffix
function selectUnselectedTutorialTimes(courseObject, section, sectionTimes, timeSuffix) {
    $.each(sectionTimes, function (i, time) {
        var timeElement = time + timeSuffix;
        
        if (!getIsClicked(timeElement)) {
            setTutorialClicked(timeElement, courseObject);
        } else {
            setClickedConflict(courseObject, timeElement, section, "tutorial");
        }
    });

}

// IAN-TODO: combine this with setTutorialUnclicked.
// These actions should be symmetric.
function setTutorialClicked(timeElement, courseObject) {
    courseObject.isTutorialSelected = true;

    $("#" + timeElement).attr("type", "tutorial");
    $("#" + timeElement).html(courseObject.name);
    $("#" + timeElement).attr("clicked", "true");

    if (!courseObject.satisfied) {
        setSatisfaction(timeElement, courseObject.satisfied);
        $(courseObject.selectedTutorial).addClass("clickedSectionUnsatisfied");
        $(courseObject.selectedTutorial).attr("satisfied", "false");
    }
}

function setTutorialUnclicked(timeElement, courseObject) {
    courseObject.isTutorialSelected = false;

    $("#" + timeElement).html("")
                        .attr("clicked", "false")
                        .removeClass("clickedTutorialTime");

    $(courseObject.selectedTutorial).removeClass("clickedSectionUnsatisfied");
}

/** Course Satisfaction **/

function satisfyCourse(courseObject, section) {
    var timeSuffix;
    var timeElement;

    if (courseObject.manualTutorialEnrolment) {
        if (courseObject.isTutorialSelected && ((courseObject.selectedTutorialSession === courseObject.selectedLectureSession))) {
            courseObject.satisfied = true;
            $(section).attr("satisfied", true);
            satisfyCourseSections(courseObject);

            if (courseObject.selectedTutorialSession === "F") {
                timeSuffix = "-fall";
            } else {
                timeSuffix = "-spring";
            }

            $.each(courseObject.selectedTutorialTime, function (i, time) {
                timeElement = time + timeSuffix;
                setSatisfaction(timeElement, courseObject.satisfied);
            });

            if (getIsYearSection(section)) {
                timeSuffix = reverseTimeSuffix(timeSuffix);
                $.each(courseObject.selectedTutorialTime, function (i, time) {
                    timeElement = time + timeSuffix;
                    setSatisfaction(timeElement, courseObject.satisfied);
                });
            }

        } else if (courseObject.isLectureSelected && ((courseObject.selectedLectureSession === courseObject.selectedTutorialSession))) {
            courseObject.satisfied = true;
            $(section).attr("satisfied", true);
            satisfyCourseSections(courseObject);

            if (courseObject.selectedLectureSession === "F") {
                timeSuffix = "-fall";
            } else {
                timeSuffix = "-spring";
            }

            $.each(courseObject.selectedTimes, function (i, time) {
                timeElement = time + timeSuffix;
                setSatisfaction(timeElement, courseObject.satisfied);
            });
            if (getIsYearSection(section)) {
                timeSuffix = reverseTimeSuffix(timeSuffix);

                $.each(courseObject.selectedTimes, function (i, time) {
                    timeElement = time + timeSuffix;
                    setSatisfaction(timeElement, courseObject.satisfied);
                });
            }

        }
    }
}

function unsatisfyCourse(courseObject, section) {
    var timeSuffix;
    var timeElement;

    if (courseObject.manualTutorialEnrolment) {
        if (courseObject.selectedLectureSession === "F") {
            timeSuffix = "-fall";
        } else {
            timeSuffix = "-spring";
        }

        if (courseObject.isTutorialSelected && (courseObject.selectedTutorialSession === courseObject.selectedLectureSession)) {
            courseObject.satisfied = false;
            $(section).attr("satisfied", false);
            $(courseObject.selectedTutorial).addClass("clickedSectionUnsatisfied");

            $.each(courseObject.selectedTutorialTime, function (i, time) {
                timeElement = time + timeSuffix;
                setSatisfaction(timeElement, courseObject.satisfied);
            });

            if (getIsYearSection(section)) {
                timeSuffix = reverseTimeSuffix(timeSuffix);

                $.each(courseObject.selectedTutorialTime, function (i, time) {
                    timeElement = time + timeSuffix;
                    setSatisfaction(timeElement, courseObject.satisfied);
                });
            }

        } else if (courseObject.isLectureSelected && (courseObject.selectedLectureSession === courseObject.selectedTutorialSession)) {
            courseObject.satisfied = false;
            $(section).attr("satisfied", false);
            $(courseObject.selectedLecture).addClass("clickedSectionUnsatisfied");

            $.each(courseObject.selectedTimes, function (i, time) {
                timeElement = time + timeSuffix;
                setSatisfaction(timeElement, courseObject.satisfied);
            });

            if (getIsYearSection(section)) {
                timeSuffix = reverseTimeSuffix(timeSuffix);

                $.each(courseObject.selectedTimes, function (i, time) {
                    timeElement = time + timeSuffix;
                    setSatisfaction(timeElement, courseObject.satisfied);
                });
            }
        }
    }
}

function satisfyCourseSections(courseObject) {
    $(courseObject.selectedLecture).removeClass("clickedSectionUnsatisfied");
    $(courseObject.selectedTutorial).removeClass("clickedSectionUnsatisfied");
    $("#" + courseObject.name + "-li" + " li").attr("satisfied", true);
}

function setSatisfaction(timeElement, satisfied) {
    $("#" + timeElement).attr("satisfied", satisfied);
}