/*jslint todo: true */
/*global $, console*/
/*jslint browser:true */
/*jslint plusplus: true */
"use strict";

console.log = function() {}

function setSectionMouseEvents(section, sectionTimes, courseObject) {
    setSectionOnClick(section, sectionTimes, courseObject);
    setSectionMouseOver(section, sectionTimes, courseObject);
    setSectionMouseOut(section, sectionTimes);
}

/** Mouse Out Direct Functions **/

function setSectionMouseOut(section, sectionTimes) {
    var timeElement;
    var timeSuffix;

    $(section).mouseout(function () {
        timeSuffix = getTimeSuffix(section);
        performMouseOut(sectionTimes, timeSuffix);
        if (getIsYearSection(section)) {
            timeSuffix = reverseTimeSuffix(timeSuffix);
            performMouseOut(sectionTimes, timeSuffix);
        }
        removeMouseOverClasses();
    });
}

function performMouseOut(sectionTimes, timeSuffix) {
    var timeElement;

    $.each(sectionTimes, function (i, time) {
        timeElement = time + timeSuffix;
        if ($("#" + timeElement).attr("clicked") !== "true") {
            $("#" + timeElement).html("");
        }
    });
}

/** Mouse Over Direct Functions **/

function setSectionMouseOver(section, sectionTimes, courseObject) {
    var timeSuffix;

    $(section).mouseover(function () {
        timeSuffix = getTimeSuffix(section);
        performMouseOver(sectionTimes, timeSuffix, courseObject);
        displayCourseInformation(courseObject, $(this));
        if (getIsYearSection(section)) {
            timeSuffix = reverseTimeSuffix(timeSuffix);
            performMouseOver(sectionTimes, timeSuffix, courseObject);
            displayCourseInformation(courseObject, $(this));
        }
    });
}

function performMouseOver(sectionTimes, timeSuffix, courseObject) {
    var timeElement;
    var isTimeClicked;

    $.each(sectionTimes, function (i, time) {
        timeElement = time + timeSuffix;
        isTimeClicked = getIsClicked(timeElement);
        if (isTimeClicked) {
            lightUpConflict(courseObject, timeElement);
        } else {
            lightUpTakeable(courseObject, timeElement);
        }
    });
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
        $("#" + timeElement).addClass("mouseOverTaken");
    } else {
        $("#" + timeElement).addClass("mouseOverGood");
    }
    $("#" + timeElement).html(courseObject.name);
}

function removeMouseOverClasses() {
    $("td").each(function (i, time) {
        $(time).removeClass("mouseOverConflict");
        $(time).removeClass("mouseOverGood");
        $(time).removeClass("mouseOverTaken");
        $(time).removeClass("mouseOverRemove");
    });
}

function displayCourseInformation(courseObject, section) {
    $("#course-info-code").html(courseObject.name);
    $("#course-info-title").html(courseObject.title);
    $("#section-stats-section").html(section.html());
    $("#section-stats-instructor").html(section.data("instructor"));
    var cap = section.data("cap");
    var enrol = section.data("enrol");
    if (cap !== null && enrol !== null) {
        var enrolString = (cap - enrol) + " out of " + cap + " spots remaining";
        $("#section-stats-enrol").html(enrolString);
    }
}

/** Mouse Click Direct Functions **/

function setSectionOnClick(section, sectionTimes, courseObject) {
    var isLecture;
    var taken;
    var satisfied;
    var inConflict;
    $(section).click(function () {
        console.log("> click : " + courseObject.name);
        isLecture = section.innerHTML.charAt(0) === "L";
        //courseObject.satisfied = !courseObject.manualTutorialEnrolment;
        taken = false;
        satisfied = true;
        inConflict = false;
        if (isLecture) {

            if (courseObject.isLectureSelected) {
                selectAlreadySelectedLecture(courseObject, section, sectionTimes);
            } else {
                setLectureSession(courseObject, section);

                selectUnselectedLecture(courseObject, section, sectionTimes);
            }
        } else {

            if (courseObject.isTutorialSelected) {
                selectAlreadySelectedTutorial(courseObject, section, sectionTimes);
            } else {
                setTutorialSession(courseObject, section);

                selectUnselectedTutorial(courseObject, section, sectionTimes);
            }

        }

        $("#" + courseObject.name + "-li" + " li[satisfied*='false']").each(function() {
            satisfied = false;
        });

        $("#" + courseObject.name + "-li" + " li[clicked*='true']").each(function() {
            if (satisfied) {
                $(this).addClass("clickedLectureTime");
            }

            var index = $.inArray($(this).attr("id"), selectedLectures);

            if (!(index > -1)) {
                    selectedLectures.push($(this).attr("id"));
            }

            taken = true;
        });

        $("#" + courseObject.name + "-li" + " li[clicked*='false']").each(function() {
            $(this).removeClass("clickedLectureTime");
            var index = $.inArray($(this).attr("id"), selectedLectures);

            if (index > -1) {
                selectedLectures.splice(index, 1);
            }

        });

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

        setHeader(courseObject, taken, satisfied);
        setCookie("selected-lectures", JSON.stringify(selectedLectures));
        removeMouseOverClasses();
        inConflict = getInConflict(inConflict);
        alertUserOfConflict(inConflict);
        console.log("< click : " + courseObject.name);
    });
}

/** Utilities **/

function setHeader(courseObject, taken, satisfied) {
    console.log("> setHeader");
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
    console.log("< setHeader");
}

function getInConflict(inConflict) {
    $("td[class*=clickedConflictTime]").each(function() {
        inConflict = true;
    });

    return inConflict;
}

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

function getSectionSessionFromSection(section) {
    console.log("> getSectionSessionFromSection");
    if (getIsYearSection(section)) {
        return "Y";
    } else if (getIsFallSection(section)) {
        return "F";
    } else if (getIsSpringSection(section)) {
        return "S";
    }
    console.log("< getSectionSessionFromSection");
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

function reverseTimeSuffix(timeSuffix) {
    console.log("> reverseTimeSuffix");
    if (timeSuffix === "-fall") {
        timeSuffix = "-spring";
    } else if (timeSuffix === "-spring") {
        timeSuffix = "-fall";
    }
    console.log("< reverseTimeSuffix");
    return timeSuffix;
}

function getTimeSuffix(section) {
    console.log("> getTimeSuffix");
    var timeSuffix;

    if (getIsFallSection(section)) {
        timeSuffix = "-fall";
    } else {
        timeSuffix = "-spring";
    }

    console.log("< getTimeSuffix");
    return timeSuffix;
}

function setClickedConflict(courseObject, timeElement, section, type) {
    console.log("> setClickedConflict");
    var conflictArray = $("#" + timeElement).data("conflictArray");
    var typeArray = $("#" + timeElement).data("typeArray");
    if (typeof conflictArray === "undefined") {
        conflictArray = [];
    }

    if (typeof typeArray === "undefined") {
        typeArray = [];
    }

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
    $("#" + timeElement).attr("in-conflict", true);
    console.log("< setClickedConflict");
}

function removeClickedConflict(courseObject, timeElement, section) {
    console.log("> removeClickedConflict");
    var index;
    var conflictArray = $("#" + timeElement).data("conflictArray");
    var typeArray = $("#" + timeElement).data("typeArray");
    if (typeof conflictArray === "undefined" || typeof typeArray === "undefined") {
        console.log("Unexpected case in removeClickedConflict()");
    } else {
        index = conflictArray.indexOf(courseObject.name);
        if ($("#" + timeElement).html() === courseObject.name) {
            $("#" + timeElement).html(conflictArray[0]);
            $("#" + timeElement).attr("type", typeArray[0]);
        }
        if (conflictArray.length === 1 ) {
            $("#" + timeElement).attr("in-conflict", false);
        }
        conflictArray.splice(index, 1);
        typeArray.splice(index, 1);
        var newCourseObject = getCourseObject($("#" + timeElement).html());
        $("#" + timeElement).attr("satisfied", newCourseObject.satisfied);
        $("#" + timeElement).data("conflictArray", conflictArray);
        $("#" + timeElement).attr("title", conflictArray);
    }
    console.log("< removeClickedConflict");
}


/** Lecture Functions **/

function setLectureSession(courseObject, section) {
    console.log("> setLectureSession");
    courseObject.selectedLectureSession = getSectionSessionFromSection(section);
    console.log("< setLectureSession");
}

function selectUnselectedLecture(courseObject, section, sectionTimes) {
    console.log("> selectUnselectedLecture");
    var timeElement;
    var timeSuffix;
    var isTimeClicked;

    $(section).attr("clicked", "true");
    satisfyCourse(courseObject, section);
    setLectureSession(courseObject, section);
    courseObject.selectedLecture = section;
    courseObject.isLectureSelected = true;
    timeSuffix = getTimeSuffix(section);
    selectUnselectedLectureTimes(sectionTimes, timeSuffix, courseObject, section);

    if (getIsYearSection(section)) {
        timeSuffix = reverseTimeSuffix(timeSuffix);
        selectUnselectedLectureTimes(sectionTimes, timeSuffix, courseObject, section);
    }

    courseObject.selectedTimes = sectionTimes;
    satisfyCourse(courseObject, section);
    console.log("< selectUnselectedLecture");
}

function selectAlreadySelectedLecture(courseObject, section, sectionTimes) {
    console.log("> selectAlreadySelectedLecture");
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
        selectNewLectureSection(section, sectionTimes, courseObject, selectedSession);
    } else {
        courseObject.selectedLecture = null;
        courseObject.selectedLectureSession = null;
        courseObject.selectedTimes = null;
    }
    console.log("< selectAlreadySelectedLecture");
}

function turnLectureOff(courseObject, section, sectionTimes) {
    console.log("> turnLectureOff");
    var timeSuffix;
    var timeElement;

    courseObject.isLectureSelected = false;
    unsatisfyCourse(courseObject, section);

    $(courseObject.selectedLecture).attr("clicked", "false");

    if (courseObject.selectedLectureSession === "F") {
        timeSuffix = "-fall";
    } else {
        timeSuffix = "-spring";
    }

    $.each(courseObject.selectedTimes, function (i, time) {
        timeElement = time + timeSuffix;

        if ($("#" + timeElement).hasClass("clickedConflictTime")) {
            removeClickedConflict(courseObject, timeElement, section);
        } else {
            $("#" + timeElement).html("");
            $("#" + timeElement).attr("clicked", "false");
        }
    });

    $(courseObject.selectedLecture).removeClass("clickedSectionUnsatisfied");

    if (getIsYearSection(section)) {
        timeSuffix = reverseTimeSuffix(timeSuffix);

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
    console.log("< turnLectureOff");
}

function selectNewLectureSection(section, sectionTimes, courseObject, selectedSession) {
    console.log("> selectNewLectureSection");
    var timeElement;
    var timeSuffix;
    var isTimeClicked;

    $(section).attr("clicked", "true");

    if(courseObject.selectedLectureSession !== selectedSession) {
        courseObject.selectedLectureSession = selectedSession;
    }

    courseObject.isLectureSelected = true;
    courseObject.selectedLecture = section;
    courseObject.selectedLectureHeader = courseObject.header;
    courseObject.selectedTimes = sectionTimes;

    timeSuffix = getTimeSuffix(section);

    satisfyCourse(courseObject, section);
    console.log(courseObject.selectedTutorialSession === courseObject.selectedLectureSession);
    selectUnselectedLectureTimes(sectionTimes, timeSuffix, courseObject, section);

    if (getIsYearSection(section)) {
        timeSuffix = reverseTimeSuffix(timeSuffix);

        selectUnselectedLectureTimes(sectionTimes, timeSuffix, courseObject, section);
    }
    console.log("< selectNewLectureSection");
}

function selectUnselectedLectureTimes(sectionTimes, timeSuffix, courseObject, section) {
    console.log("> selectUnselectedLectureTimes");
    var timeElement;
    var isTimeClicked;

    $.each(sectionTimes, function (i, time) {
        timeElement = time + timeSuffix;
        isTimeClicked = getIsClicked(timeElement);

        if (!isTimeClicked) {
            setLectureClicked(courseObject, timeElement, section);
        } else {
            setClickedConflict(courseObject, timeElement, section, "lecture");
        }

    });
    console.log("< selectUnselectedLectureTimes");
}

function setLectureClicked(courseObject, timeElement, section) {
    console.log("> setLectureClicked");
    $("#" + timeElement).attr("type", "lecture");

    if (!courseObject.satisfied) {
        setSatisfaction(courseObject, timeElement);
        $(courseObject.selectedLecture).attr("satisfied", "false");
        $(courseObject.selectedLecture).addClass("clickedSectionUnsatisfied");
    }

    $("#" + timeElement).html(courseObject.name);
    $("#" + timeElement).attr("clicked", "true");
    console.log("< setLectureClicked");
}

/** Tutorial Functions **/

function setTutorialSession(courseObject, section) {
    courseObject.selectedTutorialSession = getSectionSessionFromSection(section);
}

function selectUnselectedTutorial(courseObject, section, sectionTimes) {
    console.log("> selectUnselectedTutorial");
    var timeElement;
    var timeSuffix;
    var isTimeClicked;

    $(section).attr("clicked", "true");
    setTutorialSession(courseObject, section);
    courseObject.selectedTutorial = section;
    satisfyCourse(courseObject, section);
    courseObject.isTutorialSelected = true;
    timeSuffix = getTimeSuffix(section);

    selectUnselectedTutorialTimes(courseObject, section, sectionTimes, timeSuffix);

    if (getIsYearSection(section)) {
        timeSuffix = reverseTimeSuffix(timeSuffix);
        selectUnselectedTutorialTimes(courseObject, section, sectionTimes, timeSuffix);
    }

    courseObject.selectedTutorialTime = sectionTimes;
    console.log("< selectUnselectedTutorial");
}

function selectAlreadySelectedTutorial(courseObject, section, sectionTimes) {
    console.log("> selectAlreadySelectedTutorial");
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
    console.log("< selectAlreadySelectedTutorial");
}

function turnTutorialOff(courseObject, section, sectionTimes) {
    console.log("> turnTutorialOff");
    var timeSuffix;
    var timeElement;

    courseObject.isTutorialSelected = false;
    unsatisfyCourse(courseObject, section);

    $(courseObject.selectedTutorial).attr("clicked", "false");

    if (courseObject.selectedTutorialSession === "F") {
        timeSuffix = "-fall";
    } else {
        timeSuffix = "-spring";
    }

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
    console.log("< turnTutorialOff");
}

function selectNewTutorialSection(section, sectionTimes, courseObject, selectedSession) {
    console.log("> selectNewTutorialSection");
    var timeElement;
    var timeSuffix;
    var isTimeClicked;

    $(section).attr("clicked", "true");

    if(courseObject.selectedTutorialSession !== selectedSession) {
        courseObject.selectedTutorialSession = selectedSession;
    }

    satisfyCourse(courseObject, section);
    courseObject.isTutorialSelected = true;
    courseObject.selectedTutorial = section;
    courseObject.selectedTutorialHeader = courseObject.header;
    courseObject.selectedTutorialTime = sectionTimes;

    timeSuffix = getTimeSuffix(section);
    selectUnselectedTutorialTimes(courseObject, section, sectionTimes, timeSuffix);

    if (getIsYearSection(section)) {
        timeSuffix = reverseTimeSuffix(timeSuffix);
        selectUnselectedTutorialTimes(courseObject, section, sectionTimes, timeSuffix);
    }
    console.log("< selectNewTutorialSection");
}

function selectUnselectedTutorialTimes(courseObject, section, sectionTimes, timeSuffix) {
    console.log("> selectUnselectedTutorialTimes");
    var timeElement;
    var isTimeClicked;

    $.each(sectionTimes, function (i, time) {
        timeElement = time + timeSuffix;
        isTimeClicked = getIsClicked(timeElement);

        if (!isTimeClicked) {
            setTutorialClicked(timeElement, courseObject);
        } else {
            setClickedConflict(courseObject, timeElement, section, "tutorial");
        }
    });

    console.log("< selectUnselectedTutorialTimes");
}

function setTutorialClicked(timeElement, courseObject) {
    console.log("> setTutorialClicked");
    courseObject.isTutorialSelected = true;

    $("#" + timeElement).attr("type", "tutorial");
    $("#" + timeElement).html(courseObject.name);
    $("#" + timeElement).attr("clicked", "true");

    if (!courseObject.satisfied) {
        setSatisfaction(courseObject, timeElement);
        $(courseObject.selectedTutorial).addClass("clickedSectionUnsatisfied");
        $(courseObject.selectedTutorial).attr("satisfied", "false");
    }
    console.log("< setTutorialClicked");
}

function setTutorialUnclicked(timeElement, courseObject) {
    console.log("> setTutorialUnclicked");
    courseObject.isTutorialSelected = false;

    $("#" + timeElement).html("");
    $("#" + timeElement).attr("clicked", "false");
    $("#" + timeElement).removeClass("clickedTutorialTime");
    // $("#" + timeElement).removeClass("clickedSectionUnsatisfied");

    $(courseObject.selectedTutorial).removeClass("clickedSectionUnsatisfied");
    console.log("< setTutorialUnclicked");
}

/** Course Satisfaction **/

function satisfyCourse(courseObject, section) {
    console.log("> satisfyCourse");
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
                setSatisfaction(courseObject, timeElement);
            });

            if (getIsYearSection(section)) {
                timeSuffix = reverseTimeSuffix(timeSuffix);
                $.each(courseObject.selectedTutorialTime, function (i, time) {
                    timeElement = time + timeSuffix;
                    setSatisfaction(courseObject, timeElement);
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
                setSatisfaction(courseObject, timeElement);
            });
            if (getIsYearSection(section)) {
                timeSuffix = reverseTimeSuffix(timeSuffix);

                $.each(courseObject.selectedTimes, function (i, time) {
                    timeElement = time + timeSuffix;
                    setSatisfaction(courseObject, timeElement);
                });
            }

        }
    }
    console.log("< satisfyCourse");
}

function unsatisfyCourse(courseObject, section) {
    console.log("> unsatisfyCourse");
    var timeSuffix;
    var timeElement;
    console.log("unsatisfying");
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
                setSatisfaction(courseObject, timeElement);
            });

            if (getIsYearSection(section)) {
                timeSuffix = reverseTimeSuffix(timeSuffix);

                $.each(courseObject.selectedTutorialTime, function (i, time) {
                    timeElement = time + timeSuffix;
                    setSatisfaction(courseObject, timeElement);
                });
            }

        } else if (courseObject.isLectureSelected && (courseObject.selectedLectureSession === courseObject.selectedTutorialSession)) {
            courseObject.satisfied = false;
            $(section).attr("satisfied", false);
            $(courseObject.selectedLecture).addClass("clickedSectionUnsatisfied");

            $.each(courseObject.selectedTimes, function (i, time) {
                timeElement = time + timeSuffix;
                setSatisfaction(courseObject, timeElement);
            });

            if (getIsYearSection(section)) {
                timeSuffix = reverseTimeSuffix(timeSuffix);

                $.each(courseObject.selectedTimes, function (i, time) {
                    timeElement = time + timeSuffix;
                    setSatisfaction(courseObject, timeElement);
                });
            }
        }
    }
    console.log("< unsatisfyCourse");
}

function satisfyCourseSections(courseObject) {
    $(courseObject.selectedLecture).removeClass("clickedSectionUnsatisfied");
    $(courseObject.selectedTutorial).removeClass("clickedSectionUnsatisfied");
    $("#" + courseObject.name + "-li" + " li").each(function() {
        $(this).attr("satisfied", true);
    });
}

function setSatisfaction(courseObject, timeElement) {
    $("#" + timeElement).attr("satisfied", courseObject.satisfied);
}
