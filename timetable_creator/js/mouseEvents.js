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
    var timeElement;
    var timeSuffix;

    $(section).mouseout(function () {
        timeSuffix = getTimeSuffix(section);
        performMouseOut(sectionTimes, timeSuffix);
        if (getIsYearSection(section)) {
            timeSuffix = reverseTimeSuffix(timeSuffix);
            performMouseOut(sectionTimes, timeSuffix);
        }
    });
}

function performMouseOut(sectionTimes, timeSuffix) {
    var timeElement;

    $.each(sectionTimes, function (i, time) {
        timeElement = time + timeSuffix;
        if ($("#" + timeElement).attr("clicked") !== "true") {
            $("#" + timeElement).html("");
            $("#" + timeElement).removeClass("mouseOverGood");
        } else {
            $("#" + timeElement).removeClass("mouseOverConflict");
        }
    });
}

/** Mouse Over Direct Functions **/
function setSectionMouseOver(section, sectionTimes, courseObject) {
    var timeSuffix;

    $(section).mouseover(function () {
        timeSuffix = getTimeSuffix(section);
        performMouseOver(sectionTimes, timeSuffix, courseObject);
        $("#course-info-code").html(courseObject.name);
        $("#course-info-title").html(courseObject.title);
        $("#section-stats-section").html($(this).html());
        $("#section-stats-instructor").html($(this).data("instructor"));
        if (getIsYearSection(section)) {
            timeSuffix = reverseTimeSuffix(timeSuffix);
            performMouseOver(sectionTimes, timeSuffix, courseObject)
            $("#course-info-code").html(courseObject.name);
            $("#course-info-title").html(courseObject.title);
            $("#section-stats-section").html($(this).html());
            $("#section-stats-instructor").html($(this).data("instructor"));
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
            lightUpConflict(timeElement);
        } else {
            lightUpTakeable(timeElement, courseObject.name);
        }
    });
}

/** Mouse Over Direct Functions **/
function setSectionOnClick(section, sectionTimes, courseObject) {
    var isLecture;
    var setHeader;
    var satisfied;
    $(section).click(function () {
        isLecture = section.innerHTML.charAt(0) === "L";
        setHeader = false;
        satisfied = true;
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
                selectUnselectedTutorial(courseObject, section, sectionTimes);
            }
        }
    $("#" + courseObject.name + "-li" + " li[satisfied*='false']").each(function() {
        satisfied = false;
    });

    $("#" + courseObject.name + "-li" + " li[clicked*='true']").each(function() {
        console.log("setting one!");
        if (satisfied) {
            $(this).addClass("clickedLectureTime");
        }
        setHeader = true;
    });

    $("#" + courseObject.name + "-li" + " li[clicked*='false']").each(function() {
        $(this).removeClass("clickedLectureTime");
    });

    if (setHeader) {
        $(courseObject.header).addClass("clicked-header");
    } else {
        $(courseObject.header).removeClass("clicked-header"); 
    }

    });
}

/** Utilities **/

function getIsClicked(timeElement) {
    return $("#" + timeElement).attr("clicked") === "true";
}

function lightUpConflict(timeElement) {
    $("#" + timeElement).addClass("mouseOverConflict");
}

function lightUpTakeable(timeElement, courseCode) {
    $("#" + timeElement).html(courseCode);
    $("#" + timeElement).addClass("mouseOverGood");
}

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

function reverseTimeSuffix(timeSuffix) {
    if (timeSuffix === "-fall") {
        timeSuffix = "-spring";
    } else if (timeSuffix === "-spring") {
        timeSuffix = "-fall";
    }
    return timeSuffix;
}

function getTimeSuffix(section) {
    var timeSuffix;

    if (getIsFallSection(section)) {
        timeSuffix = "-fall";
    } else {
        timeSuffix = "-spring";
    }

    return timeSuffix;
}

function setClickedConflict(courseObject, timeElement, section) {
    var conflictArray = $("#" + timeElement).data("conflictArray");
    console.log(conflictArray);
    if (typeof conflictArray === "undefined") {
        conflictArray = [];
    }
    conflictArray.push(courseObject.name);
    $("#" + timeElement).data("conflictArray", conflictArray);
    $("#" + timeElement).attr("title", conflictArray);
    $("#" + timeElement).addClass("clickedConflictTime");
}

function removeClickedConflict(courseObject, timeElement, section) {
    var index;
    var conflictArray = $("#" + timeElement).data("conflictArray");
    console.log("Removing " + courseObject.name + " from ");
    console.log(conflictArray);
    if (typeof conflictArray === "undefined") {
        console.log("Unexpected case in removeClickedConflict()");
    } else {
        index = conflictArray.indexOf(courseObject.name);
        conflictArray.splice(index, 1);
        $("#" + timeElement).data("conflictArray", conflictArray);
        $("#" + timeElement).attr("title", conflictArray);
        if (conflictArray.length === 0) {
            console.log("Free of conflict");
            $("#" + timeElement).removeClass("clickedConflictTime");
        }
    }
    console.log(conflictArray + " After");
}


/** Lecture Functions **/

function setLectureSession(courseObject, section) {
    if (getIsFallSection(section)) {
        courseObject.selectedLectureSession = "F";
    } else if (getIsSpringSection(section)) {
        courseObject.selectedLectureSession = "S";
    } else {
        courseObject.selectedLectureSession = "Y";
    }
}

function selectUnselectedLecture(courseObject, section, sectionTimes) {
    var timeElement;
    var timeSuffix;
    var isTimeClicked;

    $(section).attr("clicked", "true");
    //$(section).addClass("clickedLectureTime");
    setLectureSession(courseObject, section);
    satisfyCourse(courseObject, section);
    courseObject.selectedLecture = section;
    courseObject.isLectureSelected = true;
    timeSuffix = getTimeSuffix(section);
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
        selectNewLectureSection(section, sectionTimes, courseObject, selectedSession);
    } else {
        courseObject.selectedLecture = null;
        courseObject.selectedLectureSession = null;
        courseObject.selectedTimes = null;
    }
}

function turnLectureOff(courseObject, section, sectionTimes) {
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
            $("#" + timeElement).removeClass("clickedLectureTime");
            $("#" + timeElement).removeClass("clickedSectionUnsatisfied");
        }

        $("#" + timeElement).removeClass("mouseOverConflict");
        $("#" + timeElement).removeClass("mouseOverGood");
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
                $("#" + timeElement).removeClass("clickedLectureTime");
                $("#" + timeElement).removeClass("clickedSectionUnsatisfied");
            }

            $("#" + timeElement).removeClass("mouseOverConflict");
            $("#" + timeElement).removeClass("mouseOverGood");
        });
    }
}

function selectNewLectureSection(section, sectionTimes, courseObject, selectedSession) {
    var timeElement;
    var timeSuffix;
    var isTimeClicked;

    $(section).attr("clicked", "true");

    if(courseObject.selectedLectureSession !== selectedSession) {

        if (selectedSession === "F") {
            courseObject.selectedLectureSession = "F";
        } else if (selectedSession === "S") {
            courseObject.selectedLectureSession = "S";
        } else {
            courseObject.selectedLectureSession = "Y";
        }

    }

    satisfyCourse(courseObject, section);
    courseObject.isLectureSelected = true;
    courseObject.selectedLecture = section;
    courseObject.selectedLectureHeader = courseObject.header;
    courseObject.selectedTimes = sectionTimes;

    // if (section.innerHTML.charAt(0) === "L") {
    //     $(courseObject.header).addClass("clickedSectionSatisfied");
    //     $(courseObject.selectedLecture).addClass("clickedSectionSatisfied");
    // } else {
    //     $(courseObject.header).addClass("clickedSectionSatisfied");
    //     $(courseObject.selectedTutorial).addClass("clickedSectionSatisfied");
    // }

    timeSuffix = getTimeSuffix(section);

    $.each(sectionTimes, function (i, time) {
        timeElement = time + timeSuffix;
        isTimeClicked = getIsClicked(timeElement);

        if (!isTimeClicked) {
            setLectureClicked(courseObject, timeElement, section);
        } else {
            setClickedConflict(courseObject, timeElement, section);
        }

    });

    if (getIsYearSection(section)) {
        timeSuffix = reverseTimeSuffix(timeSuffix);

        $.each(sectionTimes, function (i, time) {
            timeElement = time + timeSuffix;
            isTimeClicked = getIsClicked(timeElement);

            if (!isTimeClicked) {
                setLectureClicked(courseObject, timeElement, section);
            } else {
                setClickedConflict(courseObject, timeElement, section);
            } 

        });
    }
}

function selectUnselectedLectureTimes(sectionTimes, timeSuffix, courseObject, section) {
    var timeElement;
    var isTimeClicked;

    $.each(sectionTimes, function (i, time) {
        timeElement = time + timeSuffix;
        isTimeClicked = getIsClicked(timeElement);

        if (!isTimeClicked) {
            setLectureClicked(courseObject, timeElement, section);
        } else {
            setClickedConflict(courseObject, timeElement, section);
        }

    });

}

function setLectureClicked(courseObject, timeElement, section) {
    if (courseObject.satisfied) {
        $("#" + timeElement).addClass("clickedLectureTime");
    } else {
        $("#" + timeElement).addClass("clickedSectionUnsatisfied");
        $(courseObject.selectedLecture).addClass("clickedSectionUnsatisfied");
    }

    $("#" + timeElement).html(courseObject.name);
    $("#" + timeElement).attr("clicked", "true");
    $("#" + timeElement).removeClass("mouseOverGood");
}

/** Tutorial Functions **/

function setTutorialSession(courseObject, section) {
    if (getIsFallSection(section)) {
        courseObject.selectedTutorialSession = "F";
    } else if (getIsSpringSection(section)) {
        courseObject.selectedTutorialSession = "S";
    } else {
        courseObject.selectedTutorialSession = "Y";
    }
}

function selectUnselectedTutorial(courseObject, section, sectionTimes) {
    var timeElement;
    var timeSuffix;
    var isTimeClicked;

    $(section).attr("clicked", "true");
    setTutorialSession(courseObject, section);
    satisfyCourse(courseObject, section);
    courseObject.selectedTutorial = section;
    courseObject.isTutorialSelected = true;
    timeSuffix = getTimeSuffix(section);
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

function turnTutorialOff(courseObject, section, sectionTimes) {
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
            $("#" + timeElement).removeClass("clickedSectionUnsatisfied");
        }

        $("#" + timeElement).removeClass("mouseOverConflict");
        $("#" + timeElement).removeClass("mouseOverGood");
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
                $("#" + timeElement).removeClass("clickedSectionUnsatisfied");
            }

            $("#" + timeElement).removeClass("mouseOverConflict");
            $("#" + timeElement).removeClass("mouseOverGood");
        });
    }
}

function selectNewTutorialSection(section, sectionTimes, courseObject, selectedSession) {
    var timeElement;
    var timeSuffix;
    var isTimeClicked;

    $(section).attr("clicked", "true");

    if(courseObject.selectedTutorialSession !== selectedSession) {

        if (selectedSession === "F") {
            courseObject.selectedTutorialSession = "F";
        } else if (selectedSession === "S") {
            courseObject.selectedTutorialSession = "S";
        } else if (selectedSession === "Y") {
            courseObject.selectedTutorialSession = "Y";
        }
    }

    satisfyCourse(courseObject, section);
    courseObject.isTutorialSelected = true;
    courseObject.selectedTutorial = section;
    courseObject.selectedTutorialHeader = courseObject.header;
    courseObject.selectedTutorialTime = sectionTimes;

    timeSuffix = getTimeSuffix(section);
    $.each(sectionTimes, function (i, time) {
        timeElement = time + timeSuffix;
        isTimeClicked = getIsClicked(timeElement);

        if (!isTimeClicked) {
            setTutorialClicked(timeElement, courseObject);
        } else {
            setClickedConflict(courseObject, timeElement, section);
        }  

    });

    if (getIsYearSection(section)) {
        timeSuffix = reverseTimeSuffix(timeSuffix);

        $.each(sectionTimes, function (i, time) {
            timeElement = time + timeSuffix;
            isTimeClicked = getIsClicked(timeElement);

            if (!isTimeClicked) {
                setTutorialClicked(timeElement, courseObject);
            } else {
                setClickedConflict(courseObject, timeElement, section);
            }  

        });
    }
}

function selectUnselectedTutorialTimes(courseObject, section, sectionTimes, timeSuffix) {
    var timeElement;
    var isTimeClicked;

    $.each(sectionTimes, function (i, time) {
        timeElement = time + timeSuffix;
        isTimeClicked = getIsClicked(timeElement);

        if (!isTimeClicked) {
            setTutorialClicked(timeElement, courseObject);
        } else {
            setClickedConflict(courseObject, timeElement, section);
        }

        $("#" + timeElement).removeClass("mouseOverGood");
    });

}

function setTutorialClicked(timeElement, courseObject) {
    courseObject.isTutorialSelected = true;
    console.log("name=" + courseObject.name + " " + timeElement);

    $("#" + timeElement).html(courseObject.name);
    $("#" + timeElement).attr("clicked", "true");

    if (courseObject.satisfied) {
        $("#" + timeElement).removeClass("mouseOverGood");
        $("#" + timeElement).addClass("clickedTutorialTime");
    } else {
        $("#" + timeElement).addClass("clickedSectionUnsatisfied");
        $(courseObject.selectedTutorial).addClass("clickedSectionUnsatisfied");
    }
}

function setTutorialUnclicked(timeElement, courseObject) {
    courseObject.isTutorialSelected = false;

    $("#" + timeElement).html("");
    $("#" + timeElement).attr("clicked", "false");
    $("#" + timeElement).removeClass("mouseOverConflict");
    $("#" + timeElement).removeClass("clickedTutorialTime");
    $("#" + timeElement).removeClass("clickedSectionUnsatisfied");

    $(courseObject.selectedTutorial).removeClass("clickedSectionUnsatisfied");
}

/** Course Satisfaction **/

function satisfyCourse(courseObject, section) {
    var timeSuffix;
    var timeElement;

    console.log("Inner: " + courseObject.selectedLectureSession);
    console.log("Inner: " + courseObject.selectedTutorialSession);
    console.log((getSectionSessionFromSection(section) === courseObject.selectedTutorialSession));

    if (courseObject.manualTutorialEnrolment) {
        if (courseObject.isTutorialSelected && (courseObject.selectedTutorialSession === courseObject.selectedLectureSession)) {
            if (courseObject.selectedTutorialSession === "F") {
                timeSuffix = "-fall";
            } else {   
                timeSuffix = "-spring";
            }

            $.each(courseObject.selectedTutorialTime, function (i, time) {
                timeElement = time + timeSuffix;
                if ($("#" + timeElement).hasClass("clickedConflictTime")) {
                } else {
                    $("#" + timeElement).removeClass("clickedSectionUnsatisfied");
                    $("#" + timeElement).removeClass("mouseOverGood");
                    $("#" + timeElement).addClass("clickedTutorialTime");
                }
            });

            if (getIsYearSection(section)) {
                timeSuffix = reverseTimeSuffix(timeSuffix);
                $.each(courseObject.selectedTutorialTime, function (i, time) {
                    timeElement = time + timeSuffix;
                    if ($("#" + timeElement).hasClass("clickedConflictTime")) {
                    } else {
                        $("#" + timeElement).removeClass("clickedSectionUnsatisfied");
                        $("#" + timeElement).removeClass("mouseOverGood");
                        $("#" + timeElement).addClass("clickedTutorialTime");
                    }
                });
            }

            courseObject.satisfied = true;
            $(section).attr("satisfied", "true");
            satisfyCourseSections(courseObject);
        } else if (courseObject.isLectureSelected && (courseObject.selectedLectureSession === courseObject.selectedTutorialSession)) {
            if (courseObject.selectedLectureSession === "F") {
                timeSuffix = "-fall";
            } else {   
                timeSuffix = "-spring";
            }

            $.each(courseObject.selectedTimes, function (i, time) {
                timeElement = time + timeSuffix;

                if ($("#" + timeElement).hasClass("clickedConflictTime")) {
                } else {
                    $("#" + timeElement).removeClass("clickedSectionUnsatisfied");
                    $("#" + timeElement).addClass("clickedLectureTime");
                }

            });
            if (getIsYearSection(section)) {
                timeSuffix = reverseTimeSuffix(timeSuffix);

                $.each(courseObject.selectedTimes, function (i, time) {
                    timeElement = time + timeSuffix;
                    if ($("#" + timeElement).hasClass("clickedConflictTime")) {
                    } else {
                        $("#" + timeElement).removeClass("clickedSectionUnsatisfied");
                        $("#" + timeElement).addClass("clickedLectureTime");
                    }
                });

            }
            courseObject.satisfied = true;
            $(section).attr("satisfied", "true");
            satisfyCourseSections(courseObject);
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
            $.each(courseObject.selectedTutorialTime, function (i, time) {
                timeElement = time + timeSuffix;
                if ($("#" + timeElement).hasClass("clickedConflictTime")) {
                } else {
                    $("#" + timeElement).addClass("clickedSectionUnsatisfied");
                    $("#" + timeElement).removeClass("clickedTutorialTime");
                }
            });

            if (getIsYearSection(section)) {
                timeSuffix = reverseTimeSuffix(timeSuffix);
                $.each(courseObject.selectedTutorialTime, function (i, time) {
                    timeElement = time + timeSuffix;
                    if ($("#" + timeElement).hasClass("clickedConflictTime")) {
                    } else {
                        $("#" + timeElement).addClass("clickedSectionUnsatisfied");
                        $("#" + timeElement).removeClass("clickedTutorialTime");
                    }
                });
            }

            courseObject.satisfied = false;
            $(section).attr("satisfied", "false");
            $(courseObject.selectedTutorial).addClass("clickedSectionUnsatisfied");
        } else if (courseObject.isLectureSelected && (courseObject.selectedLectureSession === courseObject.selectedTutorialSession)) {
            $.each(courseObject.selectedTimes, function (i, time) {
                timeElement = time + timeSuffix;
                if ($("#" + timeElement).hasClass("clickedConflictTime")) {
                } else {
                    $("#" + timeElement).addClass("clickedSectionUnsatisfied");
                    $("#" + timeElement).removeClass("clickedLectureTime");
                }
            });

            if (getIsYearSection(section)) {
                timeSuffix = reverseTimeSuffix(timeSuffix);
                $.each(courseObject.selectedTimes, function (i, time) {
                    timeElement = time + timeSuffix;
                    if ($("#" + timeElement).hasClass("clickedConflictTime")) {
                    } else {
                        $("#" + timeElement).addClass("clickedSectionUnsatisfied");
                        $("#" + timeElement).removeClass("clickedLectureTime");
                    }
                });
            }

            courseObject.satisfied = false;
            $(section).attr("satisfied", "false");
            $(courseObject.selectedLecture).addClass("clickedSectionUnsatisfied");
        }
    }
}

function satisfyCourseSections(courseObject) {
    $(courseObject.selectedLecture).removeClass("clickedSectionUnsatisfied");
    $(courseObject.selectedTutorial).removeClass("clickedSectionUnsatisfied");   
    $("#" + courseObject.name + "-li" + " li").each(function() {
        $(this).attr("satisfied", "true");
    });
}