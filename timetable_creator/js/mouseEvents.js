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


function setSectionMouseOut(section, sectionTimes) {
    var timeElement;
    var timeSuffix;

    $(section).mouseout(function () {
        timeSuffix = getTimeSuffix(section);
        $.each(sectionTimes, function (i, time) {
            timeElement = time + timeSuffix;
            if ($("#" + timeElement).attr("clicked") !== "true") {
                $("#" + timeElement).html("");
                $("#" + timeElement).removeClass("mouseOverGood");
            } else {
                $("#" + timeElement).removeClass("mouseOverConflict");
            }
        });
    });
}

function setSectionMouseOver(section, sectionTimes, courseObject) {
    var timeElement;
    var isTimeClicked;
    var timeSuffix;

    $(section).mouseover(function () {
        timeSuffix = getTimeSuffix(section);
        $.each(sectionTimes, function (i, time) {
            timeElement = time + timeSuffix;
            isTimeClicked = getIsClicked(timeElement);
            if (isTimeClicked) {
                lightUpConflict(timeElement);
            } else {
                lightUpTakeable(timeElement, courseObject.code);
            }
        });
        setMouseOverCourseInfo(courseObject);
    });
}

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

function setMouseOverCourseInfo(courseObject) {
    $("#course-info-code").html(courseObject.code);
    $("#course-info-title").html(courseObject.title);
}

function setSectionOnClick(section, sectionTimes, courseObject) {
    var isLecture;

    $(section).click(function () {
        isLecture = section.innerHTML.charAt(0) === "L";
        if (courseObject.selected && isLecture) {
            $(section).addClass("clickedLectureTime");
            selectAlreadySelectedCourse(section, sectionTimes, courseObject);
        } else if (!isLecture) {
            selectTutorial(section, sectionTimes, courseObject);
        } else if (!courseObject.selected) {
            $(section).addClass("clickedLectureTime");
            if ($(section.parentNode).attr("class") === "sectionList-fall") {
                courseObject.selectedSession = "F";
            } else {
                courseObject.selectedSession = "S";
            }
            selectUnselectedCourse(courseObject, section, sectionTimes);
        } else {
            console.log("Uncaught section click case for: " + courseObject.code);
        }
    });
}

function selectAlreadySelectedCourse(section, sectionTimes, courseObject) {
    var selectedSession;

    turnCourseOff(section, sectionTimes, courseObject);

    if ($(section.parentNode).attr("class") === "sectionList-fall") {
        selectedSession = "F";
    } else {
        selectedSession = "S";
    }

    if (courseObject.selectedLecture.innerHTML !== section.innerHTML || courseObject.selectedSession !== selectedSession) {
        selectNewSection(section, sectionTimes, courseObject, selectedSession);
    } else {
        courseObject.selected = false;
        courseObject.selectedLecture = null;
        courseObject.selectedSession = null;
    }
}

function turnCourseOff(section, sectionTimes, courseObject) {
    var timeSuffix;
    var timeElement;
    var indexOfOffender;

    // TODO: Adapt timeElement to hold Y courses as well.
    if (courseObject.selectedSession === "F") {
        timeSuffix = "-fall";
    } else {   
        timeSuffix = "-spring";
    }

    $.each(courseObject.selectedTimes, function (i, time) {
        timeElement = time + timeSuffix;
        if ($("#" + timeElement).hasClass("clickedConflictTime")) {
            $("#" + timeElement).removeClass("clickedConflictTime");
            indexOfOffender = $("#" + timeElement).html().indexOf(courseObject.code);
            if (indexOfOffender === 0) {
                $("#" + timeElement).html($("#" + timeElement).html().substring(6));
            } else{
                $("#" + timeElement).html($("#" + timeElement).html().substring(0, indexOfOffender));
            }
        } else {
            $("#" + timeElement).html("");
            $("#" + timeElement).attr("clicked", "false");
            $("#" + timeElement).removeClass("clickedLectureTime");
        }
        $("#" + timeElement).removeClass("mouseOverConflict");
        $("#" + timeElement).removeClass("mouseOverGood");
    });
}

function selectNewSection(section, sectionTimes, courseObject, selectedSession) {
    var timeElement;
    var timeSuffix;

    if(courseObject.selectedSession !== selectedSession) {
        if (selectedSession === "F") {
            courseObject.selectedSession = "F";
        } else {
            courseObject.selectedSession = "S";
        }
    }
    courseObject.selectedLecture = section;
    courseObject.selectedLectureHeader = courseObject.header;
    courseObject.selectedTimes = sectionTimes;

    if (section.innerHTML.charAt(0) === "L") {
        // $(courseObject.header).addClass("selectedLectureSection");
    } else {
        // $(courseObject.header).addClass("selectedTutorialSection");
    }

    timeSuffix = getTimeSuffix(section);
    $.each(sectionTimes, function (i, time) {
        timeElement = time + timeSuffix;
        $("#" + timeElement).html(courseObject.code);
        $("#" + timeElement).attr("clicked", "true");
        $("#" + timeElement).addClass("clickedLectureTime");
        $(section).attr("clicked", "true"); 
        $("#" + timeElement).removeClass("mouseOverGood");   
    });
}

function selectTutorial(section, sectionTimes, courseObject) {
    var timeElement;
    var timeSuffix;
    var isTimeClicked;

    timeSuffix = getTimeSuffix(section);
    $.each(sectionTimes, function (i, time) {
        timeElement = time + timeSuffix;
        isTimeClicked = getIsClicked(timeElement);
        if (isTimeClicked && $("#" + timeElement).html() === courseObject.code) {
            setTutorialUnclicked(timeElement);
        } else if (!isTimeClicked) {
            setTutorialClicked(timeElement, courseObject);
        }
        $("#" + timeElement).removeClass("mouseOverGood");
    });
}

function setTutorialClicked(timeElement, courseObject) {
    $("#" + timeElement).html(courseObject.code);
    $("#" + timeElement).attr("clicked", "true");
    $("#" + timeElement).addClass("clickedTutorialTime");
}

function setTutorialUnclicked(timeElement) {
    $("#" + timeElement).html = "";
    $("#" + timeElement).attr("clicked", "false");
    $("#" + timeElement).removeClass("mouseOverConflict");
    $("#" + timeElement).removeClass("clickedTutorialTime");
}

function selectUnselectedCourse(courseObject, section, sectionTimes) {
    var timeElement;
    var timeSuffix;
    var isTimeClicked;

    courseObject.selected = true;
    courseObject.selectedLecture = section;

    // if (courseObject.selectedLecture.innerHTML.charAt(0) === "L") {
    //     courseObject.isLectureSelected = true;
    // } else {
    //     courseObject.isTutorialSelected = true;
    // }

    // header is not defined.
    // courseObject.selectedLectureHeader = header;
    courseObject.selectedTimes = sectionTimes;
    timeSuffix = getTimeSuffix(section);
    $.each(sectionTimes, function (i, time) {
        timeElement = time + timeSuffix;
        isTimeClicked = getIsClicked(timeElement);
        if (isTimeClicked && $("#" + timeElement).html() === courseObject.code) {
            $("#" + timeElement).html("");
            $("#" + timeElement).attr("clicked", "false");
            $("#" + timeElement).removeClass("clickedLectureTime");
        } else if (!isTimeClicked) {
            setNewClickedCourse(courseObject, timeElement, section);
        } else {
            setNewClickedConflict(courseObject, timeElement, section);
        }
    });
}

function setNewClickedCourse(courseObject, timeElement, section) {
    $("#" + timeElement).html(courseObject.code);
    $("#" + timeElement).attr("clicked", "true");
    $("#" + timeElement).addClass("clickedLectureTime");
    $(section).attr("clicked", "true");
    $("#" + timeElement).removeClass("mouseOverGood");
}

function setNewClickedConflict(courseObject, timeElement, section) {
    $("#" + timeElement).html($("#" + timeElement).html() + courseObject.code);
    $(section).attr("clicked", "true");
    $("#" + timeElement).addClass("clickedConflictTime");
}

function getTimeSuffix(section) {
    var timeSuffix;

    if ($(section.parentNode).hasClass("sectionList-fall")) {
        timeSuffix = "-fall";
    } else if ($(section.parentNode).attr("class") === "sectionList-spring") {
        timeSuffix = "-spring";
    }
    return timeSuffix;
}

