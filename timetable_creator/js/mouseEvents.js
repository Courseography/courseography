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
    var timeSuffix = getTimeSuffix(section);
    $(section).mouseout(function () {
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
    var timeSuffix = getTimeSuffix(section);
    $(section).mouseover(function () {
        timeElement = time + timeSuffix;
        $.each(sectionTimes, function (i, time) {
            timeElement = time + timeSuffix;
            if ($("#" + timeElement).attr("clicked") === "true") {
                $("#" + timeElement).addClass("mouseOverConflict");
            } else {
                $("#" + timeElement).html(courseObject.code);
                $("#" + timeElement).addClass("mouseOverGood");
            }
        });
        $("course-info-code").html(courseObject.code);
        $("course-info-title").html(courseObject.title);
    });
}

function setSectionOnClick(section, sectionTimes, courseObject) {
    $(section).click(function () {
        var isLecture = section.innerHTML.charAt(0) === "L";
        if (courseObject.selected && isLecture && courseObject.isLectureSelected) {
            $(section).addClass("clickedLectureTime");
            unselectCourse(section, sectionTimes, courseObject);
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

function unselectCourse(section, sectionTimes, courseObject) {
    var timeElement;
    var timeSuffix;
    var selectedSession;
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
            var indexOfOffender = $("#" + timeElement).html().indexOf(courseObject.code);
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

    if ($(section.parentNode).attr("class") === "sectionList-fall") {
        selectedSession = "F";
    } else {
        selectedSession = "S";
    }

    if (courseObject.selectedLecture.innerHTML !== section.innerHTML || courseObject.selectedSession !== selectedSession) {
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
            $("#" + timeElement).attr("clicked","true");
            $("#" + timeElement).addClass("clickedLectureTime");
            $(section).attr("clicked", "true"); 
            $("#" + timeElement).removeClass("mouseOverGood");   
        });
    } else {
        courseObject.selected = false;
        courseObject.selectedLecture = null;
        courseObject.selectedSession = null;
    }
}

function selectTutorial(section, sectionTimes, courseObject) {
    var timeElement;
    var timeSuffix;
    timeSuffix = getTimeSuffix(section);
    $.each(sectionTimes, function (i, time) {
        timeElement = time + timeSuffix;
        if ($("#" + timeElement).attr("clicked") === "true" && $("#" + timeElement).html() === courseObject.code) {
            $("#" + timeElement).html = "";
            $("#" + timeElement).attr("clicked", "false");
            $("#" + timeElement).removeClass("mouseOverConflict");
            $("#" + timeElement).removeClass("mouseOverGood");
            $("#" + timeElement).removeClass("clickedTutorialTime");
        } else if ($("#" + timeElement).attr("clicked") !== "true") {
            $("#" + timeElement).html(courseObject.code);
            $("#" + timeElement).attr("clicked", "true");
            $("#" + timeElement).addClass("clickedTutorialTime");
        }
        $("#" + timeElement).removeClass("mouseOverGood");
    });
}

function selectUnselectedCourse(courseObject, section, sectionTimes) {
    var timeElement;
    courseObject.selected = true;
    courseObject.selectedLecture = section;

    if (courseObject.selectedLecture.innerHTML.charAt(0) === "L") {
        courseObject.isLectureSelected = true;
    } else {
        courseObject.isTutorialSelected = true;
    }

    // header is not defined.
    courseObject.selectedLectureHeader = header;
    courseObject.selectedTimes = sectionTimes;
    var timeSuffix = getTimeSuffix(section);
    $.each(sectionTimes, function (i, time) {
        timeElement = time + timeSuffix;
        if ($("#" + timeElement).attr("clicked") === "true" && $("#" + timeElement).html() === courseObject.code) {
            $("#" + timeElement).html("");
            $("#" + timeElement).attr("clicked", "false");
            $("#" + timeElement).removeClass("clickedLectureTime");
        } else if ($("#" + timeElement).attr("clicked") !== "true") {
            $("#" + timeElement).html(courseObject.code);
            $("#" + timeElement).attr("clicked", "true");
            $("#" + timeElement).addClass("clickedLectureTime");
            $(section).attr("clicked", "true");
            $("#" + timeElement).removeClass("mouseOverGood");
        } else {
            $("#" + timeElement).html($("#" + timeElement).html() + courseObject.code);
            $(section).attr("clicked", "true");
            $("#" + timeElement).addClass("clickedConflictTime");
        }
    });
}

function getTimeSuffix(section) {
    var timeSuffix;
    if ($(section.parentNode).attr("class") === "sectionList-fall") {
        timeSuffix = "-fall";
    } else {
        timeSuffix = "-spring";
    }
    return timeSuffix;
}