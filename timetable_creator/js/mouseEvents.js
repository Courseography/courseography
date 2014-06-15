/*jslint todo: true */
/*global $, console*/
/*jslint browser:true */
/*jslint plusplus: true */
"use strict";

function setSectionMouseOut(section, sectionTimes) {
    var timeElement;
    $(section).mouseout(function () {
        $.each(sectionTimes, function (i, time) {
            if ($(section.parentNode).attr("class") === "sectionList-fall") {
                timeElement = time + "-fall";
            } else {
                timeElement = time + "-spring";
            }
            if (document.getElementById(timeElement).getAttribute("clicked") !== "true") {
                document.getElementById(timeElement).innerHTML = "";
                $("#" + timeElement).removeClass("mouseOverGood"); 
            } else {
                $("#" + timeElement).removeClass("mouseOverConflict");
            }
        });
    });
}

function setSectionMouseEvents(section, sectionTimes, courseObject) {
    setSectionOnClick(section, sectionTimes, courseObject);
    setSectionMouseOver(section, sectionTimes, courseObject);
    setSectionMouseOut(section, sectionTimes);
}

function setSectionMouseOver(section, sectionTimes, courseObject) {
    var timeElement;
    $(section).mouseover(function () {
        $.each(sectionTimes, function (i, time) {
            if ($(section.parentNode).attr("class") === "sectionList-fall") {
                timeElement = time + "-fall";
            } else {
                timeElement = time + "-spring";
            }
            if (document.getElementById(timeElement).getAttribute("clicked") === "true") {
                $("#" + timeElement).addClass("mouseOverConflict");
            } else {
                document.getElementById(timeElement).innerHTML = courseObject.code;
                $("#" + timeElement).addClass("mouseOverGood");
            }
        });
        document.getElementById("course-info-code").innerHTML = courseObject.code;
        document.getElementById("course-info-title").innerHTML = courseObject.title;
    });
}

function setSectionOnClick(section, sectionTimes, courseObject) {
    $(section).click(function () {
        var isLecture = section.innerHTML.charAt(0) === "L";
        console.log(courseObject.selectedSession);
        if (courseObject.selected === "true" && isLecture && courseObject.isLectureSelected === "true") {
            $(section).addClass("clickedLectureTime");
            unselectCourse(section, sectionTimes, courseObject);
        } else if (!isLecture) {
            selectTutorial(section, sectionTimes, courseObject);
        } else if (courseObject.selected === "false") {
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
            var indexOfOffender = document.getElementById(timeElement).innerHTML.indexOf(courseObject.code);
            if (indexOfOffender === 0) {
                document.getElementById(timeElement).innerHTML = document.getElementById(timeElement).innerHTML.substring(indexOfOffender, 6);
            } else{
                document.getElementById(timeElement).innerHTML = document.getElementById(timeElement).innerHTML.substring(0, indexOfOffender);
            }
        } else {
            document.getElementById(timeElement).innerHTML = "";
            document.getElementById(timeElement).setAttribute("clicked", "false");
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

        $.each(sectionTimes, function (i, time) {
            if ($(section.parentNode).attr("class") === "sectionList-fall") {
                timeElement = time + "-fall";
            } else {
                timeElement = time + "-spring";
            }
            document.getElementById(timeElement).innerHTML = courseObject.code;
            document.getElementById(timeElement).setAttribute("clicked","true");
            $("#" + timeElement).addClass("clickedLectureTime");
            $(section).attr("clicked", "true"); 
            $("#" + timeElement).removeClass("mouseOverGood");   
        });
    } else {
        courseObject.selected = "false";
        courseObject.selectedLecture = null;
        courseObject.selectedSession = null;

    }
}

function selectTutorial(section, sectionTimes, courseObject) {
    var timeElement;

    $.each(sectionTimes, function (i, time) {
        if ($(section.parentNode).attr("class") === "sectionList-fall") {
            timeElement = time + "-fall";
        } else {
            timeElement = time + "-spring";
        }
        if (document.getElementById(timeElement).getAttribute("clicked") === "true" && document.getElementById(timeElement).innerHTML === courseObject.code) {
            document.getElementById(timeElement).innerHTML = "";
            document.getElementById(timeElement).setAttribute("clicked", "false");
            $("#" + timeElement).removeClass("mouseOverConflict");
            $("#" + timeElement).removeClass("mouseOverGood");
            $("#" + timeElement).removeClass("clickedTutorialTime");
        } else if (document.getElementById(timeElement).getAttribute("clicked") !== "true") {
            document.getElementById(timeElement).innerHTML = courseObject.code;
            document.getElementById(timeElement).setAttribute("clicked","true");
            $("#" + timeElement).addClass("clickedTutorialTime");
        }
        $("#" + timeElement).removeClass("mouseOverGood");
    });
}

function selectUnselectedCourse(courseObject, section, sectionTimes) {
    var timeElement;
    courseObject.selected = "true";
    courseObject.selectedLecture = section;
    if (courseObject.selectedLecture.innerHTML.charAt(0) === "L") {
        courseObject.isLectureSelected = "true";
    } else {
        courseObject.isTutorialSelected = "true";
    }

    courseObject.selectedLectureHeader = header;
    courseObject.selectedTimes = sectionTimes;

    $.each(sectionTimes, function (i, time) {

        if ($(section.parentNode).attr("class") === "sectionList-fall") {
            timeElement = time + "-fall";
        } else {
            timeElement = time + "-spring";
        }
        if (document.getElementById(timeElement).getAttribute("clicked") === "true" && document.getElementById(timeElement).innerHTML === courseObject.code) {
            document.getElementById(timeElement).innerHTML = "";
            document.getElementById(timeElement).setAttribute("clicked", "false");
            $("#" + timeElement).removeClass("clickedLectureTime");
        } else if (document.getElementById(timeElement).getAttribute("clicked") !== "true") {
            document.getElementById(timeElement).innerHTML = courseObject.code;
            document.getElementById(timeElement).setAttribute("clicked","true");
            $("#" + timeElement).addClass("clickedLectureTime");
            $(section).attr("clicked", "true");
            $("#" + timeElement).removeClass("mouseOverGood");
        } else {
            document.getElementById(timeElement).innerHTML = document.getElementById(timeElement).innerHTML + courseObject.code;
            $(section).attr("clicked", "true");
            $("#" + timeElement).addClass("clickedConflictTime");
        }
    });
}