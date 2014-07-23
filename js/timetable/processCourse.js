/*jslint todo: true */
/*global $, console, jQuery*/
/*jslint browser:true */
/*jslint plusplus: true */
"use strict";
var result;
var sections;
var section;
var sectionTimes;

// IAN-TODO Make sure to combine Lecture and Tutorial functions everywhere.
// Get rid of the vars while you are at it.
function processSessionLectures(session, courseObject, timeSuffix) {
    var sectionList = document.createElement("ul");
    $.each(session.lectures, function (i, lecture) {
        if (lecture.section.charAt(1) !== "2" && lecture.time !== "Online Web Version") {
            section = document.createElement("li");
            sectionTimes = convertTimes(lecture.time);
            $(section).data("instructor", lecture.instructor);
            $(section).data("cap", lecture.cap);
            $(section).data("enrol", lecture.enrol);
            $(section).data("wait", lecture.wait);
            section.appendChild(document.createTextNode(lecture.section));
            if (!courseObject.manualTutorialEnrolment && session.tutorials.length > 0) {
                sectionTimes = sectionTimes.concat(convertTimes(session.tutorials[i][0]));
            }
            if (timeSuffix === "Y") {
                var springTimes = [];
                // IAN-TODO Make array be one array.
                $.each(sectionTimes, function(i) {
                    springTimes.push("#" + sectionTimes[i] + "S");
                    sectionTimes[i] = "#" + sectionTimes[i] + "F";
                });
                sectionTimes = sectionTimes.concat(springTimes);

            } else {
                $.each(sectionTimes, function(i) {
                    sectionTimes[i] = "#" + sectionTimes[i] + timeSuffix;
                });
            }
            setSectionMouseEvents(section, sectionTimes, courseObject);
            sectionList.appendChild(section);
        }
    });
    return sectionList;
}

function processSessionTutorials(session, courseObject, sectionList, timeSuffix) {
    $.each(session.tutorials, function (i, tutorial) {
        if (courseObject.manualTutorialEnrolment) {
            section = document.createElement("li");
            sectionTimes = convertTimes(tutorial[1]);
            section.appendChild(document.createTextNode(tutorial[0]));
            if (timeSuffix === "Y") {
                var springTimes = [];
                $.each(sectionTimes, function(i) {
                    springTimes.push("#" + sectionTimes[i] + "S");
                    sectionTimes[i] = "#" + sectionTimes[i] + "F";
                });
                sectionTimes = sectionTimes.concat(springTimes);

            } else {
                $.each(sectionTimes, function(i) {
                    sectionTimes[i] = "#" + sectionTimes[i] + timeSuffix;
                });
            }
            setSectionMouseEvents(section, sectionTimes, courseObject);
            $(section).data("cap", parseInt(tutorial[3]));
            $(section).data("enrol", parseInt(tutorial[4]));
            $(section).data("wait", parseInt(tutorial[5]));
            sectionList.appendChild(section);
        }
    });
    return sectionList;
}

function processSession(courseObject) {
    var sectionList;
    sections = document.createElement("div");
    sections.setAttribute("class", "sections");
    if (typeof courseObject.Y !== "undefined") {
        sectionList = document.createElement("ul");
        sectionList = processSessionLectures(courseObject.Y, courseObject, "Y");
        sectionList = processSessionTutorials(courseObject.Y, courseObject, sectionList, "Y");
        $(sectionList).attr("class", "sectionList-year");
        setSectionIds(courseObject, sectionList, "Y");
        sections.appendChild(sectionList);
    } else {
        if (typeof courseObject.F !== "undefined") {
            sectionList = document.createElement("ul");
            sectionList = processSessionLectures(courseObject.F, courseObject, "F");
            sectionList = processSessionTutorials(courseObject.F, courseObject, sectionList, "F");
            // IAN-TODO Convert all fall, spring and year to F S and Y
            $(sectionList).attr("class", "sectionList-fall");
            setSectionIds(courseObject, sectionList, "F");
            sections.appendChild(sectionList);
        }
        if (typeof courseObject.S !== "undefined") {
            sectionList = document.createElement("ul");
            sectionList = processSessionLectures(courseObject.S, courseObject, "S");
            sectionList = processSessionTutorials(courseObject.S, courseObject, sectionList, "S");
            $(sectionList).attr("class", "sectionList-spring");
            setSectionIds(courseObject, sectionList, "S");
            sections.appendChild(sectionList);
        }
    }
    return sections;
}

function setSectionIds(courseObject, sectionList, sessionSuffix) {
    $(sectionList).children("li").each(function(index, lecture) {
        $(lecture).attr("id", courseObject.name + "-" + $(this).html() + "-" + sessionSuffix);
    });
}
