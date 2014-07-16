/*jslint todo: true */
/*global $, console, jQuery*/
/*jslint browser:true */
/*jslint plusplus: true */
"use strict";
var result;
var sections;
var section;
var sectionTimes;

function processSessionLectures(session, courseObject) {
    var sectionList = document.createElement("ul");
    $.each(session.lectures, function (i, lecture) {
        if (lecture.section.charAt(1) !== "2" && lecture.time !== "Online Web Version") {
            section = document.createElement("li");
            sectionTimes = convertTimes(lecture.time);
            $(section).data("instructor", lecture.instructor);
            $(section).data("cap", lecture.cap);
            $(section).data("enrol", lecture.enrol);
            section.appendChild(document.createTextNode(lecture.section));
            if (!courseObject.manualTutorialEnrolment && session.tutorials.length > 0) {
                sectionTimes = sectionTimes.concat(convertTimes(session.tutorials[i][0]));
            }
            setSectionMouseEvents(section, sectionTimes, courseObject);
            sectionList.appendChild(section);
        }
    });
    return sectionList;
}

function processSessionTutorials(session, courseObject, sectionList) {
    $.each(session.tutorials, function (i, tutorial) {
        if (courseObject.manualTutorialEnrolment) {
            section = document.createElement("li");
            sectionTimes = convertTimes(tutorial[1]);
            section.appendChild(document.createTextNode(tutorial[0]));
            setSectionMouseEvents(section, sectionTimes, courseObject);
            $(section).data("cap", tutorial[2]);
            $(section).data("enrol", tutorial[3]);
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
        sectionList = processSessionLectures(courseObject.Y, courseObject);
        sectionList = processSessionTutorials(courseObject.Y, courseObject, sectionList);
        $(sectionList).attr("class", "sectionList-year");
        setSectionIds(courseObject, sectionList, "Y");
        sections.appendChild(sectionList);
    } else {
        if (typeof courseObject.F !== "undefined") {
            sectionList = document.createElement("ul");
            sectionList = processSessionLectures(courseObject.F, courseObject);
            sectionList = processSessionTutorials(courseObject.F, courseObject, sectionList);
            $(sectionList).attr("class", "sectionList-fall");
            setSectionIds(courseObject, sectionList, "F");
            sections.appendChild(sectionList);
        }
        if (typeof courseObject.S !== "undefined") {
            sectionList = document.createElement("ul");
            sectionList = processSessionLectures(courseObject.S, courseObject);
            sectionList = processSessionTutorials(courseObject.S, courseObject, sectionList);
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
