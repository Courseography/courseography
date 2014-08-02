function processSessionSections(session, course, timeSuffix) {
    var sectionList = document.createElement("ul");
    $.each(session.lectures, function (i, lecture) {
        if (lecture.section.charAt(1) !== "2" && lecture.time !==
            "Online Web Version") {
            var section = document.createElement("li");
            var sectionTimes = convertTimes(lecture.time);
            $(section).data("instructor", lecture.instructor);
            $(section).data("cap", lecture.cap);
            $(section).data("enrol", lecture.enrol);
            $(section).data("wait", lecture.wait);
            section.appendChild(document.createTextNode(lecture.section));
            if (!course.manualTutorialEnrolment && session.tutorials.length > 0) {
                sectionTimes = sectionTimes.concat(convertTimes(session
                    .tutorials[i][0]));
            }
            if (timeSuffix === "Y") {
                $.each(sectionTimes, function(i) {
                    sectionTimes.push("#" + sectionTimes[i] + "S");
                    sectionTimes[i] = "#" + sectionTimes[i] + "F";
                });
            } else {
                $.each(sectionTimes, function(i) {
                    sectionTimes[i] = "#" + sectionTimes[i] + timeSuffix;
                });
            }
            setSectionMouseEvents(section, sectionTimes, course);
            sectionList.appendChild(section);
        }
    });

    $.each(session.tutorials, function (i, tutorial) {
        if (course.manualTutorialEnrolment) {
            var section = document.createElement("li");
            var sectionTimes = convertTimes(tutorial[1]);
            section.appendChild(document.createTextNode(tutorial[0]));
            if (timeSuffix === "Y") {
                $.each(sectionTimes, function(i) {
                    sectionTimes.push("#" + sectionTimes[i] + "S");
                    sectionTimes[i] = "#" + sectionTimes[i] + "F";
                });

            } else {
                $.each(sectionTimes, function(i) {
                    sectionTimes[i] = "#" + sectionTimes[i] + timeSuffix;
                });
            }
            setSectionMouseEvents(section, sectionTimes, course);
            $(section).data("cap", parseInt(tutorial[3]));
            $(section).data("enrol", parseInt(tutorial[4]));
            $(section).data("wait", parseInt(tutorial[5]));
            sectionList.appendChild(section);
        }
    });
    return sectionList;
}


function processSession(course) {
    var sectionList;
    var sections = document.createElement("div");
    sections.setAttribute("class", "sections");
    if (typeof course.Y !== "undefined") {
        sectionList = processSessionSections(course.Y, course, "Y");
        $(sectionList).attr("class", "sectionList-Y");
        setSectionIds(course, sectionList, "Y");
        sections.appendChild(sectionList);
    } else {
        if (typeof course.F !== "undefined") {
            sectionList = processSessionSections(course.F, course, "F");
            $(sectionList).attr("class", "sectionList-F");
            setSectionIds(course, sectionList, "F");
            sections.appendChild(sectionList);
        }
        if (typeof course.S !== "undefined") {
            sectionList = processSessionSections(course.S, course, "S");
            $(sectionList).attr("class", "sectionList-S");
            setSectionIds(course, sectionList, "S");
            sections.appendChild(sectionList);
        }
    }
    return sections;
}


function setSectionIds(course, sectionList, sessionSuffix) {
    $(sectionList).children("li").each(function(index, lecture) {
        $(lecture).attr("id", course.name + "-" + $(this).html() + "-" +
            sessionSuffix);
    });
}
