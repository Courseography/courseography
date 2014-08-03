"use strict";


function setSectionMouseEvents(section, sectionTimes, course) {
    setSectionOnClick(section, sectionTimes, course);
    setSectionMouseOver(section, sectionTimes, course);
    setSectionMouseOut(section, sectionTimes);
    setTdHover();
}


function setTdHover() {
    var tdSelector = $("td");

    tdSelector.mouseover(function () {
        var courseHtml = $(this).html();
        var course = getCourseObject(courseHtml);
        if (typeof course !== "undefined") {
            $.each(course.selectedLectureTimes.concat(course
                .selectedTutorialTimes), function (i, time) {
                $(time).addClass("hover-time");
            });
        }
    });

    tdSelector.mouseout(function () {
        var courseHtml = $(this).html();
        var course = getCourseObject(courseHtml);
        if (typeof course !== "undefined") {
            $.each(course.selectedLectureTimes.concat(course
                .selectedTutorialTimes), function (i, time) {
                $(time).removeClass("hover-time");
            });
        }
    });
}


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
        $(time).attr("hover", "off");
    });
}


/** Mouse Over Direct Functions **/

function setSectionMouseOver(section, sectionTimes, course) {
    $(section).mouseover(function () {
        performMouseOver(sectionTimes, course);
        displayCourseInformation(course);
        displaySectionInformation(course, $(this));
    });
}

function performMouseOver(sectionTimes, course) {
    $.each(sectionTimes, function (i, time) {
        if (getIsClicked(time)) {
            lightUpConflict(course, time);
        } else {
            lightUpTakeable(course, time);
        }
    });

}


function lightUpConflict(course, time) {
    if ($(time).html() === course.name) {
        $(time).attr("hover", "remove");
    } else {
        $(time).attr("hover", "conflict");
    }
}

function lightUpTakeable(course, time) {
    $(time).html(course.name);
    $(time).attr("hover", "good");
}


function displayCourseInformation(course) {
    $("#course-info-code").html(course.name);
    $("#course-info-title").html(course.title);
}


function displaySectionInformation(section) {
    $("#section-stats-section").html(section.html());
    $("#section-stats-instructor").html(section.data("instructor"));
    var cap = section.data("cap");
    var enrol = section.data("enrol");
    var wait = section.data("wait");
    if (cap !== null && enrol !== null) {
        var enrolString = (cap - enrol) + " out of " + cap +
            " spots remaining";
        if (wait !== null && wait !== undefined && wait !== 0) {
            enrolString += "; " + wait + " students on the waitlist";
        }
        $("#section-stats-enrol").html(enrolString);
    }

}


function setSectionOnClick(section, sectionTimes, course) {
    $(section).click(function () {
        var isLecture = section.innerHTML.charAt(0) === "L";
        updateSelectedLectures($(section));
        if ((course.isLectureSelected && isLecture) ||
            (course.isTutorialSelected && !isLecture)) {
            selectAlreadySelectedSection(course, section, sectionTimes);
        } else {
            selectSection(course, section, sectionTimes);
        }
        
        satisfyCourse(course);
        setCookie("selected-lectures", JSON.stringify(selectedLectures));

        alertUserOfConflict();
        setHeader(course);
    });
}


function setHeader(course) {
    $(course.header).attr("taken", $("#" + course.name +
        "-li li[clicked*='true']").length > 0)
                    .attr("satisfied", course.satisfied);
}


function updateSelectedLectures(section) {
    if (!inArray(section.attr("id"), selectedLectures)) {
        selectedLectures.push(section.attr("id"));
    }
}


// IAN-RESPONSE It seemed kind of silly to make this function,
// given that the index is sometimes used.
// IAN-RESPONSE-RESPONSE The only time we use index is when removing an item.
// We should create a helper function for that, too.
function inArray(item, array) {
    return $.inArray(item, array) > -1;
}

function removeFromArray(item, array) {
    var index = array.indexOf(item);
    array.splice(index, 1);
    return index;
}

function getIndexFromArray(item, array) {
    return $.inArray(item, array);
}


function getInConflict() {
    return $("td[class*=clickedConflictTime]").length > 0;
}


function alertUserOfConflict() {
    var dialogSelector = $("#dialog");
    getInConflict() ? dialogSelector.fadeIn(750) :
        dialogSelector.fadeOut(750);
}


function getIsClicked(time) {
    return $(time).attr("clicked") === "true";
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
    return $(section.parentNode).hasClass("sectionList-Y");
}


function getIsFallSection(section) {
    return $(section.parentNode).hasClass("sectionList-F");
}


function getIsSpringSection(section) {
    return $(section.parentNode).hasClass("sectionList-S");
}


// Need to adapt code for P.
function getType(section) {
    if (($(section).html().charAt(0) === 'L')) {
        return "L";
    } else {
        return "T";
    }
}


function setClickedConflict(course, time, section) {
    var type = getType(section);
    var conflictArray = $(time).data("conflictArray");
    var typeArray = $(time).data("typeArray");
    conflictArray.push(course.name);
    typeArray.push(type);
    $(time).data("conflictArray", conflictArray)
           .data("typeArray", typeArray)
           .attr("title", conflictArray)
           .attr("in-conflict", "true");
}


function removeClickedConflict(course, time) {
    var conflictArray = $(time).data("conflictArray");
    var typeArray = $(time).data("typeArray");

    if ($(time).html() === course.name) {
        $(time).html(conflictArray[0])
               .attr("type", typeArray[0]);
        conflictArray.splice(0, 1);
        typeArray.splice(0, 1);
    } else {
        var index = removeFromArray(course.name, conflictArray);
        typeArray.splice(index, 1);
    }

    if (conflictArray.length === 0) {
        $(time).attr("in-conflict", "false");
    }

    var newCourseObject = getCourseObject($(time).html());
    $(time).attr("satisfied", newCourseObject.satisfied)
           .data("conflictArray", conflictArray)
           .data("typeArray", typeArray)
           .attr("title", conflictArray);
}


function selectAlreadySelectedSection(course, section, sectionTimes) {
    turnSectionOff(course, section, sectionTimes);
    var selectedSession = getSession(section);
    var type = getType(section);
    if (type === "L") {
        if (course.selectedLecture.innerHTML !== section.innerHTML ||
            course.selectedLectureSession !== selectedSession) {
            selectSection(course, section, sectionTimes);
        } else {
            course.selectedLecture = undefined;
            course.selectedLectureSession = undefined;
            course.selectedLectureTimes = undefined;
        }
    } else {
        if (course.selectedTutorial.innerHTML !== section.innerHTML ||
            course.selectedTutorialSession !== selectedSession) {
            selectSection(course, section, sectionTimes);
        } else {
            course.selectedTutorial = undefined;
            course.selectedTutorialSession = undefined;
            course.selectedTutorialTimes = undefined;
        }
    }
}


function selectSection(course, section, sectionTimes) {
    var type = getType(section);
    if (type === "L") {
        course.selectedLecture = section;
        course.isLectureSelected = true;
        course.selectedLectureTimes = sectionTimes;
    } else {
        course.selectedTutorial = section;
        course.isTutorialSelected = true;
        course.selectedTutorialTimes = sectionTimes;
    }
    $(section).attr("clicked", "true");
    setSession(course, section);

    selectUnselectedTimes(course, sectionTimes, section);
}


function turnSectionOff(course, section) {
    var type = getType(section);
    var index = -1;
    removeSectionTimes(course, section);
    if (type === "L") {
        course.isLectureSelected = false;
        $(course.selectedLecture).attr("clicked", "false");
        index = getIndexFromArray($(course.selectedLecture).attr("id"),
            selectedLectures);
    } else {  
        course.isTutorialSelected = false;
        $(course.selectedTutorial).attr("clicked", "false");
        index = getIndexFromArray($(course.selectedTutorial).attr("id"),
            selectedLectures);
    }

    if (index > -1) {
        selectedLectures.splice(index, 1);
    }
}


function removeSectionTimes(course, section) {
    var sectionTimes;
    var type = getType(section);
    if (type === "L") {
        sectionTimes = course.selectedLectureTimes;
    } else {
        sectionTimes = course.selectedTutorialTimes;
    }
    $.each(sectionTimes, function (i, time) {
        if ($(time).attr("in-conflict") === "true") {
            removeClickedConflict(course, time, section);
        } else {
            $(time).html("")
                   .attr("clicked", "false")
                   .attr("satisfied", true)
                   .attr("type", "");               
        }
    });
}


function setSession(course, section) {
    var type = getType(section);
    var session = getSession(section);
    if (type === "L") {
        course.selectedLectureSession = session;
    } else {
        course.selectedTutorialSession = session;
    }
}


function selectUnselectedTimes(course, sectionTimes, section) {
    $.each(sectionTimes, function (i, time) {
        if (!getIsClicked(time)) {
            setClickedTime(course, time, section);
        } else {
            setClickedConflict(course, time, section);
        }
    });
}


function setClickedTime(course, time, section) {
    var type = getType(section);
    $(time).html(course.name)
           .attr("clicked", "true")
           .attr("type", type);
}

function satisfyCourse(course) {
    course.satisfied = (course.selectedTutorialSession ===
        course.selectedLectureSession) ||
        !course.manualTutorialEnrolment;
    setSatisfaction(course);
}


function setSatisfaction(course) {
    if (typeof course.selectedLectureTimes !== "undefined") {
        $.each(course.selectedLectureTimes, function (i, time) {
            $(time).attr("satisfied", course.satisfied);
        });
        $(course.selectedLecture).attr("satisfied", course.satisfied);
    }

    if (typeof course.selectedTutorialTimes !== "undefined") {
        $.each(course.selectedTutorialTimes, function (i, time) {
            $(time).attr("satisfied", course.satisfied);
        });
        $(course.selectedTutorial).attr("satisfied", course.satisfied);
    }
    setSectionsSatisfied(course);
}


function setSectionsSatisfied(course) {
    if (course.satisfied) {
        $("#" + course.name + "-li" + " li").attr("satisfied", true);
    }
}
