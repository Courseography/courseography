"use strict";

function setSectionMouseEvents(section, sectionTimes, course) {
    setSectionOnClick(section, sectionTimes, course);
    setSectionMouseOver(section, sectionTimes, course);
    setSectionMouseOut(section, sectionTimes);
    setTdHover();
}


function setTdHover() {
    $("td").mouseover(function() {
        var courseHtml = $(this).html();
        var course = getCourseObject(courseHtml);
        if (typeof course !== "undefined") {
            $.each(course.selectedLectureTimes.concat(course.selectedTutorialTimes), function(i, time) {
                $(time).addClass("hover-time");
            });
        }
    });

    $("td").mouseout(function() {
        var courseHtml = $(this).html();
        var course = getCourseObject(courseHtml);
        if (typeof course !== "undefined") {
            $.each(course.selectedLectureTimes.concat(course.selectedTutorialTimes), function(i, time) {
                $(time).removeClass("hover-time");
            });
        }
    });
}

/** Mouse Out Direct Functions **/

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
        performMouseOver(sectionTimes, course, section);
        displayCourseInformation(course);
        displaySectionInformation(course, $(this))
    });
}

function performMouseOver(sectionTimes, course, section) {
    $.each(sectionTimes, function (i, time) {
        if (getIsClicked(time)) {
            lightUpConflict(course, time, section);
        } else {
            lightUpTakeable(course, time);
        }
    });
}

function lightUpConflict(course, time, section) {
    if ($(time).html() === course.name
        && $(time).attr("type") === getType(section)) {
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

function displaySectionInformation(course, section) {
    $("#section-stats-section").html(section.html());
    $("#section-stats-instructor").html(section.data("instructor"));
    var cap = section.data("cap");
    var enrol = section.data("enrol");
    var wait = section.data("wait");
    if (cap !== null && enrol !== null) {
        var enrolString = (cap - enrol) + " out of " + cap + " spots remaining";
        if (wait !== null && wait !== undefined && wait !== 0) {
            enrolString += "; " + wait + " students on the waitlist";
        }
        $("#section-stats-enrol").html(enrolString);
    }
}

/** Mouse Click Direct Functions **/

function setSectionOnClick(section, sectionTimes, course) {
    $(section).click(function () {
        var isLecture = section.innerHTML.charAt(0) === "L";
        var isTutorial = section.innerHTML.charAt(0) === "T";
        var isPractical = section.innerHTML.charAt(0) === "P";
        updateSelectedLectures($(section));
        if ((course.isLectureSelected && isLecture) 
            || (course.isTutorialSelected && isTutorial)
            || (course.isPracticalSelected && isPractical)) {
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

/** Utilities **/

function setHeader(course) {
    $(course.header).attr("taken", $("#" + course.name + "-li li[clicked*='true']").length > 0)
                    .attr("satisfied", course.satisfied);
}

function updateSelectedLectures(section) {
    if (!inArray(section.attr("id"), selectedLectures)) {
        selectedLectures.push(section.attr("id"));
    }
}

// IAN-RESPONSE It seemed kind of silly to make this function, given that the index is sometimes used.
// IAN-RESPONSE-RESPONSE The only time we use index is when removing an item.
// We should create a helper function for that, too.
function inArray(item, array) {
    return $.inArray(item, array) > -1;
}

function getInConflict() {
    return $("td[class*=clickedConflictTime]").length > 0;
}

function alertUserOfConflict() {
    getInConflict() ? $("#dialog").fadeIn(750) : $("#dialog").fadeOut(750);
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

function getType(section) {
    return $(section).html().charAt(0);
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

function removeClickedConflict(course, time, section) {
    var conflictArray = $(time).data("conflictArray");
    var typeArray = $(time).data("typeArray");
    var index = conflictArray.indexOf(course.name);
    if ($(time).html() === course.name) {
        $(time).html(conflictArray[0]);

        if (index === -1 || !(getType(section) === typeArray[0])) {
            $(time).attr("type", typeArray[0]);
        }

        conflictArray.splice(0, 1);
        typeArray.splice(0, 1);
    } else {
        conflictArray.splice(index, 1);
        typeArray.splice(index, 1);
    }

    if (conflictArray.length === 0) {
        $(time).attr("in-conflict", "false");
    }

    var newCourseObject = getCourseObject($(time).html());
    $(time).attr("satisfied", newCourseObject.satisfied);

    $(time).data("conflictArray", conflictArray)
           .data("typeArray", typeArray)
           .attr("title", conflictArray);
}

function selectAlreadySelectedSection(course, section, sectionTimes) {
    turnSectionOff(course, section, sectionTimes);
    var selectedSession = getSession(section);
    var type = getType(section);
    if (type === "L") {
        if (course.selectedLecture.innerHTML !== section.innerHTML
            || course.selectedLectureSession !== selectedSession) {
            selectSection(course, section, sectionTimes);
        } else {
            course.selectedLecture = undefined;
            course.selectedLectureSession = undefined;
            course.selectedLectureTimes = undefined;
        }
    } else if (type === "T") {
        if (course.selectedTutorial.innerHTML !== section.innerHTML
            || course.selectedTutorialSession !== selectedSession) {
            selectSection(course, section, sectionTimes);
        } else {
            course.selectedTutorial = undefined;
            course.selectedTutorialSession = undefined;
            course.selectedTutorialTimes = undefined;
        }
    } else if (type === "P") {
        if (course.selectedPractical.innerHTML !== section.innerHTML
            || course.selectedPracticalSession !== selectedSession) {
            selectSection(course, section, sectionTimes);
        } else {
            course.selectedPractical = undefined;
            course.selectedPracticalSession = undefined;
            course.selectedPracticalTimes = undefined;
        }
    }
}

function selectSection(course, section, sectionTimes) {
    var type = getType(section);
    if (type === "L") {
        course.selectedLecture = section;
        course.isLectureSelected = true;
        course.selectedLectureTimes = sectionTimes;
    } else if (type === "T") {
        course.selectedTutorial = section;
        course.isTutorialSelected = true;
        course.selectedTutorialTimes = sectionTimes;
    } else if (type === "P") {
        course.selectedPractical = section;
        course.isPracticalSelected = true;
        course.selectedPracticalTimes = sectionTimes;
    }
    $(section).attr("clicked", "true");
    setSession(course, section);

    selectUnselectedTimes(course, sectionTimes, section);
}

function turnSectionOff(course, section, sectionTimes) {
    var index;
    var type = getType(section);
    removeSectionTimes(course, section);
    if (type === "L") {
        course.isLectureSelected = false;
        $(course.selectedLecture).attr("clicked", "false");
        index = $.inArray($(course.selectedLecture).attr("id"), selectedLectures);
    } else if (type === "T") {  
        course.isTutorialSelected = false;
        $(course.selectedTutorial).attr("clicked", "false");
        index = $.inArray($(course.selectedTutorial).attr("id"), selectedLectures);
    } else if (type === "P") {  
        course.isPracticalSelected = false;
        $(course.selectedPractical).attr("clicked", "false");
        index = $.inArray($(course.selectedPractical).attr("id"), selectedLectures);
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
    } else if (type === "T") {
        sectionTimes = course.selectedTutorialTimes;
    } else if (type === "P") {
        sectionTimes = course.selectedPracticalTimes;
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
    } else if (type === "T") {
        course.selectedTutorialSession = session;
    } else if (type === "P") {
        course.selectedPracticalSession = session;
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
    course.satisfied = ((course.selectedTutorialSession === course.selectedLectureSession) && !course.practicalEnrolment) 
    || ((course.selectedPracticalSession === course.selectedLectureSession) && !course.tutorialEnrolment) 
    || !course.manualTutorialEnrolment
    || ((course.selectedTutorialSession === course.selectedLectureSession)
        && (course.selectedTutorialSession === course.selectedPracticalSession));
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

    if (typeof course.selectedPracticalTimes !== "undefined") {
        $.each(course.selectedPracticalTimes, function (i, time) {
            $(time).attr("satisfied", course.satisfied);
        });
        $(course.selectedPractical).attr("satisfied", course.satisfied);
    }
    setSectionsSatisfied(course);
}

function setSectionsSatisfied(course) {
    if (course.satisfied) {
        $("#" + course.name + "-li" + " li").attr("satisfied", true);
    }
}
