"use strict";

function setSectionMouseEvents(section, sectionTimes, course) {
    setSectionOnClick(section, sectionTimes, course);
    setSectionMouseOver(section, sectionTimes, course);
    setSectionMouseOut(section, sectionTimes);
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
        $(time).removeClass("mouseOverConflict mouseOverGood " +
                            "mouseOverRemove");
    });
}

// IAN-TODO We still need to figure out a better way to do this,
// to not loop through every td.
function removeMouseOverClasses() {
    $("td").removeClass("mouseOverConflict mouseOverGood " +
                        "mouseOverRemove");
}

/** Mouse Over Direct Functions **/

function setSectionMouseOver(section, sectionTimes, course) {
    $(section).mouseover(function () {
        performMouseOver(sectionTimes, course);
        displayCourseInformation(course);
        displaySectionInformation(course, $(this))
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
        $(time).addClass("mouseOverRemove");
    } else {
        $(time).addClass("mouseOverConflict");
    }
}

function lightUpTakeable(course, time) {
    $(time).addClass("mouseOverGood");
    $(time).html(course.name);
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
    if (cap !== null && enrol !== null) {
        var enrolString = (cap - enrol) + " out of " + cap + " spots remaining";
        $("#section-stats-enrol").html(enrolString);
    }
}

/** Mouse Click Direct Functions **/

function setSectionOnClick(section, sectionTimes, course) {
    $(section).click(function () {
        var isLecture = section.innerHTML.charAt(0) === "L";
        var taken = false;

        if ((course.isLectureSelected && isLecture) || (course.isTutorialSelected && !isLecture)) {
            selectAlreadySelectedSection(course, section, sectionTimes);
        } else {
            selectSection(course, section, sectionTimes);
        }
        
        satisfyCourse(course);

        if (!inArray($(section).attr("id"), selectedLectures)) {
            selectedLectures.push($(section).attr("id"));
        }

        $("td[clicked*=false]").attr("satisfied", true)
                               .attr("type", "")
                               .html("");

        setHeader(course);
        setCookie("selected-lectures", JSON.stringify(selectedLectures));
        removeMouseOverClasses();

        alertUserOfConflict();

        if (course.satisfied) {
            $("#" + course.name + "-li" + " li").attr("satisfied", true);
        }
    });
}

/** Utilities **/

// IAN-RESPONSE It seemed kind of silly to make this function, given that the index is sometimes used.
// IAN-RESPONSE-RESPONSE The only time we use index is when removing an item.
// We should create a helper function for that, too.
function inArray(item, array) {
    return $.inArray(item, array) > -1;
}

// IAN-TODO I guess you want to switch to attributes
// There should really be just one status attribute.
// This seems just like how we handle the nodes in the graph.
function setHeader(course) {
    var taken;

    if ($("#" + course.name + "-li li[clicked*='true']").length) {
        taken = true;
    }

    if (taken && course.satisfied) {
        $(course.header).removeClass("clickedSectionUnsatisfied")
                        .addClass("clicked-header");
        course.taken = true;
    } else if (!course.satisfied) {
        $(course.header).addClass("clickedSectionUnsatisfied");
    } else {
        $(course.header).removeClass("clickedSectionUnsatisfied clicked-header");
        course.taken = false;
    }
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

// Need to adapt code for P.
function getType(section) {
    if (($(section).html().charAt(0) == 'L')) {
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

function removeClickedConflict(course, time, section) {
    var conflictArray = $(time).data("conflictArray");
    var typeArray = $(time).data("typeArray");

    if ($(time).html() === course.name) {
        $(time).html(conflictArray[0])
               .attr("type", typeArray[0]);
        conflictArray.splice(0, 1);
        typeArray.splice(0, 1);
    } else {
        var index = conflictArray.indexOf(course.name);
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


/** Unified course functions (!) **/

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
    } else {
        if (course.selectedTutorial.innerHTML !== section.innerHTML
            || course.selectedTutorialSession !== selectedSession) {
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

function turnSectionOff(course, section, sectionTimes) {
    var type = getType(section);
    removeSectionTimes(course, section);
    if (type === "L") {
        course.isLectureSelected = false;
        $(course.selectedLecture).attr("clicked", "false");
        var index = $.inArray($(course.selectedLecture).attr("id"), selectedLectures);
    } else {  
        course.isTutorialSelected = false;
        $(course.selectedTutorial).attr("clicked", "false");
        var index = $.inArray($(course.selectedTutorial).attr("id"), selectedLectures);
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
                   .attr("clicked", "false");
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
    course.satisfied = (course.selectedTutorialSession === course.selectedLectureSession) || !course.manualTutorialEnrolment;
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
}
