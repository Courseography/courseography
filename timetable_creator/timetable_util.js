/*jslint todo: true */
/*global $, alert*/
/*jslint browser:true */
/*jslint plusplus: true */
"use strict";
var day = /M|T|W|R|F/;
//var time = /1|2|3|4|5|6|7|8|9/;
var hyphen = /-/;
var result;
var i;
var contentString = "";
var backgroundTdColor = "#003366";
var courseSelect;
var xmlhttp;
var csvSplitNewline;
var splitLine;
var isACourse;
var notYetLogged;
var header;
var sections;
var sectionList;
var section;
var sectionTimes;
var times;
var timeSlots;
var entry;
// In the future, it would be nice to pull the course values either from all json files, or the course nodes.


/**
 * Processes this sections times. 
 * TODO: Decide whether to shave this function from the project. There are bigger plans for this function,
 * other than to just call setSectionMouseEvents.
 * @param sections
 * @param sectionTimes
 * @param header
 * @param courseNode
 */
function processSectionTimes(section, sectionTimes, header, courseNode) {
    setSectionMouseEvents(section, sectionTimes, header, courseNode);
}

/**
 * Returns an array of strings of separate lecture times for the specified section. 
 * Sections of type "lecture" are generally separated with commas, whereas tutorials are generally not.
 * TODO: Experiment with split method to shave down code.
 * @param   section
 * @param   type
 * @returns An array of this sections listed time sections.
 */
function getTimesArray(section, type) {
    var times = [];
    if (type === "lecture") {
        times = section.time.split(",");
    } else if (type === "tutorial" && typeof section !== "undefined") {
        times.push(section);
    }
    return times;
}


/**
 * Gets an Array of section's time slots. For example, ["M10","M11","M12","F10"].
 *
 * @param section
 * @param type
 */
function getSectionTimeSlot(section, type) {
    times = getTimesArray(section, type);
    timeSlots = constructTimesArray(times);
    return timeSlots;
}

/**
 * Processes courseNodes session's lectures.
 *
 * @param session     This courseNode's session. Either courseNode.Y, courseNode.F, courseNode.S.
 * @param courseNode  The courseNode that is currently being processed.
 * @param header      The corresponding header to the courseNode object.
 * @param sectionList Recently taken out.
 */
function processSessionLectures(session, courseNode, header, sectionList) {
    $.each(session.lectures, function (i, lecture) {
        if (lecture.section.charAt(1) !== "2") {
            section = document.createElement("li");
            sectionTimes = getSectionTimeSlot(lecture, "lecture");
            section.setAttribute("class", "section");
            section.appendChild(document.createTextNode(lecture.section));
            if (courseNode.manualTutorialEnrolment === false) {
                sectionTimes = sectionTimes.concat(getSectionTimeSlot(session.tutorials[i], "tutorial"));
            }
            processSectionTimes(section, sectionTimes, header, courseNode);
            sectionList.appendChild(section);
        }
    });
    return sectionList;
}

/**
 * Processes courseNodes session's tutorials.
 *
 * @param session     This courseNode's session. Either courseNode.Y, courseNode.F, courseNode.S.
 * @param courseNode  The courseNode that is currently being processed.
 * @param header      The corresponding header to the courseNode object.
 * @param sectionList Recently taken out.
 */
function processSessionTutorials(session, courseNode, header, sectionList) {
    $.each(session.tutorials, function (i, tutorial) {
        if (courseNode.manualTutorialEnrolment) {
            section = document.createElement("li");
            sectionTimes = getSectionTimeSlot(tutorial[1], "tutorial");
            section.setAttribute("class", "section");
            section.appendChild(document.createTextNode(tutorial[0]));
            processSectionTimes(section, sectionTimes, header, courseNode);
            sectionList.appendChild(section);
        }
    });
    return sectionList;
}


/**
 * Processes courseNodes session. A session is the semester construct within the timetable.
 *
 * @param session     This courseNode's session. Either courseNode.Y, courseNode.F, courseNode.S.
 * @param courseNode  The courseNode that is currently being processed.
 * @param header      The corresponding header to the courseNode object.
 * @param sectionList Recently taken out.
 */
function processSession(session, courseNode, header) {
    var sectionList = document.createElement("ul");
    sectionList = processSessionLectures(session, courseNode, header, sectionList);
    sectionList = processSessionTutorials(session, courseNode, header, sectionList);

    return sectionList;
}

/**
 * Sets up the list item entry, adding the element to the course-select list.
 *
 * @param courseNode The course that is being added to the list.
 */
function setupEntry(courseNode) {

    // TODO: Set up winter/spring ability
    if (typeof (courseNode.F) !== "undefined" || typeof (courseNode.Y) !== "undefined") {
        entry = document.createElement("li");
        header = document.createElement("h3");
        sections = document.createElement("div");

        $(entry).css("border", "2px solid black");
        header.appendChild(document.createTextNode(courseNode.name));
        entry.setAttribute("class", "course");
        sections.id = courseNode.name + "Sections";

        if (typeof (courseNode.F) !== "undefined") {
            sectionList = processSession(courseNode.F, courseNode, header);
                        
        } else if (typeof (courseNode.Y) !== "undefined") {
            sectionList = processSession(courseNode.Y, courseNode, header);
        }

        sections.appendChild(sectionList);
        entry.appendChild(header);
        entry.appendChild(sections);
        courseSelect.appendChild(entry);
    }
}

/**
 * Requests the JSON object stored within the res folder of the project.
 *
 * @param courseCode
 */
function getCourse(courseCode) {
    $.ajax({
        url: "../res/courses/timetable/" + courseCode + "TimeTable.txt",
        dataType: "json",
        async: false,
        success: function (data) {
            result = data;
        }
    });
    return result;
}

/**
 * Adds the course to the course-select list navigation panel.
 *
 * @param course The course code of the course being added to the list. For example "CSC108H1"
 * TODO: Make sure limiting the course node name doesn't affect anything.
 */
function addCourseToList(course) {
    var courseNode = getCourse(course);
    courseNode.selected = "false";

    // Convert CSC***H1 -> CSC***
    courseNode.name = courseNode.name.substring(0, 6);
    setupEntry(courseNode);
}


/**
 * Sets up the course-select list (unordered list HTML element).
 * 
 */
function setupList() {
    var course;
    // Iterates through the courses grabbed with the XMLHTTP request.
    // TODO: Rely on better way to grab all course node name in the future.
    for (i = 0; i < csvSplitNewline.length; i++) {
        splitLine = csvSplitNewline[i].split(",");
        course = splitLine[0];
        isACourse = course.indexOf("CSC") > -1;

        // Filters out graduate/undergraduate hybrid courses and makes them purely undergraduate courses.
        if (course.indexOf("/") > -1) {
            course = course.substring(0, course.indexOf("/"));
        }

        // Many courses have duplicate listings due to the timetable holding both F and S sections.
        notYetLogged = contentString.indexOf(course) <= -1;

        if (isACourse && notYetLogged) {
            addCourseToList(course);
            contentString = contentString + course;
        }
    }
}


/**
 * Links each JSON course object to its list item counterpart.
 *
 */
function linkCourseToLI() {
    $(".course h3").each(function () {
        $(this).data(this.innerHTML, getCourse(this.innerHTML));
    });
}









/**
 * Set's all section's mouse events: onclick, mouseover and mouseout. header is generally affected cosmetically.
 *
 * @param section
 * @param sectionTimes
 * @param header
 * @param courseNode
 */
function setSectionMouseEvents(section, sectionTimes, header, courseNode) {
    setSectionOnClick(section, sectionTimes, header, courseNode);
    setSectionMouseOver(section, sectionTimes, courseNode);
    setSectionMouseOut(section, sectionTimes);
}


/**
 * Sets section's onclick function. header is also modified in section's onclick method.
 *
 * TODO: use css class attrribute instead of dynamically changing style.
 * TODO: break up function to increase readability.
 * TODO: Fix cross course conflicts.
 * @param section      The lecture section that is course's mousover function is being applied to.
 * @param sectionTimes
 * @param header
 * @param courseNode
 */
function setSectionOnClick(section, sectionTimes, header, courseNode) {

    $(section).click(function () {

        var isLecture = section.innerHTML.charAt(0) === "L";

        // Duplicate course call
        if (courseNode.selected === "true" && isLecture && courseNode.isLectureSelected === "true") {
            $(courseNode.selectedLecture).css("background-color", backgroundTdColor);
            $(courseNode.selectedLecture).css("color", "white");
            $(courseNode.selectedLectureHeader).css("background-color", backgroundTdColor);
            $(courseNode.selectedLectureHeader).css("color", "white");
            $.each(courseNode.selectedTimes, function (i, time) {
                document.getElementById(time).innerHTML = "";
                document.getElementById(time).setAttribute("clicked", "false");
                $("#" + time).css("background-color", "");
            });
            if (courseNode.selectedLecture.innerHTML !== section.innerHTML) {

                courseNode.selectedLecture = section;
                courseNode.selectedLectureHeader = header;
                courseNode.selectedTimes = sectionTimes;

                if (section.innerHTML.charAt(0) === "L") {
                    $(section).css("background-color", "blue");
                    $(header).css("background-color", "blue");
                } else {
                    $(section).css("background-color", "orange");
                    $(header).css("background-color", "orange");
                }

                $.each(sectionTimes, function (i, time) {
                        document.getElementById(time).innerHTML = courseNode.name;
                        document.getElementById(time).setAttribute("clicked","true");
                        $("#" + time).css("background-color", "blue");
                        $(section).attr("clicked", "true");    
                });
            } else {
                courseNode.selected = "false";
                courseNode.selectedLecture = null;
            }

        } else if (!isLecture) {
            $.each(sectionTimes, function (i, time) {
                if (document.getElementById(time).getAttribute("clicked") === "true" && document.getElementById(time).innerHTML === courseNode.name) {
                    document.getElementById(time).innerHTML = "";
                    document.getElementById(time).setAttribute("clicked", "false");
                $("#" + time).css("background-color", "");
                    resetSectionAndHeaderStyle(time, section, header);
                } else if (document.getElementById(time).getAttribute("clicked") !== "true") {
                    document.getElementById(time).innerHTML = courseNode.name;
                    document.getElementById(time).setAttribute("clicked","true");
                    setSectionAndHeaderStyle(time, section, header);
                }
            });



        } else if (courseNode.selected === "false") {
            courseNode.selected = "true";
            courseNode.selectedLecture = section;

            if (courseNode.selectedLecture.innerHTML.charAt(0) === "L") {
                courseNode.isLectureSelected = "true";
            } else {
                courseNode.isTutorialSelected = "true";
            }

            courseNode.selectedLectureHeader = header;
            courseNode.selectedTimes = sectionTimes;

            $.each(sectionTimes, function (i, time) {
                if (document.getElementById(time).getAttribute("clicked") === "true" && document.getElementById(time).innerHTML === courseNode.name) {
                    document.getElementById(time).innerHTML = "";
                    document.getElementById(time).setAttribute("clicked","false");
                    resetSectionAndHeaderStyle(time, section, header);
                    setTimeStyleOFF(time);
                } else if (document.getElementById(time).getAttribute("clicked") !== "true") {
                    document.getElementById(time).innerHTML = courseNode.name;
                    document.getElementById(time).setAttribute("clicked","true");
                    setSectionAndHeaderStyle(time, section, header);
                    setTimeStyleON(time, section);
                }
            });
        }
    });
}


/**---------------Functions to help style changes------------------------*/
function setSectionAndHeaderStyle(time, section, header) {
    if (section.innerHTML.charAt(0) === "L") {
        $(section).attr("class", "sectionClickedLecture");
        $(header).attr("class", "headerClickedLecture");
    } else {
        $(section).attr("class", "sectionClickedTutorial");
        $(header).attr("class", "headerClickedTutorial");
    }
    $(section).attr("clicked", "true");
}

function setTimeStyleON(time, section) {
    if (section.innerHTML.charAt(0) === "L") {
        $("#" + time).css("background-color", "blue");
    } else {
        $("#" + time).css("background-color", "orange");
    }

}

function setTimeStyleOFF(time) {
    $("#" + time).css("background-color", backgroundTdColor);
}
/**---------------Functions to help style changes------------------------*/
function resetSectionAndHeaderStyle(time, section, header) {
    $(section).css("background-color", backgroundTdColor);
    $(section).css("color", "white");
    $(header).css("background-color", backgroundTdColor);
    $(header).css("color", "white");
}

/**
 * Sets section's mouseover function.
 *
 * @param section      The lecture section that is course's mousover function is being applied to.
 * @param sectionTimes
 * @param courseNode   The courseNode that owns the lecture section.
 */
function setSectionMouseOver(section, sectionTimes, courseNode) {
    $(section).mouseover(function () {
        $.each(sectionTimes, function (i, time) {
            if (document.getElementById(time).getAttribute("clicked") === "true") {
                $("#" + time).css("background-color", "#FF6666");
            } else {
                document.getElementById(time).innerHTML = courseNode.name;
            }
            if (courseNode.name === "CSC309") {
                // TODO: Speak with David about 309.
                alert("Bug alert! 309 has a mislabeled section in the CSC timesheet!");
            }
        });
    });
}


/**
 * Sets section's mouseout function. 
 *
 * @param section      The lecture section that is course's mousover function is being applied to.
 * @param sectionTimes
 */
function setSectionMouseOut(section, sectionTimes) {
    $(section).mouseout(function () {
        $.each(sectionTimes, function (i, time) {
            if (document.getElementById(time).getAttribute("clicked") !== "true") {
                document.getElementById(time).innerHTML = "";
                //$("#" + time).css("background-color", backgroundTdColor);    
            } else {
                //Cancels out the tutorial on click TODO: find better solution.
                $("#" + time).css("background-color", "blue");
            }
        });
    });
}

/**
 * Parses the individual course time string.
 * Example strings: "MWF10-12" | "M10" | "MT10-1".
 * TODO: Redo entire function. It works for now, but could be made simpler. As mentioned in the below TODO, it needs to be shaved.
 * @param An array of strings representing this course sections allocated times. ["F10","MT10-1"]
 * @return An array of strings representing each individual time. For instance, "M10-12" would be split into
 * ["M10","M11","M12"].
 * TODO: Break up function to increase readability.
 * @param times
 */
function constructTimesArray(times) {
    var firstTimeSlot;
    var secondTimeSlot;
    var thirdTimeSlot;
    var timeSlots = [];
    var numDays;
    var difference;
    var timeLength;
    $.each(times, function (i, lectureTime) {
        timeLength = lectureTime.length;
        numDays = 0;
        for (var k = 0; k < timeLength; k++) {

            if (lectureTime.charAt(k).match(day) !== null) {
                numDays++;
            }

            switch (k) {
                case 0: // M,T,W,R,F - Always a day
                    firstTimeSlot = lectureTime.charAt(0);
                    break;
                case 1: //M1, MW
                    switch(numDays) {
                        case 1: // M1
                            firstTimeSlot = firstTimeSlot + lectureTime.charAt(1);
                            break;
                        case 2: //MW
                            secondTimeSlot = lectureTime.charAt(1);
                            break;
                    }
                    break;
                case 2: //MWF. MW1. M1-
                    switch(numDays) {
                        case 1: // M10, M1- !!!!!
                            if (!lectureTime.charAt(2).match(hyphen)) {
                                firstTimeSlot = firstTimeSlot + lectureTime.charAt(2);        
                            }
                            break;
                        case 2: //MW1
                            firstTimeSlot = firstTimeSlot + lectureTime.charAt(2);
                            secondTimeSlot = secondTimeSlot + lectureTime.charAt(2);
                            break;
                        case 3: // MWF
                            thirdTimeSlot = lectureTime.charAt(2);
                            break;
                    }
                    break;
                case 3: //MWF1
                    switch(numDays) {
                        case 1: //M10-, M1-2 W6-9

                            if (timeLength > 5) {
                                difference = lectureTime.substring(4,6) - lectureTime.substring(1,3);
                                newTime = parseInt(firstTimeSlot.substring(1,3));
                            } else if (timeLength === 5) {
                                if (lectureTime.charAt(2).match(hyphen)) { //M9-11
                                    difference = lectureTime.substring(3,5) - lectureTime.charAt(1);
                                    newTime = parseInt(firstTimeSlot.charAt(1));
                                } else { //M11-1
                                    difference = lectureTime.charAt(4) - lectureTime.substring(1,3) + 12;
                                    newTime = parseInt(firstTimeSlot.substring(1,3));
                                }
                            } else { // M1-2
                                difference = lectureTime.charAt(3) - (lectureTime.charAt(1));
                                newTime = parseInt(firstTimeSlot.charAt(1));
                            }

                            for (var l = 0; l < difference; l++) {
                                if (newTime > 12) {
                                    newTime = newTime - 12;
                                }
                                firstTimeSlot = firstTimeSlot.charAt(0) + newTime;
                                timeSlots.push(firstTimeSlot);
                                newTime = newTime + 1;
                            }

                            return timeSlots;
                        case 2: //MW1-, MW10-12, WF11
                            if (lectureTime.search("-") <= -1) {

                                firstTimeSlot = firstTimeSlot + lectureTime.charAt(3);
                                secondTimeSlot = secondTimeSlot + lectureTime.charAt(3);

                            } else {
                                var newTime;
                                if (timeLength > 5) {
                                    difference = lectureTime.substring(5,7) - lectureTime.substring(2,4);
                                    newTime = parseInt(firstTimeSlot.substring(2,4));
                                } else if (timeLength === 6) {
                                    difference = lectureTime.substring(1,3) - lectureTime.charAt(4);
                                    newTime = parseInt(firstTimeSlot.substring(1,3));
                                } else {
                                    difference = lectureTime.charAt(1) - (lectureTime.substring(1,3) - 12);
                                    newTime = parseInt(firstTimeSlot.charAt(1));
                                }

                                for (var o = 0; o < difference; o++) {
                                    if (newTime > 12) {
                                        newTime = newTime - 12;
                                    }
                                    firstTimeSlot = firstTimeSlot.charAt(0) + newTime;
                                    //timeSlots.push(firstTimeSlot);
                                    newTime = newTime + 1;
                                }
                            }
                            break;
                        case 3: // MWF1
                                firstTimeSlot = firstTimeSlot + lectureTime.charAt(3);
                                secondTimeSlot = secondTimeSlot + lectureTime.charAt(3);
                                thirdTimeSlot = thirdTimeSlot + lectureTime.charAt(3);
                            break;
                        case 4: // MWTF, not yet covered by CS, but german has sections like this.

                            break;
                    }
                    break;
                case 4:
                    switch(numDays) {
                        case 1: // M10-1, case previously covered

                            break;
                        case 2: // MW10-1, MW1-10, cases previously covered

                            break;
                        case 3: // MWF12, MWF1-2 <-- need to cover this case, for future.
                            firstTimeSlot = firstTimeSlot + lectureTime.charAt(4);
                            secondTimeSlot = secondTimeSlot + lectureTime.charAt(4);
                            thirdTimeSlot = thirdTimeSlot + lectureTime.charAt(4);
                            break;
                        case 4: // MTWF1, todo

                            break;
                    }
                    break;
                case 5:
                    switch(numDays) {
                        case 1: // No strings reach this state.

                            break;
                        case 2: // MT10-12 <-- already covered

                            break;
                        case 3: //

                            break;
                        case 3: //

                            break;
                    }
                    break;
                case 6: //MW11-12
                    switch(numDays) {
                        case 1: // No strings reach this state.

                            break;
                        case 2: //

                            break;
                        case 3: // 

                            break;
                        case 3: // 

                            break;
                    }
                    break;
            }
        }

        if (typeof (firstTimeSlot) !== "undefined") {
            timeSlots.push(firstTimeSlot);

        } 
        if (typeof (secondTimeSlot) !== "undefined") {
            timeSlots.push(secondTimeSlot);

        }
        if (typeof (thirdTimeSlot) !== "undefined") {
            timeSlots.push(thirdTimeSlot);

        }
    });
    return timeSlots;
}






/**
 * Called when the document is ready. This links all courses that have been grabbed from the XMLHTTP request
 * with their corresponding JSON object, and adds them to the course-select HTML component, including all
 * onclick, mouseover, mouseout methods. 
 */
$(document).ready(function () {
    courseSelect = document.getElementById("course-select");
    if (window.XMLHttpRequest) {
        xmlhttp = new XMLHttpRequest();
    } else {
        xmlhttp = new window.ActiveXObject("Microsoft.XMLHTTP");
    }

    xmlhttp.open("GET", "../res/timetable2014.csv", false);
    xmlhttp.send();

    var httpResponse = xmlhttp.responseText;
    csvSplitNewline = httpResponse.split("\n");
    setupList();
    linkCourseToLI();
    $("#course-select").accordion({heightStyle: "content", collapsible: true});
});

// function showValue(newValue) {
// 	document.getElementById("range").innerHTML=newValue;
// 	timetable = document.getElementById("timeTable");
// 	$(timetable).css("background", "-webkit-linear-gradient(top, #32a3ff 6%,#1e5799 62%,#1e5799 64%,#000000 100%);");
// }

$(function() {
    $( "#tabs" ).tabs({
      collapsible: true, active: 0
    });
 });