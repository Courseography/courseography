/*jslint todo: true */
/*global $, console*/
/*jslint browser:true */
/*jslint plusplus: true */
"use strict";
var day = /M|T|W|R|F/;
//var time = /1|2|3|4|5|6|7|8|9/;
var hyphen = /-/;
var result;
var i;
var contentString = "";
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

/**
 * Processes this sections times. 
 * TODO: Decide whether to shave this function from the project. There are bigger plans for this function,
 * other than to just call setSectionMouseEvents.
 * @param sections
 * @param sectionTimes
 * @param courseObject
 */
 function processSectionTimes(section, sectionTimes, courseObject) {
    setSectionMouseEvents(section, sectionTimes, courseObject);
}

/**
 * Processes courseObjects session's lectures.
 *
 * @param session     This courseObject's session. Either courseObject.Y, courseObject.F, courseObject.S.
 * @param courseObject  The courseObject that is currently being processed.
 * @param header      The corresponding header to the courseObject object.
 * @param sectionList Recently taken out.
 */
 function processSessionLectures(session, courseObject) {
    var sectionList = document.createElement("ul");
    $.each(session.lectures, function (i, lecture) {
        if (lecture.section.charAt(1) !== "2" && lecture.time !== "Online Web Version") {
            section = document.createElement("li");
            sectionTimes = lecture.times;
            section.appendChild(document.createTextNode(lecture.section));
            if (courseObject.manualTutorialEnrolment === false && session['tutorials'].length > 0) {
                sectionTimes = sectionTimes.concat(session['tutorials'][i]);
            }
            processSectionTimes(section, sectionTimes, courseObject);
            sectionList.appendChild(section);
        }
    });
    return sectionList;
}

function returnUniqueElements(array) {
    var sortedArray = array.sort();
    var result = []
    for (var i = 0; i < array.length - 1; i++) {
        if (sortedArray[i + 1] !== sortedArray[i]) {
            result.push(sortedArray[i]);
        }
    }
    result.push(sortedArray[array.length-1]);
    return result;
}

/**
 * Processes courseObjects session's tutorials.
 *
 * @param session     This courseObject's session. Either courseObject.Y, courseObject.F, courseObject.S.
 * @param courseObject  The courseObject that is currently being processed.
 * @param header      The corresponding header to the courseObject object.
 * @param sectionList Recently taken out.
 */
 function processSessionTutorials(session, courseObject, sectionList) {
    $.each(session.tutorials, function (i, tutorial) {
        if (courseObject.manualTutorialEnrolment) {
            section = document.createElement("li");
            console.log("tutorial" + tutorial[1])
            sectionTimes = tutorial[1];
            section.appendChild(document.createTextNode(tutorial[0]));
            processSectionTimes(section, sectionTimes, courseObject);
            sectionList.appendChild(section);
        }
    });
    return sectionList;
}


/**
 * Processes courseObjects session. A session is the semester construct within the timetable.
 *
 * @param session     This courseObject's session. Either courseObject.Y, courseObject.F, courseObject.S.
 * @param courseObject  The courseObject that is currently being processed.
 * @param header      The corresponding header to the courseObject object.
 * @param sectionList Recently taken out.
 */
 function processSession(courseObject) {
    sections = document.createElement("div");
    sections.setAttribute("class", "sections");
    if (typeof courseObject.Y !== "undefined") {  
        var sectionList = document.createElement("ul");  
        sectionList = processSessionLectures(courseObject.Y, courseObject);
        sectionList = processSessionTutorials(courseObject.Y, courseObject, sectionList);
        $(sectionList).attr("class", "sectionList-year");
        sections.appendChild(sectionList);
    } else {
        var sectionList = document.createElement("ul");
        if (typeof courseObject.F !== "undefined") {
            sectionList = processSessionLectures(courseObject.F, courseObject);
            sectionList = processSessionTutorials(courseObject.F, courseObject, sectionList);
            $(sectionList).attr("class", "sectionList-fall");
            sections.appendChild(sectionList);
        }
        if (typeof courseObject.S !== "undefined") { 
            var sectionList = document.createElement("ul");
            sectionList = processSessionLectures(courseObject.S, courseObject);
            sectionList = processSessionTutorials(courseObject.S, courseObject, sectionList);
            $(sectionList).attr("class", "sectionList-spring");
            sections.appendChild(sectionList);
        }
    }
    return sections;
}

/**
 * Sets up the list item entry, adding the element to the course-select list.
 *
 * @param courseObject The course that is being added to the list.
 */
 function setupEntry(courseObject) {
    entry = document.createElement("li");
    header = document.createElement("h3");
    header.appendChild(document.createTextNode(courseObject.code));
    courseObject.header = header;
    sections = processSession(courseObject);
    entry.appendChild(header);
    $(sections).css("height","100%");
    $(sections).css("width","100%");
    entry.appendChild(sections);
    courseSelect.appendChild(entry);
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
    var courseObject = getCourse(course);
    courseObject.selectedSession = null;
    courseObject.selected = "false";

    // Convert CSC***H1 -> CSC***
    courseObject.code = courseObject.code.substring(0, 6);
    setupEntry(courseObject);
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
 * Set's all section's mouse events: onclick, mouseover and mouseout. header is generally affected cosmetically.
 *
 * @param section
 * @param sectionTimes
 * @param header
 * @param courseObject
 */
 function setSectionMouseEvents(section, sectionTimes, courseObject) {
    setSectionOnClick(section, sectionTimes, courseObject);
    setSectionMouseOver(section, sectionTimes, courseObject);
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
 * @param courseObject
 */
 function setSectionOnClick(section, sectionTimes, courseObject) {
    $(section).click(function () {
        var isLecture = section.innerHTML.charAt(0) === "L";
        var selectedSession;
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
    // TODO: Fix cross session courses taken twice!
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

/**
 * Sets section's mouseover function.
 *
 * @param section      The lecture section that is course's mousover function is being applied to.
 * @param sectionTimes
 * @param courseObject   The courseObject that owns the lecture section.
 */
 function setSectionMouseOver(section, sectionTimes, courseObject) {
    var timeElement;
    $(section).mouseover(function () {
        $.each(sectionTimes, function (i, time) {
            if ($(section.parentNode).attr("class") === "sectionList-fall") {
                timeElement = time + "-fall";
            } else {
                timeElement = time + "-spring";
            }
            console.log(timeElement)
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

/**
 * Sets section's mouseout function. 
 *
 * @param section      The lecture section that is course's mousover function is being applied to.
 * @param sectionTimes
 */
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

function setAccordion() {
$("#course-select").accordion({heightStyle: "content", collapsible: true, active: false/*, event: "click hoverintent"*/});
}

/**
 * Called when the document is ready. This links all courses that have been grabbed from the XMLHTTP request
 * with their corresponding JSON object, and adds them to the course-select HTML component, including all
 * onclick, mouseover, mouseout methods. 
 */
 $(document).ready(function () {
    courseSelect = document.getElementById("course-select");
    
    csvSplitNewline = getCourseArray();
    setupList();
    setAccordion();
    trapScroll();
});

function getCourseArray() {
    var httpResponse;
    if (window.XMLHttpRequest) {
        xmlhttp = new XMLHttpRequest();
    } else {
        xmlhttp = new window.ActiveXObject("Microsoft.XMLHTTP");
    }

    xmlhttp.open("GET", "../res/timetable2014.csv", false);
    xmlhttp.send();

    httpResponse = xmlhttp.responseText;

    return httpResponse.split("\n");
}

$.event.special.hoverintent = {
    setup: function() {
      $( this ).bind( "mouseover", jQuery.event.special.hoverintent.handler );
    },
    teardown: function() {
        $( this ).unbind( "mouseover", jQuery.event.special.hoverintent.handler );
    },
    handler: function( event ) {
        var currentX, currentY, timeout,
        args = arguments,
        target = $( event.target ),
        previousX = event.pageX,
        previousY = event.pageY;

        function track( event ) {
          currentX = event.pageX;
          currentY = event.pageY;
        };

    function clear() {
        target
        .unbind( "mousemove", track )
        .unbind( "mouseout", clear );
        clearTimeout( timeout );
    }

    function handler() {
        var prop,
        orig = event;

        if ( ( Math.abs( previousX - currentX ) +
            Math.abs( previousY - currentY ) ) < 7 ) {
          clear();

        event = $.Event( "hoverintent" );
        for ( prop in orig ) {
            if ( !( prop in event ) ) {
              event[ prop ] = orig[ prop ];
            }
        }
            // Prevent accessing the original event since the new event
            // is fired asynchronously and the old event is no longer
            // usable (#6028)
            delete event.originalEvent;

            target.trigger( event );
        } else {
            previousX = currentX;
            previousY = currentY;
            timeout = setTimeout( handler, 100 );
        }
    }
 
    timeout = setTimeout( handler, 100 );
    target.bind({
        mousemove: track,
        mouseout: clear
});
}
};

var trapScroll;


/*
 * Adapted from http://codepen.io/LelandKwong/pen/edAmn. Will look into http://jscrollpane.kelvinluck.com/.
 */
 (function($){  

  trapScroll = function(opt){

    var trapElement;
    var scrollableDist;
    var trapClassName = 'trapScroll-enabled';
    var trapSelector = '#course-select';
    
    var trapWheel = function(e){
      if (!$('body').hasClass(trapClassName)) return;
      else {        
        var curScrollPos = trapElement.scrollTop();
        var wheelEvent = e.originalEvent;
        var dY = wheelEvent.deltaY;

        // only trap events once we've scrolled to the end
        // or beginning
        if ((dY>0 && curScrollPos >= scrollableDist) ||
            (dY<0 && curScrollPos <= 0)) {

          return false;
  }
}
}

$(document)
.on('wheel', trapWheel)
.on('mouseleave', trapSelector, function(){

    $('body').removeClass(trapClassName);
})
.on('mouseenter', trapSelector, function(){        

    trapElement = $(this);
    var containerHeight = trapElement.outerHeight();
        var contentHeight = trapElement[0].scrollHeight; // height of scrollable content
        scrollableDist = contentHeight - containerHeight;
        
        if (contentHeight>containerHeight)
          $('body').addClass(trapClassName);        
  });       
}   
})($);


