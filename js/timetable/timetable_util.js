var trapScroll;
var selectedCourses = [];
var selectedLectures = [];
var courseObjects = [];

function removeCourseObject(courseName) {
    var index = -1;
    for (var i = 0; i < courseObjects.length; i++) {
        if (courseName === courseObjects[i].name) {
            index = i;
            break;
        }
    }
    if (index > -1) {
        courseObjects.splice(index, 1);
    }
}

function getCourseObject(courseName) {
    var courseObject;
    for (var i = 0; i < courseObjects.length; i++) {
        if (courseObjects[i].name === courseName) {
            courseObject = courseObjects[i];
        }
    }
    return courseObject;

}

/*
 * Adapted from http://codepen.io/LelandKwong/pen/edAmn. Will look into http://jscrollpane.kelvinluck.com/.
 */
 (function($) {
    trapScroll = function(){
        var trapElement;
        var scrollableDist;
        var trapClassName = "trapScroll-enabled";
        var trapSelector = "#course-select";

        var trapWheel = function(e){
            if (!$("body").hasClass(trapClassName)) {
                return;
            } else {
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
        };

        $(document)
        .on("wheel", trapWheel)
        .on("mouseleave", trapSelector, function() {
            $("body").removeClass(trapClassName);
        })
        .on("mouseenter", trapSelector, function() {
            trapElement = $(this);
            var containerHeight = trapElement.outerHeight();
            var contentHeight = trapElement[0].scrollHeight; // height of scrollable content
            scrollableDist = contentHeight - containerHeight;

            if (contentHeight > containerHeight) {
                $("body").addClass(trapClassName);
            }
        });
    };
})($);


function setAccordion() {
    $("#course-select li").accordion({heightStyle: "content", collapsible: true, active: false});
}


// Search function for timetable
function enableSearch() {
    $("#course-filter").keyup(function() {
        resetSearchList();
    });
}

function resetSearchList() {
    var courseList;
    var courseEntry;
    var counter = 0;
    var index;
    var filter = $("#course-filter").val().toLowerCase();
    $('#search-list').empty();
    courseList = document.createElement("ul");
    if (filter !== "") {
        $.each(courses, function(i, course) {

            // If the course matches and if there are fewer than 100 courses in the list, add it to the list.
            if (course.toLowerCase().indexOf(filter) > -1 && counter < 100) {
                courseEntry = document.createElement("li");

                // "Star" the course if it has been previously selected.
                if ($.inArray(course, selectedCourses) > -1) {
                    $(courseEntry).addClass("starred-course");
                }

                // Add an ID to the list so we can come back and star it when it is clicked.
                $(courseEntry).attr("id", course + "-search");
                courseEntry.innerHTML = course;
                $(courseEntry).click(function() {
                    $(this).toggleClass("starred-course")
                    if ($.inArray(course, selectedCourses) > -1) {
                        removeCourseFromList(course);
                    } else {
                        addCourseToList(course);
                    }
                });

                counter++;
                courseList.appendChild(courseEntry);
            }
        });
    }
    searchList.appendChild(courseList);
}

function restoreFromCookies() {
    var starredCourseCookie = getCookie("selected-courses");
    if (starredCourseCookie.length > 0) {
        var selectedCoursesTemp = $.parseJSON(starredCourseCookie);
        $.each(selectedCoursesTemp, function (i, course) {
            addCourseToList(course);
        });
    }

    var starredLectureCookie = getCookie("selected-lectures");
    if (starredLectureCookie.length > 0) {
        selectedLectures = $.parseJSON(starredLectureCookie);
        $.each(selectedLectures, function (i, course) {
            $("#" + course).click();
        });
    }
}

function convertTimes(times) {
    var timeList = [];
    var timeString;
    var days = "MTWRF";
    var time;

    for(var i = 0; i < times.length; i++) {

        // If a course is "12", we don't want to add a "0". That would result in something like "M0". We exclude this from the mod cases.
        if ((times[i][1] % 12) !== 0) {
            time = times[i][1] % 12;
        } else {
            time = times[i][1];
        }

        timeString = days.charAt(times[i][0]);
        timeString = timeString + time;
        timeList.push(timeString);
    }

    return timeList;
}

function addCourseToList(course) {
    var courseObject = getCourse(course);
    courseObject.isLectureSelected = false;
    courseObject.isTutorialSelected = false;
    courseObject.isPracticalSelected = false;
    courseObject.tutorialEnrolment = false;
    courseObject.practicalEnrolement = false;
    if (courseObject.manualTutorialEnrolment) {
        $.each(courseObject.S.tutorials, function(i, tutorial) {
            if (tutorial[0].charAt(0) === "P") {
                courseObject.practicalEnrolment = true;
            } else if (tutorial[0].charAt(0) === "T") {
                courseObject.tutorialEnrolment = true;
            }
        });
    }
    courseObject.status = "inactive";
    setupEntry(courseObject);

    selectedCourses.push(course);
    var jsonCookie = JSON.stringify(selectedCourses);
    setCookie("selected-courses", jsonCookie);
}

function removeCourseFromList(course) {
    var courseElement = document.getElementById(course + "-li");
    $("#" + course + "-li" + " li[clicked*='true']").each(function() {
        $(this).click();
    });
    courseSelect.removeChild(courseElement);

    var index = $.inArray(course, selectedCourses);
    selectedCourses.splice(index, 1);
    var jsonCookie = JSON.stringify(selectedCourses);
    setCookie("selected-courses", jsonCookie);

    resetSearchList();

    removeCourseObject(course);
}
