var trapScroll;
var selectedCourses = [];
var selectedLectures = [];
var courseObjects = [];


/* Array utilities */
function inArray(item, array) {
    return $.inArray(item, array) > -1;
}


function removeCourseObject(courseName) {
    for (var i = 0; i < courseObjects.length; i++) {
        if (courseName === courseObjects[i].name) {
            courseObjects.splice(i, 1)
            break;
        }
    }
}


function getCourseObject(courseName, courseArray) {
    for (var i = 0; i < courseArray.length; i++) {
        if (courseArray[i].name === courseName) {
            return courseArray[i];
        }
    }
    return undefined;
}


/* Timetable Search List */
function enableSearch() {
    $("#course-filter").keyup(function() {
        resetSearchList();
    });
}


function resetSearchList() {
    var filter = $("#course-filter").val().toUpperCase();
    $("#search-list").empty();
    var courseList = document.createElement("ul");
    if (filter !== "") {
        $.each(courses, function(i, course) {
            var counter = 0;

            // If the course matches and if there are fewer than
            // 100 courses in the list, add it to the list.
            if (course.indexOf(filter) > -1 && counter < 100) {
                var courseEntry = document.createElement("li");

                // "Star" the course if it has been previously selected.
                if (inArray(course, selectedCourses)) {
                    $(courseEntry).addClass("starred-course");
                }

                // Add an ID to the list so we can come back and star
                // it when it is clicked.
                $(courseEntry).attr("id", course + "-search");
                courseEntry.innerHTML = course;
                $(courseEntry).click(function() {
                    $(this).toggleClass("starred-course");
                    if (inArray(course, selectedCourses)) {
                        removeCourseFromList(course);
                    } else {
                        addCourseToList(course);
                    }
                })
                .mouseover(function() {
                    var courseResult = fetchCourse(course);
                    displayCourseTitle(courseResult);
                })
                .mouseout(function() {
                    clearCourseInformation();
                });

                counter++;
                courseList.appendChild(courseEntry);
            }
        });
    }
    $("#search-list").append(courseList);
}


/* Cookie Interaction */
function restoreFromCookies() {
    var starredCourseCookie = getJSONCookie("selected-courses");
    if (starredCourseCookie.length > 0) {
        var selectedCoursesTemp = $.parseJSON(starredCourseCookie);
        var newCourses = [];
        $.each(selectedCoursesTemp, function (i, course) {
            try {
                addCourseToList(course);
                newCourses.push(course);
            } catch (e) {
                console.log("Removed bad course from cookie: " + course);
                console.log(e);
            }
        });
    }

    var starredLectureCookie = getJSONCookie("selected-lectures");
    if (starredLectureCookie.length > 0) {
        selectedLectures = $.parseJSON(starredLectureCookie);
        var newSections = [];
        $.each(selectedLectures, function (i, section) {
            try {
                $("#" + section).click();
                newSections.push(section);
            } catch (e) {
                console.log("Removed bad section from cookie: " + section);
            }
            
        });
    }

    setCookie("selected-courses", JSON.stringify(newCourses));
    setCookie("selected-lectures", JSON.stringify(newSections));
}


function convertTimes(times) {
    var timeList = [];
    var timeString;
    var days = "MTWRF";
    var time;

    for(var i = 0; i < times.length; i++) {

        // If a course is "12", we don't want to add a "0". That would result
        // in something like "M0". We exclude this from the mod cases.
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


// Used to determine if course requires manual practical enrolment
function hasManualPractical(section, index, array) {
    return (section[0].charAt(0) === "P");
}


// Used to determine if course requires manual tutorial enrolment
function hasManualTutorial(section, index, array) {
    return (section[0].charAt(0) === "T");
}


function addCourseToList(courseName) {
    var course = getCourse(courseName);
    course.isLectureSelected = false;
    course.isTutorialSelected = false;
    course.isPracticalSelected = false;
    if (course.manualTutorialEnrolment) {
        if (course.Y !== undefined) {
            course.practicalEnrolment = course.Y.tutorials.some(hasManualPractical);
            course.tutorialEnrolment = course.Y.tutorials.some(hasManualTutorial);
        } else if (course.F !== undefined) {
            course.practicalEnrolment = course.F.tutorials.some(hasManualPractical);
            course.tutorialEnrolment = course.F.tutorials.some(hasManualTutorial);
        } else {
            course.practicalEnrolment = course.S.tutorials.some(hasManualPractical);
            course.tutorialEnrolment = course.S.tutorials.some(hasManualTutorial);
        }
    }
    course.status = "inactive";
    $("#course-select").append(renderEntry(course));

    selectedCourses.push(courseName);
    var jsonCookie = JSON.stringify(selectedCourses);
    setCookie("selected-courses", jsonCookie);
}


function renderEntry(course) {
    var entry = document.createElement("li");
    entry.id = course.name + "-li";
    
    var header = renderHeader(course.name);
    entry.appendChild(header);
    course.header = header;

    sections = processSession(course);
    
    entry.appendChild(sections);
    $(entry).accordion({
        heightStyle: "content",
        collapsible: true,
        active: false
    });
    
    return entry;
}


function renderHeader(name) {
    var header = document.createElement("h3");
    header.appendChild(document.createTextNode(name));
    
    var courseImg = document.createElement("img");
    $(courseImg).attr("src", "res/ico/delete.ico")
                .addClass("close-icon")
                .click(function () {
                    removeCourseFromList(course.name);
                });
    header.appendChild(courseImg);
    
    return header;
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


function displayCourseTitle(course) {
    $("#course-info-code").html(course.name);
    $("#course-info-title").html(course.title);
    $("#section-stats-section").html("");
    $("#section-stats-instructor").html("");
    $("#section-stats-enrol").html("");
}


/**
 * Adapted from http://codepen.io/LelandKwong/pen/edAmn.
 * Will look into http://jscrollpane.kelvinluck.com/.
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