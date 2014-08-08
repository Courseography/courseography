/* Array utilities */
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


/* These specifically manipulate the two global arrays,
courseObjects and selectedLectures. */
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


function updateSelectedLectures(section) {
    if (!inArray(section.attr("id"), selectedLectures)) {
        selectedLectures.push(section.attr("id"));
    }
}


/* AJAX Functions */
function getVeryLargeCourseArray() {
    var splitArray = undefined;

    $.ajax({
        url: "js/timetable/courses.txt",
        dataType: "text",
        async: false,
        success: function (data) {
            splitArray = data.split("\n").map(function (course) {
                return course.substring(0, 8);
            });
        }
    });

    return splitArray;
}


function fetchCourse(name) {
    var course;
    $.ajax({
        url: "res/courses/" + name + ".txt",
        dataType: "json",
        async: false,
        success: function (data) {
            course = data;
        },
        error: function () {
            throw "No course file";
        }
    });
    courseCache.push(course);
    return course;
}


function getCourse(name) {
    var course = getCourseObject(name, courseCache);
    if (course === undefined) {
        course = fetchCourse(name);
    }
    return course;
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
                    var courseResult = getCourse(course);
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
    refreshStarredCourses();
}


// Highlight starred (selected) courses in search list
function refreshStarredCourses() {
    $("#search-list").find("li").each(function (index) {
        var course = $(this).text();
        if (inArray(course, selectedCourses)) {
            $(this).addClass("starred-course");
        } else {
            $(this).removeClass("starred-course");
        }
    });
}

/* Cookie Interaction */
function restoreFromCookies() {
    var starredCourseCookie = getJSONCookie("selected-courses");
    var starredLectureCookie = getJSONCookie("selected-lectures");

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

    saveCookies(newCourses, newSections);
}


function saveCookies(courses, sections) {
    setCookie("selected-courses", JSON.stringify(courses));
    setCookie("selected-lectures", JSON.stringify(sections));
}


// Used to determine if course requires manual practical enrolment
function hasManualPractical(section, index, array) {
    return (section[0].charAt(0) === "P");
}


// Used to determine if course requires manual tutorial enrolment
function hasManualTutorial(section, index, array) {
    return (section[0].charAt(0) === "T");
}


function addCourseToList(name) {
    var course = new Course(name);
    $("#course-select").append(course.render());
    courseObjects.push(course);
    selectedCourses.push(name);
    saveCookies(selectedCourses, selectedLectures);
}


function removeCourseFromList(name) {
    var courseSelector = "#" + name + "-li";
    var courseElement = $(courseSelector);
    $(courseSelector + " li[clicked*='true']").each(function() {
        $(this).click();
    });
    courseElement.remove();

    // Remove course from memory
    removeCourseObject(name);
    removeFromArray(name, selectedCourses);
    saveCookies(selectedCourses, selectedLectures);

    // Refresh starred courses
    refreshStarredCourses();
}


/* Info box */
function displayCourseTitle(course) {
    displayCourseInformation(course);
    $("#section-stats-section").html("");
    $("#section-stats-instructor").html("");
    $("#section-stats-enrol").html("");
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


function clearCourseInformation() {
    $("#course-info-code").empty();
    $("#course-info-title").empty();
    $("#section-stats-section").empty();
    $("#section-stats-instructor").empty();
    $("#section-stats-enrol").empty();
}


// Parse times for cell ids.
function convertTimes(times) {
    var timeList = [];
    var time;

    for(var i = 0; i < times.length; i++) {

        // If a course is "12", we don't want to add a "0". That would result
        // in something like "M0". We exclude this from the mod cases.
        if ((times[i][1] % 12) !== 0) {
            time = times[i][1] % 12;
        } else {
            time = times[i][1];
        }

        var timeString = "MTWRF".charAt(times[i][0]);
        timeString = timeString + time;
        timeList.push(timeString);
    }

    return timeList;
}
