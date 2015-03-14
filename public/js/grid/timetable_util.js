/**
 * Updates selectedSections with sectionId.
 * @param {string} sectionId The ID of the lecture section being updated.
 */
function updateSelectedLectures(sectionId) {
    'use strict';

    if (!inArray(sectionId, selectedSections)) {
        selectedSections.push(sectionId);
    }
}


/**
 * Populate the global list of courses.
 */
function getVeryLargeCourseArray() {
    'use strict';

    $.ajax({
        url: "all-courses",
        dataType: "text",
        async: false,
        success: function (data) {
            courses = data.split('\n').map(function (course) {
                return course.substring(0, 8);
            });
        }
    });
}


/**
 * Enables course search functionality.
 */
function enableSearch() {
    'use strict';
    $('#course-filter').keyup(function() {
        resetSearchList();
    });
}


/**
 * Resets the course search list.
 * TODO: Function is a bit lengthy and convoluted.
 */
function resetSearchList() {
    'use strict';

    var searchListObject = $('#search-list');

    var filter = $('#course-filter').val().toUpperCase();
    searchListObject.empty();
    var courseList = document.createElement('ul');
    if (filter !== '') {
        $.each(courses, function(i, course) {
            var counter = 0;

            // If the course matches and if there are fewer than
            // 100 courses in the list, add it to the list.
            if (course.indexOf(filter) > -1 && counter < 100) {
                var courseEntry = document.createElement('li');

                // Add an ID to the list so we can come back and select
                // it when it is clicked.
                $(courseEntry).attr('id', course + '-search')
                              .html(course)
                              .click(function() {
                                   $(this).toggleClass('starred-course');
                                   if (inArray(course, selectedCourses)) {
                                       removeCourseFromList(course);
                                   } else {
                                       addCourseToList(course);
                                   }
                               })
                               .mouseover(function() {
                                   var courseResult = getCourse(course);
                                   renderDisplayCourseTitle(courseResult);
                               })
                               .mouseout(function() {
                                   renderClearCourseInformation();
                               });

                counter++;
                courseList.appendChild(courseEntry);
            }
        });
    }
    searchListObject.append(courseList);
    refreshSelectedCourses();
}


/**
 * Highlights the selected courses in the course search list.
 */
function refreshSelectedCourses() {
    'use strict';

    $('#search-list').find('li').each(function (index) {
        var course = $(this).text();
        if (inArray(course, selectedCourses)) {
            $(this).addClass('starred-course');
        } else {
            $(this).removeClass('starred-course');
        }
    });
}


/* Cookie Interaction */


/**
 * Restores selected courses and sections from a previous session.
 */
function restoreFromCookies() {
    'use strict';

    var selectedCourseCookie = getCookie('selected-courses');
    var selectedSectionCookie = getCookie('selected-lectures');

    if (selectedCourseCookie.length === 0) {
        selectedCourseCookie = [];
    }

    if (selectedSectionCookie.length === 0) {
        selectedSectionCookie = [];
    }

    if (selectedCourseCookie.length > 0) {
        var selectedCoursesTemp = $.parseJSON(selectedCourseCookie);
        var newCourses = [];
        $.each(selectedCoursesTemp, function (i, course) {
            try {
                addCourseToList(course);
                newCourses.push(course);
            } catch (e) {
                console.log('Removed bad course from cookie: ' + course);
                console.log(e);
            }
        });
    }

    if (selectedSectionCookie.length > 0) {
        selectedSections = $.parseJSON(selectedSectionCookie);
        var newSections = [];
        $.each(selectedSections, function (i, section) {
            try {
                $('#' + section).click();
                newSections.push(section);
            } catch (e) {
                console.log('Removed bad section from cookie: ' + section);
                console.log(e);
            }

        });
    }

    saveCookies(newCourses, newSections);
}


/**
 * Stores courses and sections in cookies.
 * @param {string[]} courses All selected courses.
 * @param {string[]} sections All selected sections.
 */
function saveCookies(courses, sections) {
    'use strict';

    if (courses !== undefined) {
        setCookie("selected-courses", JSON.stringify(courses));
    } else {
        setCookie("selected-courses", "[]");
    }

    if (sections !== undefined) {
        setCookie("selected-lectures", JSON.stringify(sections));
    } else {
        setCookie("selected-lectures", "[]");
    }
}


/**
 * Selects a course.
 * TODO: Bad function name
 * TODO: Bad param name
 * @param {string} name The course code.
 */
function addCourseToList(name) {
    'use strict';

    var course = new Course(name);
    $('#course-select').append(course.render());
    courseObjects.push(course);
    selectedCourses.push(name);
    saveCookies(selectedCourses, selectedSections);
}


/**
 * Deselects a course.
 * TODO: Bad function name
 * TODO: Bad param name
 * @param {string} name The course code.
 */
function removeCourseFromList(name) {
    'use strict';

    var courseSelector = '#' + name + '-li';
    var courseElement = $(courseSelector);
    $(courseSelector + ' li[clicked*="true"]').each(function() {
        $(this).click();
    });
    courseElement.remove();

    // Remove course from memory
    removeCourseObject(name);
    removeFromArray(name, selectedCourses);

    saveCookies(selectedCourses, selectedSections);

    // Refresh selected courses
    refreshSelectedCourses();
}


/* Info box */


/**
 * Displays course's title.
 * TODO: Bad function name
 * @param {Course} course The course.
 */
function renderDisplayCourseTitle(course) {
    'use strict';

    renderDisplayCourseInformation(course);
    $('#section-stats-section').empty();
    $('#section-stats-instructor').empty();
    $('#section-stats-enrol').empty();
}


/**
 * Displays course's information.
 * @param {Course} course The course.
 */
function renderDisplayCourseInformation(course) {
    'use strict';

    $('#course-info-code').html(course.name);
    $('#course-info-title').html(course.title);
}


/**
 * Displays section's information.
 * @param {Section} section The section.
 */
function renderDisplaySectionInformation(section) {
    'use strict';

    $('#section-stats-section').html(section.name);
    $('#section-stats-instructor').html(section.instructor);
    var cap = section.cap;
    var enrol = section.enrol;
    var wait = section.wait;
    if (cap !== undefined && enrol !== undefined) {
        var enrolString = (cap - enrol) + ' out of ' + cap +
            ' spots remaining';
        if (wait !== undefined && wait !== 0) {
            enrolString += '; ' + wait + ' students on the waitlist';
        }
        $('#section-stats-enrol').html(enrolString);
    }
}


/**
 * Clears displayed course information.
 */
function renderClearCourseInformation() {
    'use strict';

    $('#course-info-code').empty();
    $('#course-info-title').empty();
    $('#section-stats-section').empty();
    $('#section-stats-instructor').empty();
    $('#section-stats-enrol').empty();
}
