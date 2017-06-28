var selectedCourses = [];   // All selected Courses.
var selectedSections = [];  // All selected sections.


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
 * Highlights the selected courses in the course search list.
 */
function refreshSelectedCourses() {
    'use strict';

    $('#search-list').find('li').each(function (index) {
        var course = $(this).text();
        $(this).toggleClass('starred-course', inArray(course, selectedCourses));
    });
}


define(function() {

    // "Global" list of courses
    var courses = [];

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
        var courseList = document.createElement('ul');

        searchListObject.empty();

        if (filter !== '') {
            $.each(courses, function(i, course) {

                // If the course matches the input text then add it to the list.
                if (course.indexOf(filter) > -1) {
                    var courseEntry = document.createElement('li');

                    // Add an ID to the list so we can come back and select
                    // it when it is clicked.
                    $(courseEntry).attr('id', course + '-search')
                                  .html(course)
                                  .click(function() {
                                       $(this).toggleClass('starred-course');
                                       if (inArray(course, selectedCourses)) {
                                           deselectCourse(course);
                                       } else {
                                           selectCourse(course);
                                       }
                                   })
                                   .mouseover(function() {
                                       var courseResult = getCourse(course);
                                       renderDisplayCourseTitle(courseResult);
                                   })
                                   .mouseout(function() {
                                       renderClearCourseInformation();
                                   });
                    courseList.appendChild(courseEntry);
                }
            });
        }
        searchListObject.append(courseList);
        refreshSelectedCourses();
    }


    /* Local Storage Interaction */

    /**
     * Restores selected courses and sections from a previous session.
     */
    function restoreFromLocalStorage() {
        'use strict';

        var selectedCourseLocalStorage = localStorage.getItem('selected-courses');
        var selectedSectionLocalStorage = localStorage.getItem('selected-lectures');

        if (!selectedCourseLocalStorage) {
            selectedCourseLocalStorage = [];
        } else {
            selectedCourseLocalStorage = selectedCourseLocalStorage.split('_');
        }

        if (!selectedSectionLocalStorage) {
            selectedSectionLocalStorage = [];
        } else {
            selectedSectionLocalStorage = selectedSectionLocalStorage.split('_');
        }


        if (selectedCourseLocalStorage.length > 0) {
            var newCourses = [];
            $.each(selectedCourseLocalStorage, function (i, course) {
                try {
                    selectCourse(course);
                    newCourses.push(course);
                } catch (e) {
                    console.log('Removed bad course from local storage: ' + course);
                    console.log(e);
                }
            });
        }

        if (selectedSectionLocalStorage.length > 0) {
            var newSections = [];
            $.each(selectedSectionLocalStorage, function (i, section) {
                try {
                    $('#' + section).click();
                    newSections.push(section);
                } catch (e) {
                    console.log('Removed bad section from local storage: ' + section);
                    console.log(e);
                }
            });
        }

        saveLocalStorage(newCourses, newSections);
    }

    /**
     * Renders the button that allows the user to clear
     * all selected courses.
     */
    function renderClearAllButton() {
        'use strict';

        var clearAllItem = document.getElementById('clear-all');
        $(clearAllItem).click(function () {
            if (confirm('Clear all selected courses?')) {
                $.each(courseObjects.slice(0), function (i, course) {
                    deselectCourse(course.name);
                });
            }
        });
    }

    /**
     * Selects a course.
     * @param {string} courseCode The course code.
     */
    function selectCourse(courseCode) {
        'use strict';

        var course = new Course(courseCode);
        $('#course-select').append(course.render());
        courseObjects.push(course);
        selectedCourses.push(courseCode);
        saveLocalStorage(selectedCourses, selectedSections);
    }

    return {
        getVeryLargeCourseArray: getVeryLargeCourseArray,
        enableSearch: enableSearch,
        restoreFromLocalStorage: restoreFromLocalStorage,
        renderClearAllButton: renderClearAllButton
    };
});

/* Local Storage Interaction */

/**
 * Stores courses and sections in local storage.
 * @param {string[]} courses All selected courses.
 * @param {string[]} sections All selected sections.
 */
function saveLocalStorage(courses, sections) {
    'use strict';

    if (courses !== undefined) {
        localStorage.setItem("selected-courses", courses.join('_'));
    } else {
        localStorage.setItem("selected-courses", "");
    }

    if (sections !== undefined) {
        localStorage.setItem("selected-lectures", sections.join('_'));
    } else {
        localStorage.setItem("selected-lectures", "");
    }
}


/**
 * Deselects a course.
 * @param {string} courseCode The course code.
 */
function deselectCourse(courseCode) {
    'use strict';

    var courseSelector = '#' + courseCode + '-li';
    var courseElement = $(courseSelector);
    $(courseSelector + ' li[clicked*="true"]').each(function() {
        $(this).click();
    });
    courseElement.remove();

    // Remove course from memory
    removeCourseObject(courseCode);
    removeFromArray(courseCode, selectedCourses);

    saveLocalStorage(selectedCourses, selectedSections);

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
