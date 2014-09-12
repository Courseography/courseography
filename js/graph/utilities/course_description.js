/**
 *
 * @param id
 * @returns {string}
 */
function fetchCourseDescription(id) {
    'use strict';

    var result = '';
    var names = formatCourseName(id);
    $.each(names, function (i, name) {
        result += readCalendarEntry(name);
    });

    return result;
}


/**
 *
 * @param id
 * @returns {Array}
 */
function formatCourseName(id) {
    'use strict';

    var names;

    if (id === 'CSC200') {
        names = id + 'Y1';
    } else if (id === 'Calc1') {
        names = 'MAT135H1' + ' ' + 'MAT136H1' + ' ' + 'MAT137Y1' + ' ' + 'MAT157Y1';
    } else if (id === 'Lin1') {
        names = 'MAT221H1' + ' ' + 'MAT223H1' + ' ' + 'MAT240H1';
    } else if (id === 'Sta1') {
        names = 'STA247H1' + ' ' + 'STA255H1';
    } else if (id === 'Sta2') {
        names = 'STA248H1' + ' ' + 'STA261H1';
    } else {
        names = id + 'H1';
    }

    names = names.split(" ");
    return names;
}


/**
 *
 * @param name
 * @returns {*}
 */
function readCalendarEntry(name) {
    'use strict';

    var course = new Course(name);
    return formatCourseDescription(course);
}


/**
 *
 * @param course
 * @returns {string}
 */
function formatCourseDescription(course) {
    'use strict';

    var courseDescription = '<p>' + course.description + '</p>';

    if (course.prereqString !== undefined && course.prereqString !== null) {
        courseDescription += '<p><strong>Prerequisite:</strong> ' + course.prereqString + '</p>';
    }
    if (course.prep !== undefined && course.prep !== null) {
        courseDescription += '<p><strong>Recommended Preparation:</strong> ' + course.prep + '</p>';
    }
    if (course.exclusions !== undefined && course.exclusions !== null) {
        courseDescription += '<p><strong>Exclusions:</strong> ' + course.exclusions + '</p>';
    }

    courseDescription += '<p><strong>Distribution Requirement Status:</strong> ' + course.distribution + '</p>';
    courseDescription += '<p><strong>Breadth Requirement:</strong> ' + course.breadth + '</p>';

    return courseDescription;
}


/**
 *
 * @param id
 * @returns {string}
 */
function getCourseTitle(id) {
    'use strict';
    var name = formatCourseName(id);
    if (name.length === 1) {
        var course = new Course(name);
        name = course.title;
    }
    return id + ': ' + name;
}