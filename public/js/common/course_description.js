/**
 * Returns a course's description.
 * @param {string} id The Node's id.
 * @returns {string} The course description.
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
 * Returns and formats all course codes in id.
 * @param {string} id The Node's ID.
 * @returns {string[]} All formatted course codes.
 * TODO: Change function name
 */
function formatCourseName(id) {
    'use strict';

    var names;

    if (id === 'CSC200') {
        names = id + 'Y1';
    } else if (id === 'Calc1') {
        names = 'MAT135H1' + ' ' + 'MAT136H1' + ' ' + 'MAT137Y1' + ' ' +
                'MAT157Y1';
    } else if (id === 'Lin1') {
        names = 'MAT221H1' + ' ' + 'MAT223H1' + ' ' + 'MAT240H1';
    } else if (id === 'Sta1') {
        names = 'STA247H1' + ' ' + 'STA255H1';
    } else if (id === 'Sta2') {
        names = 'STA248H1' + ' ' + 'STA261H1';
    } else if (id.indexOf('H1', id.length - 2) !== -1) {
        names = id;
    } else {
        names = id + 'H1';
    }

    names = names.split(" ");
    return names;
}


/**
 * Returns a formatted version of a course's description.
 * TODO: Duplicate description
 * @param {string} name The name of the course.
 * @returns {*} A formatted version of a course's description.
 */
function readCalendarEntry(name) {
    'use strict';

    var course = new Course(name);
    return formatCourseDescription(course);
}


/**
 * Returns a formatted version of the course's description.
 * @param {Course} course The course.
 * @returns {string} The Course's description.
 */
function formatCourseDescription(course) {
    'use strict';

    var courseDescription = '<p>' + course.description + '</p>';

    if (course.prereqString !== undefined && course.prereqString !== null) {
        courseDescription += '<p><strong>Prerequisite:</strong> ' +
                             course.prereqString + '</p>';
    }
    if (course.coreqs !== undefined && course.coreqs !== null) {
        courseDescription += '<p><strong>Corequisites:</strong> ' +
                             course.coreqs + '</p>';
    }
    if (course.prep !== undefined && course.prep !== null) {
        courseDescription += '<p><strong>Recommended Preparation:</strong> ' +
                             course.prep + '</p>';
    }
    if (course.exclusions !== undefined && course.exclusions !== null) {
        courseDescription += '<p><strong>Exclusions:</strong> ' +
                             course.exclusions + '</p>';
    }

    courseDescription += '<p><strong>Distribution Requirement Status:</strong> ' +
                         course.distribution + '</p>';
    courseDescription += '<p><strong>Breadth Requirement:</strong> ' +
                          course.breadth + '</p>';

    return courseDescription;
}


/**
 * Returns a course's title.
 * @param {string} id The Node's ID.
 * @param {string[]} formatted The formatted Course Description.
 * @returns {string} The course's title.
 */
function getCourseTitle(id, formatted) {
    'use strict';

    var name = formatted; 
    if (name.length === 1) {
        var course = new Course(name[0]);
        name = course.title;
    }

    return id.toUpperCase() + ': ' + name;
}
