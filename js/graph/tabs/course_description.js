// Read course description from resource files
function fetchCourseDescription(id) {
    'use strict';

    var result;
    if (id === 'CSC200') {
        result = readCalendarEntry(id + 'Y1');
    } else if (id === 'Calc1') {
        result = readCalendarEntry('MAT135H1') + readCalendarEntry('MAT136H1') + readCalendarEntry('MAT137Y1') + readCalendarEntry('MAT157Y1');
    } else if (id === 'Lin1') {
        result = readCalendarEntry('MAT221H1') + readCalendarEntry('MAT223H1') + readCalendarEntry('MAT240H1');
    } else if (id === 'Sta1') {
        result = readCalendarEntry('STA247H1') + readCalendarEntry('STA255H1');
    } else if (id === 'Sta2') {
        result = readCalendarEntry('STA248H1') + readCalendarEntry('STA261H1');
    } else {
        result = readCalendarEntry(id + 'H1');
    }

    return result;
}


function readCalendarEntry(name) {
    'use strict';

    var course = new Course(name);
    return formatCourseDescription(course);
}

function formatCourseDescription(course) {
    var courseDescription = '<h3>' + course.name + ': ' + course.title + '</h3>';
    courseDescription += '<p>' + course.description + '</p>';
    
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