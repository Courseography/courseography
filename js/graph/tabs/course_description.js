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

    if (name in courseDescriptions) {
        return courseDescriptions[name];
    }

    var result = '';
    $.ajax({
        url: 'res/courses/' + name + '.txt',
        dataType: 'json',
        async: false,
        success: function (data) {
            result += '<h3>' + data.name + ': ' + data.title + '</h3>';
            result += '<p>' + data.description + '</p>';
            if (data.prereqString !== undefined && data.prereqString !== null) {
                result += '<p><strong>Prerequisite:</strong> ' + data.prereqString + '</p>';
            }
            if (data.prep !== undefined && data.prep !== null) {
                result += '<p><strong>Recommended Preparation:</strong> ' + data.prep + '</p>';
            }
            if (data.exclusions !== undefined && data.exclusions !== null) {
                result += '<p><strong>Exclusions:</strong> ' + data.exclusions + '</p>';
            }

            result += '<p><strong>Distribution Requirement Status:</strong> ' + data.distribution + '</p>';
            result += '<p><strong>Breadth Requirement:</strong> ' + data.breadth + '</p>';
        }
    });
    courseDescriptions[name] = result;
    return result;
}