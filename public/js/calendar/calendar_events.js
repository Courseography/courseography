/**
 * Requests a CSV file from the server. The server automatically generates this
 * file.
 */

var calendarRefer = document.getElementById('calendarlink');
calendarRefer.onclick = function() {getCalendarEvents()};


function getCalendarEvents() {
    'use strict';

    var events;
    var coursesFall = getCoursesTable('fall');
    var coursesWinter = getCoursesTable('winter');
    
    $.ajax({
        url: 'calendar',
        async: false,
        data: {coursesFall: coursesFall, coursesWinter: coursesWinter},
        success: function (data) {
            events = data;
        },
        error: function () {
            throw 'No CSV file generated';
        }
    });
    return events;
}


function getCoursesTable(session) {
    'use strict';

    var sessionChar = session === 'fall' ? 'F' : 'S';
    var days = ['M', 'T', 'W', 'R', 'F'];
    var courses = '';
    for (var i = 8; i < 22; i++) {
        for (var j = 0; j < 5; j++) {
            courses += $('#' + days[j] + i + '-0' + sessionChar).text();
            courses += '_';
        }
    }

    return courses
}
