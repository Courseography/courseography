/**
 * Requests a CSV file from the server. The server automatically generates this
 * file.
 */

var calendarRefer = document.getElementById('calendarlink');
//calendarRefer.addEventListener('click', function() {getCalendarEvents('fall')}, false);
// $('calendar').click(function({getCalendarEvents('fall')}));
calendarRefer.onclick=function(){getCalendarEvents('fall')};
//calendarDoc.onclick=getCalendarEvents('fall'){"http://localhost:8000/calendar"};
//calendarTag.onclick=function(){myscript};

//calendar.addEventListener('click', function() {
//      getCalendarEvents;
//.addEventListener('click', getCalendarEvents())

function getCalendarEvents(session) {
    'use strict';

    var events;
    var courses = getCoursesTable(session);
    session = session.charAt(0).toUpperCase() + session.slice(1);
    $.ajax({
        url: 'calendar',
        data: {courses: courses, session: session},
        success: function (data) {
            events = data;
            //MMM call back data
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
