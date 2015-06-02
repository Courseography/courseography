/**
 * Requests a CSV file from the server. The server automatically generates this
 * file.
 */

/*document.addEventListener('calendar', function() {
    var calendar = document.getElementById('calendar');
    // onClick's logic below:
    calendar.addEventListener('click', function() {
        getCalendarEvents;
    });
});

//.addEventListener('click', getCalendarEvents())
*/

var calendarTag = document.getElementsByTagName('calendar');
//calendarTag.onclick=getCalendarEvents("fall"){"hs/MasterTemplate.hs"};
//calendarTag.onclick=getCalendarEvents("fall"){"/hs/MasterTemplate.hs"};
//calendarTag.onclick=getCalendarEvents("fall"){"courseography/hs/MasterTemplate.hs"};
//calendarTag.onclick=getCalendarEvents("fall"){"static/hs/MasterTemplate.hs"};
calendarTag.onclick=function(){getCalendarEvents('fall')};
//calendarTag.onclick=function(){myscript};

//calendar.addEventListener('click', function() {
//      getCalendarEvents;

function getCalendarEvents(session) {
    'use strict';

    var events;
    var courses = getCoursesTable(session);
    session = session.charAt(0).toUpperCase() + session.slice(1);
    $.ajax({
        url: 'calendar',
        async: false,
        data: {courses: courses, session: session},
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
