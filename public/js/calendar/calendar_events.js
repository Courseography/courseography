/**
 * Requests a CSV file from the server. The server automatically generates this
 * file.
 */
window.alert("I looked at this file")
var calendarRefer = document.getElementById('calendarlink');
//calendarRefer.addEventListener('click', function() {getCalendarEvents('fall')}, false);
// $('calendar').click(function({getCalendarEvents('fall')}));
calendarRefer.onclick=function(){getCalendarEvents('fall')};
//calendarDoc.onclick=getCalendarEvents('fall'){"http://localhost:8000/calendar"};
//calendarTag.onclick=function(){myscript};

//calendar.addEventListener('click', function() {
//      getCalendarEvents;
//.addEventListener('click', getCalendarEvents())

function getCalendarEvents(session) 
{
var courses = getCoursesTable(session);
// fall turns into Fall
session = session.charAt(0).toUpperCase() + session.slice(1);
// Using link
$("calendarlink").click(function()        //calendarlink is the bottom reference
{
    // Ajax call
    $.ajax(
    {url: "calendar",
        beforeSend: function()
        {
        alert("My ajax got called")
        },
    data: {courses: courses, session: session},  //key:value
       //data sent to the server
        success: function(data)         //result data is what I should return
        {
        alert("CSV file generated");
        //return courses.  THIS MAY BE DIFFERENT
        },
        error: function()
        {
        alert("Error. No CSV file generated");
        }
    });
});
}
/*
function getCalendarEvents(session) {
    'use strict';

    var events;
    var courses = getCoursesTable(session);
    session = session.charAt(0).toUpperCase() + session.slice(1);
    system.out.println("about to enter ajax call");
    $.ajax({
        url: 'calendar',
        async: false,
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
*/

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
