/**
 * Requests an image from the server. The server automatically generates this
 * image. Displays the image in a new modal.
 */
function getGridImage(session) {
    'use strict';

    session = session.charAt(0).toUpperCase() + session.slice(1);
    var courses = getCoursesTable(session);
    $.ajax({
        url: 'timetable-image',
        data: {courses: courses, session: session},
        success: function (data) {
            var contentDiv = $('<div></div>');
            var topContentDiv = $('<div></div>');
            var calendarOption = $('<a href="calendar">Download ICS</a>');
            calendarOption.attr('target', '_blank');
            topContentDiv.html('<img id="post-image" src="data:image/png;base64,' + data + '" />');
            contentDiv.attr('id', 'modal-content-container')
                      .append(calendarOption)
                      .append(topContentDiv);

            var sessionButton = $('<button type="button" class="btn btn-primary" id="switch-session-button">Switch Sessions</button>');
            sessionButton.click(function () {
                session = session === 'Fall' ? 'Spring' : 'Fall';
                updateGridImage(session);
            });
            contentDiv.append(sessionButton);
            openModal('Export', contentDiv);
        },
        error: function () {
            throw 'No image generated';
        }
    });
}


/**
 * Requests an image from the server, and updates the existing modal.
 */
function updateGridImage(session) {
    var courses = getCoursesTable(session);
    session = session.charAt(0).toUpperCase() + session.slice(1);
    $.ajax({
        url: 'timetable-image',
        data: {courses: courses, session: session},
        success: function (data) {
            $('#post-image').attr('src', 'data:image/png;base64,' + data);
            $('#switch-session-button').unbind('click').click(function () {
                session = session === 'Fall' ? 'Spring' : 'Fall';
                updateGridImage(session);
            });
        },
        error: function () {
            throw 'No image generated';
        }
    });
}


function getCoursesTable(session) {
    'use strict';

    var sessionChar = session === 'Fall' ? 'F' : 'S';
    var days = ['M', 'T', 'W', 'R', 'F'];
    var courses = '';
    for (var i = 8; i < 22; i++) {
        for (var j = 0; j < 5; j++) {
            courses += $('#' + days[j] + i + '-0' + sessionChar).text();
            courses += '_';
        }
    }

    return courses;
}
