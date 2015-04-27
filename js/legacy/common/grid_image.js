'use strict'

import $ from 'jquery'

/**
 * Requests an image from the server. The server automatically generates this
 * image.
 * @returns {String} The base64 representation of an image.
 */
export function getGridImage(session) {
    'use strict';

    var img;
    var courses = getCoursesTable(session);
    session = session.charAt(0).toUpperCase() + session.slice(1);
    $.ajax({
        url: 'timetable-image',
        async: false,
        data: {courses: courses, session: session},
        success: function (data) {
            img = data;
        },
        error: function () {
            throw 'No image generated';
        }
    });

    return img;
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
