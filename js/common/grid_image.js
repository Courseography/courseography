/**
 * Requests an image from the server. The server automatically generates this
 * image.
 * @returns {String} The base64 representation of an image.
 */
function getGridImage() {
    'use strict';

    var img;
    var courses = getCoursesTable();

    $.ajax({
        url: 'timetable-image',
        async: false,
        data: "courses=" + courses,
        success: function (data) {
            img = data;
        },
        error: function () {
            throw 'No image generated';
        }
    });

    return img;
}

function getCoursesTable() {
    var days = ["M", "T", "W", "R", "F"];
    var courses = "";
    for (var i = 8; i < 22; i++) {
        for (var j = 0; j < 5; j++) {
            courses += $("#" + days[j] + i + "F").text();
            courses += "\n";
        }
    }

    return courses
}
