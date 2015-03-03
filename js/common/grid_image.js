/**
 * Requests an image from the server. The server automatically generates this
 * image.
 * @returns {String} The base64 representation of an image.
 */
function getGridImage() {
    'use strict';

    var img;
    var str = '';

    $('td').each(function (i, elem) {
        if ($(elem).attr("class") !== 'timetable-time' &&
            !($(elem).attr("id").indexOf('.') > -1) &&
            ($(elem).attr("id").indexOf('S') == -1)) {
            str = str + $(elem).html() + '%0A';
        }
    });

    $.ajax({
        url: 'timetable-image?courses=' + str,
        async: false,
        success: function (data) {
            img = data;
        },
        error: function () {
            throw 'No image generated';
        }
    });

    return img;
}
