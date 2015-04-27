'use strict';

/**
 * Requests an image from the server. The server automatically generates this
 * image.
 * @returns {String} The base64 representation of an image.
 */
function getGraphImage() {
    var img;

    $.ajax({
        url: 'image',
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
