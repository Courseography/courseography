/**
 * Requests an image from the server. The server automatically generates this
 * image.
 */
function getGraphImage() {
    'use strict';

    $.ajax({
        url: 'image',
        success: function (data) {
            var contentDiv = $('<div></div>');
            var topContentDiv = $('<div></div>');
            var calendarOption = $('<a href="calendar" target="_blank">Download ICS</a>');
            calendarOption.attr('target', '_blank');
            var calendarOption2 = $('<button onclick="getPDF()" style="color: #6B6882; background-color: Transparent; background-repeat:no-repeat;border: none;cursor:pointer;overflow: hidden;outline:none;">Download PDF</button>');
            topContentDiv.html('<img id="post-image" src="data:image/png;base64,' + data + '" />');
            contentDiv.attr('id', 'modal-content-container')
                      .append(calendarOption)
                      .append(calendarOption2)
                      .append(topContentDiv);

            openModal('Export', contentDiv);
        },
        error: function () {
            throw 'No image generated';
        }
    });

}
