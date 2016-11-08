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
            // var calendarOption = $('<a href="timetable-pdf" target="_blank">Download ICS</a>');
            var calendarOption = $('<button onclick="getPDF()" style="color: #6B6882; background-color: Transparent; background-repeat:no-repeat;border: none;cursor:pointer;overflow: hidden;outline:none;">Download ICS</button>');
            calendarOption.attr('target', '_blank');
            topContentDiv.html('<img id="post-image" src="data:image/png;base64,' + data + '" />');
            contentDiv.attr('id', 'modal-content-container')
                      .append(calendarOption)
                      .append(topContentDiv);

            openModal('Export', contentDiv);
        },
        error: function () {
            throw 'No image generated';
        }
    });

}

// /* Request bytestring of PDF of graph and timetable and convert to PDF*/
// function getPDF() {
//     'use strict';

//     $.ajax({
//         url: 'timetable-pdf',
//         success: function (data) {
//             var pdfAsDataUri = "data:application/pdf;base64," + data;
//             window.open(pdfAsDataUri);
//         },
//         error: function () {
//             throw 'No pdf generated';
//         }
//     });
// }
