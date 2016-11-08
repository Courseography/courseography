$(document).ready(function () {
  $('#nav-export').click(function () {
    openExportModal();
  });
});


/**
 * Creates and displays the Export modal content div.
 */
function openExportModal() {
    'use strict';

    var context = $('#courseography-header').attr('context');
    var session = 'fall';
    var img = (context === 'graph') ? getGraphImage() : getGridImage(session);
}


/* Request bytestring of PDF of graph and timetable and convert to PDF*/
function getPDF() {
    'use strict';

    $.ajax({
        url: 'timetable-pdf',
        success: function (data) {
            var pdfAsDataUri = "data:application/pdf;base64," + data;
            window.open(pdfAsDataUri);
        },
        error: function () {
            throw 'No pdf generated';
        }
    });
}