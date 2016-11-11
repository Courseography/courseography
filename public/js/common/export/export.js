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
