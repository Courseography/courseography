$(document).ready(function () {
  $('#nav-export').click(function () {
    openModal('Export', createExportModalDiv());
  });
});

/**
 * Creates and returns the Facebook modal content div.
 * @returns {jQuery} The Facebook modal content div.
 */
function createExportModalDiv() {
    'use strict';

    var context = $('#courseography-header').attr('context');
    var session = 'fall';
    var img = (context === 'graph') ? getGraphImage() : getGridImage(session);
    var contentDiv = $('<div></div>');
    var topContentDiv = $('<div></div>');
    contentDiv.attr('id', 'modal-content-container');
    topContentDiv.html('<img id="post-image" src="data:image/png;base64,' + img + '" />');
    contentDiv.append(topContentDiv);
    contentDiv.attr('id', 'modal-content-container');

    if (context === 'grid') {
        var sessionButton = $('<button type="button" class="btn btn-primary">Switch Sessions</button>');
        sessionButton.click(function () {
            session = session === 'fall' ? 'spring' : 'fall';
            img = getGridImage(session);
            $('#post-image').attr('src', 'data:image/png;base64,' + img);
        });
        contentDiv.append(sessionButton);
    }

    return contentDiv;
}
