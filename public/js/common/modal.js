/**
 * Creates and returns the main modal content div.
 * @param {string} courseCode The course code.
 * @returns {jQuery} The main modal content div.
 */
function createModalDiv(courseCode) {
    'use strict';

    var contentDiv = $('<div></div>');
    contentDiv.attr('id', 'modal-content-container');
    return contentDiv;
}


/**
 * Opens a modal.
 * @param {String} title The title of the modal.
 * @param {jQuery} modalDiv The div that is opened as a modal.
 */
function openModal(title, modalDiv) {
    'use strict';

    var context = $('#courseography-header').attr('context');

    if ($('.modal-ui').length === 0) {
        modalDiv.attr('title', title)
                .addClass('modal-ui').dialog({
                    autoOpen: true,
                    modal: true,
                    width: 750,
                    height: 400,
                    closeText: 'X',
                    open: function(event, ui) {
                        $('.ui-widget-overlay').bind('click', function () {
                            modalDiv.dialog('close');
                        }); },
                    close: function () {
                        $(this).remove();
                        if (context === 'graph') {
                            lightUpGraph();
                        }
                    }});

        if (context === 'graph') {
            dimGraph();
        }
    }
}


/**
 * Lights up the graph.
 */
function lightUpGraph() {
    'use strict';

    $('body').css('background', 'rgb(255,255,255)');
}


/**
 * Dims the graph.
 */
function dimGraph() {
    'use strict';

    $('body').css('background', 'rgb(40,40,40)');

    $('.tooltip-group').remove();
}
