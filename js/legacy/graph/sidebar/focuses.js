'use strict';

import $ from 'jquery';

/**
 * Updates the active focus.
 * @param {string} id The ID of the active focus.
 */
export function updateActiveFocus(id) {
    'use strict';

    var graphObject = $('#graph');

    $('ellipse.spotlight').remove();

    if (id === '') {
        clearFocus();
    } else {
        var focus = global[id + 'FocusList'];
        $('body').css('background', 'rgb(40,40,40)');
        $('.node, .hybrid').attr('data-active', 'unlit');
        $.each(focus, function (index, elem) {
            spotlight(elem);
        });
        graphObject.html(graphObject.html()); // Hack to make spotlights appear
        activeFocus = id;
    }
}


/**
 * Removes active focus spotlights.
 */
export function clearFocus() {
    'use strict';

    $('body').css('background', 'white');
    activeFocus = '';
    $.each(nodes, function (index, elem) {
        global.nodeMap[elem].updateSVG();
    });
}


/**
 * Highlights a Node with a 'spotlight'.
 * @param {string} id The ID of the highlighted Node.
 */
function spotlight(id) {
    'use strict';

    var nodeObject = $('#' + id.toLowerCase());

    var node = $('#' + id + ' > rect');
    var width = parseFloat(node.attr('width')) / 2;
    var height = parseFloat(node.attr('height')) / 2;
    var x = parseFloat(node.attr('x')) + width;
    var y = parseFloat(node.attr('y')) + height;

    var ellipse = '<ellipse class="spotlight" cx="'.concat(String(x),
                                                           '" cy = "',
                                                           String(y),
                                                           '" rx="',
                                                           String(width + 9),
                                                           '" ry="',
                                                           String(height + 8.5),
                                                           '"/>');

    nodeObject.before(ellipse);
    nodeObject.attr('data-active', 'lit');
}
