/**
 *
 * @param id
 */
function updateActiveFocus(id) {
    'use strict';

    var graphObject = $('#graph');

    $('ellipse.spotlight').remove();
    // Remove old icon
    $('.focusList .statusIcon').remove();

    if (id === '') {
        clearFocus();
    } else {
        var focus = window[id + 'FocusList'];
        $('body').css('background', 'rgb(40,40,40)');
        $('.node, .hybrid').attr('data-active', 'unlit');
        $.each(focus, function (index, elem) {
            spotlight(elem);
        });
        graphObject.html(graphObject.html()); // Hack to make spotlights appear
        setMouseCallbacks();
        activeFocus = id;

        // Put in new icon.
        $('.focusList a[href="#' + id + 'Details"]').append(
            "<img class='statusIcon' src='res/ico/close.ico' alt='Click to close!'/>"
        );
    }
}


// Removes spotlight on active focus
/**
 *
 */
function clearFocus() {
    'use strict';

    $('body').css('background', 'white');
    activeFocus = '';
    $.each(nodes, function (index, elem) {
        window[elem].updateSVG();
    });
}


// Put a spotlight on a node
/**
 *
 * @param id
 */
function spotlight(id) {
    'use strict';
    var nodeObject = $('#' + id);

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

    window[id].updateSVG();
}