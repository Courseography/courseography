/**
 * Displays a tooltip for a Node.
 * @param {string} nodeId The Node's ID.
 */
function displayTooltip(nodeId) {
    'use strict';

    var rectObject = $('#' + nodeId).find('rect');
    var xPos, yPos;
    // The tooltip displays with a width of 222. If the node has an x
    // position of less than 222, the tooltip will be cut off by the svg
    // boundaries. In this case, we display the tooltip on the left.
    var rightSide = rectObject.attr('x') > 222;

    // The tooltip is offset with a 'padding' of 5.
    if (rightSide) {
        xPos = parseFloat(rectObject.attr('x')) - 65;
    } else {
        xPos = parseFloat(rectObject.attr('x')) +
            parseFloat($('#' + nodeId).children('rect').attr('width')) + 5;
    }

    yPos = parseFloat(rectObject.attr('y'));
    createInfoBox(nodeId, xPos, yPos);
}


/**
 * Creates an info box.
 * @param nodeId
 * @param xPos
 * @param yPos
 */
function createInfoBox(nodeId, xPos, yPos) {
    'use strict';

    var g = createG(nodeId);
    createRect(g, 'node-tooltip', nodeId + '-tooltip', xPos, yPos,
        60, 30, 'black');
    createText(g, nodeId, 'node-tooltip', nodeId + '-tooltip', xPos, yPos,
        60, 30);
}


/**
 * Creates an svg rect object and appends it to #graphRootSVG.
 * @param {jQuery} g The g element of the rect.
 * @param {string} rectClass Class(es) of the rect.
 * @param {string} rectId The ID of the rect.
 * @param {number} posX The x position of the rect.
 * @param {number} posY The y position of the rect.
 * @param {number} width The width of the rect.
 * @param {number} height The height of the rect.
 * @param {string} color The fill and stroke color of the rect.
 */
function createRect(g, rectClass, rectId, posX, posY, width, height, color) {
    'use strict';

    var rect = $(document.createElementNS('http://www.w3.org/2000/svg', 'rect'))
        .attr('class', rectClass + '-rect ' + rectId + '-rect')
        .attr('id', rectId + '-rect')
        .attr('x', posX)
        .attr('y', posY)
        .attr('rx', 10)
        .attr('ry', 10)
        .attr('fill', 'white')
        .attr('stroke', color)
        .attr('stroke-width', 2)
        .attr('width', width)
        .attr('height', height);

    g.append(rect);

    $('.tooltip-group').hover(
        function () {
            clearAllTimeouts();
        },
        function () {
            $('.tooltip-group').fadeOut(400, function () {$(this).remove(); });
        });
}


/**
 * Creates an svg text element and appends it to #graphRootSVG.
 * @param {jQuery} g The g element of the text element.
 * @param {string} nodeId The Node's ID.
 * @param {string} textClass Class(es) of the text element.
 * @param {string} textId The ID of the text element.
 * @param {number} posX The x position of the text element.
 * @param {number} posY The y position of the text element.
 * @param {number} width The width of the text element.
 * @param {number} height The height of the text element.
 */
function createText(g, nodeId, textClass, textId, posX, posY, width, height) {
    'use strict';

    var text = $(document.createElementNS('http://www.w3.org/2000/svg', 'text'))
        .text('Info')
        .attr('class', textClass + '-text ' + textId + '-text')
        .attr('id', textId + '-text')
        .attr('x', parseFloat(posX) + width / 2 - 18)
        .attr('y', parseFloat(posY) + height / 2 + 6);
    g.append(text);
}


/**
 * Sets up and Returns a tooltips SVG g element.
 * @param {string} nodeId
 * @returns {jQuery}
 */
function createG(nodeId) {
    'use strict';

    var g = $(document.createElementNS('http://www.w3.org/2000/svg', 'g'));

    g.attr('class', 'tooltip-group')
        .css('cursor', 'pointer')
        .click(function () {
            var id = nodeId.substring(0, 6);
            openModal(getCourseTitle(id), createModalDiv(id));
            //openReactModal doesnt "open" a modal yet, this just overwrites the contents from openModal
            openReactModal(id);
        });
    $('svg').append(g);

    return g;
}
