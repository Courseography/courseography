/**
 * Javascript code to generate the logical data structures
 * (Node & Edge) to represent prerequisites, given just the SVG graph.
 */


/**
 * Generates Node and Edge objects based on geometric relationships.
 * TODO: This function is too long.
 */
function buildGraph() {
    'use strict';

    var xStart;
    var yStart;
    var yEnd;
    var xEnd;

    $('.node').each(function () {
        makeNode('AND', $(this).attr('id'));
    });

    $('.hybrid').each(function () {
        var id = $(this).attr('id');
        var course = $(this).children('text').text();
        var reqs = parseAnd(course)[0];
        makeHybrid('AND', id);
        $.each(reqs, function (index, elem) {
            if ($.isArray(elem)) {
                var orNode = id + elem.join();
                makeHybrid('OR', orNode);
                $.each(elem, function (i, e) {
                    window[orNode].parents.push(window[e]);
                    window[e].children.push(window[orNode]);
                });
                window[id].parents.push(window[orNode]);
                window[orNode].children.push(window[id]);
            } else {
                window[id].parents.push(window[elem]);
                window[elem].children.push(window[id]);
            }
        });
    });

    $('.bool').each(function () {
        var id = $(this).attr('id');
        var type = $(this).children('text').text().toUpperCase();
        makeHybrid(type, id);
    });

    $('path').each(function () {
        var coords = $(this).attr('d').split(' ');
        coords = coords.filter(function (str) {return str !== 'M' &&
            str !== 'L' && str !== ''; });

        // Do something for internet explorer
        xStart = parseFloat(coords[0].split(",")[0]);
        yStart = parseFloat(coords[0].split(",")[1]);
        var end = coords.pop();
        xEnd = parseFloat(end.split(",")[0]);
        yEnd = parseFloat(end.split(",")[1]);

        var startNode = '';
        var endNode = '';

        $('.node, .hybrid').each(function () {
            // Check intersection
            var r = $(this).children('rect');
            var xRect = parseFloat(r.attr('x'));
            var yRect = parseFloat(r.attr('y'));
            var width = parseFloat(r.attr('width'));
            var height = parseFloat(r.attr('height'));

            if (intersects(xStart, yStart, xRect, yRect, width, height, 10)) {
                startNode = $(this).attr('id');
            }

            if (intersects(xEnd, yEnd, xRect, yRect, width, height, 20)) {
                endNode = $(this).attr('id');
            }

            if (startNode !== '' && endNode !== '') {
                return false;
            }
        });

        $('.bool').each(function () {
            // Check intersection
            var el = $(this).children('ellipse');
            var cx = parseFloat(el.attr('cx'));
            var cy = parseFloat(el.attr('cy'));
            var rx = parseFloat(el.attr('rx'));
            var ry = parseFloat(el.attr('ry'));

            if (Math.abs(xStart - cx) <= rx + 1 && Math.abs(yStart - cy) <= ry + 1) {
                startNode = $(this).attr('id');
            }

            if (Math.abs(xEnd - cx) <= rx + 9 && Math.abs(yEnd - cy) <= ry + 9) {
                endNode = $(this).attr('id');
            }

            if (startNode !== '' && endNode !== '') {
                return false;
            }
        });

        if (startNode === '' && endNode === '') {
            console.log("Could not make edge, diagnose:");
            console.log("Start: " + startNode);
            console.log("End: " + endNode);
        }

        makeEdge(window[startNode], window[endNode], $(this).attr('id'));
    });
}


/**
 *
 * @param {string} s
 * @returns {Array}
 */
function parseAnd(s) {
    'use strict';

    var curr = s;
    var andList = [];
    while (curr.length > 0) {
        if (curr.charAt(0) === ',' ||
            curr.charAt(0) === ';' ||
            curr.charAt(0) === ' ') {
            curr = curr.substr(1);
        } else {
            var result = parseOr(curr);
            if (curr === result[1]) {
                console.log('Parsing failed for ' + s + '  with curr = ' + curr);
                break;
            } else {
                curr = result[1];
                andList.push(result[0]);
            }
        }
    }
    return [andList, curr];
}


/**
 *
 * @param {string} s
 * @returns {Array}
 */
function parseOr(s) {
    'use strict';

    var curr = s;
    var orList = [];
    var tmp;
    var result;
    while (curr.length > 0 &&
        curr.charAt(0) !== ',' &&
        curr.charAt(0) !== ';') {

        if (curr.charAt(0) === '(') {
            tmp = curr.substr(1, curr.indexOf(')'));
            result = parseCourse(tmp);
            orList.append(result[0]);
            curr = curr.substr(curr.indexOf(')') + 1);
        } else if (curr.charAt(0) === ' ' ||
            curr.charAt(0) === '/') {
            curr = curr.substr(1);
        } else {
            result = parseCourse(curr);
            if (curr === result[1]) {
                console.log('Parsing failed for ' + s + ' with curr = ' + curr);
                break;
            }
            curr = result[1];
            orList.push(result[0]);
        }
    }

    if (orList.length === 1) {
        orList = orList[0];
    }

    return [orList, curr];
}


/**
 *
 * @param {string} s
 * @returns {Array}
 */
function parseCourse(s) {
    'use strict';

    var start = s.search(/[,/]/);

    if (start === 3) {
        return ['CSC' + s.substr(0, start), s.substr(start)];
    } else if (start > 0) {
        return [s.substr(0, start), s.substr(start)];
    }

    if (s.length === 3) {
        return ['CSC' + s, ''];
    }

    return [s, ''];
}


/**
 * Returns whether point (px,py) intersects with
 * the rectangle whose top left corner is at (rx,ry) with width
 * width, height height and an offset of offset.
 * @param {number} px The point's x position.
 * @param {number} py The point's y position.
 * @param {number} rx The rectangle's x position.
 * @param {number} ry The rectangle's y position.
 * @param {number} width The rectangle's width.
 * @param {number} height The rectangle's height.
 * @param {number} offset The offset.
 * @returns {boolean} Whether the point intersects with the rectangle + offset.
 */
function intersects(px, py, rx, ry, width, height, offset) {
    'use strict';

    var dx = px - rx;
    var dy = py - ry;
    return dx >= -1 * offset &&
        dx <= width + offset &&
        dy >= -1 * offset &&
        dy <= height + offset;
}