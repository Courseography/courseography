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

    $('.path').each(function () {
        makeEdge(window[$(this).attr('source-node')], window[$(this).attr('target-node')], $(this).attr('id'));
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