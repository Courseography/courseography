/**
 * Javascript code to generate the logical data structures
 * (Node & Edge) to represent prerequisites, given just the SVG graph.
 */

/**
 * Generates Node and Edge objects based on geometric relationships.
 * TODO: This function is too long.
 */
function buildGraph(gId) {
    'use strict';

    // Hard-coded hybrid relationships.
    // TODO: This should be automatically generated.
    if (gId === '1') { // CSC graph
        h91.parents.push(mat135136137157calc1);
        mat135136137157calc1.children.push(h91);
        h133csc165calc1.parents.push(csc165240);
        csc165240.children.push(h133csc165calc1);
        h133csc165calc1.parents.push(mat135136137157calc1); // Auto-generated OR node
        mat135136137157calc1.children.push(h133csc165calc1);
        h101.parents.push(mat221223240lin1);
        mat221223240lin1.children.push(h101);
        h99.parents.push(sta247255257sta1);
        sta247255257sta1.children.push(h99);
        h142.parents.push(sta247255257sta1);
        sta247255257sta1.children.push(h142);
        h103.parents.push(sta247255257sta1);
        sta247255257sta1.children.push(h103);
        h89.parents.push(csc263265);
        csc263265.children.push(h89);
        h121.parents.push(csc263265);
        csc263265.children.push(h121);
        h131.parents.push(csc263265);
        csc263265.children.push(h131);
        h123.parents.push(csc236240);
        csc236240.children.push(h123);
    } else if (gId === '2') { // STA graph
        // Because of naming conventions, we re-write the prereqs manually
        h211.logicalType = 'OR';
        h211.parents = [sta247, sta255, sta257];
        sta247.children.push(h211);
        sta255.children.push(h211);
        sta257.children.push(h211);

        h206.logicalType = 'AND';
        h206.parents = [mat235237257];
        mat235237257.children.push(h206);

        h199.logicalType = 'AND';
        h199.parents = [csc108120148];
        csc108120148.children.push(h199);
    }
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
        return ['csc' + s.substr(0, start), s.substr(start)];
    } else if (start > 0) {
        return [s.substr(0, start), s.substr(start)];
    }

    if (s.length === 3) {
        return ['csc' + s, ''];
    }

    return [s, ''];
}
