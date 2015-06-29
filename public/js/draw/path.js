/* CREATE PATHS */

/** 
 * Make the dummy elbow node with center at position position.
 * @position {object} position The x and y position of the dummy node being created.
 * @returns
 */
function makeElbow(position) {
    'use strict';

    var elbow = document.createElementNS(xmlns, 'circle');

    elbow.setAttributeNS(null, 'cx', position.x);
    elbow.setAttributeNS(null, 'cy', position.y);
    elbow.setAttributeNS(null, 'r', 4);
    elbow.setAttributeNS(null, 'class', 'elbow');

    elbow.addEventListener('mousedown', selectElbow, false);
    svgDoc.appendChild(elbow);

    curPath.elbows.push(elbow);

    return elbow;
}


/**
 * Creates an SVG path that has the coordinates specified by pathString.
 * @param {string} pathString The coordinates of the new path to be created.
 * @param {string} type The type of the path (could also be a region).
 */
function startPath(pathString, type) {
    'use strict';

    // note: id will get set when the path is complete, also marker (arrowhead)
    // curPath will get modified until path is complete
    curPath = document.createElementNS(xmlns, 'path'); 
    curPath.setAttributeNS(null, 'd', pathString); 
    curPath.setAttributeNS(null, 'stroke', 'black');
    curPath.setAttributeNS(null, 'class', 'path');
    curPath.elbows = [];
    if (type === 'path') {
        curPath.setAttributeNS(null, 'data-active', 'drawn');
       svgDoc.appendChild(curPath);
    } else {
        curPath.elbows.push(startPoint);
        document.getElementById('regions').appendChild(curPath);
    }
}


/**
 * Creates a new path or completes curPath by ending it at endNode and giving it
 * the id pathID.
 * @param{string} pathId The id of the now complete path.
 * @param{SVGElement} endNode The last coordinate for the path.
 */
function finishPath(pathId, endNode) {
    'use strict';

    var pathString;

    if (curPath === null) { // create a new path, node to node
        pathString = findClosest( {x: parseFloat(startNode.getAttribute('x'), 10),
                                       y: parseFloat(startNode.getAttribute('y'), 10)},
                                       'node', 
                                      {x: parseFloat(endNode.getAttribute('x'), 10),
                                       y: parseFloat(endNode.getAttribute('y'), 10)},
                                       'node');

        startPath(pathString, 'path');
    } else { // finish curPath, elbow to node
        var curElbow = {x: parseFloat(curPath.elbows[curPath.elbows.length - 1].getAttribute('cx'), 10),
                        y: parseFloat(curPath.elbows[curPath.elbows.length - 1].getAttribute('cy'), 10)};
        pathString = findClosest(curElbow, 'elbow',
                                     {x: parseFloat(endNode.getAttribute('x'), 10),
                                      y: parseFloat(endNode.getAttribute('y'), 10)},
                                     'node'); 
        curPath.setAttributeNS(null, 'd', curPath.getAttribute('d') + pathString);
    }

    select(endNode);
    curPath.elbows.map(function (item) {
        item.path = pathId; 
    });
    curPath.setAttributeNS(null, 'id', pathId);
    curPath.setAttributeNS(null, 'class', 'path');
    curPath.setAttributeNS(null, 'marker-end', 'url(#arrow)');
    curPath.addEventListener('click', pathClicked, false);

    // update relationships
    startNode.kids.push(endNode);
    endNode.parents.push(startNode);
    startNode.outEdges.push(curPath);
    endNode.inEdges.push(curPath);   
}


/**
 * Return the best coordinates for the start and end of a path. 
 * Pre-Requisites: At least one of source and target must be a node.
 * @param {SVGElement} source A node or an elbow element.
 * @param {string} sourceType The type of source, elbow or node.
 * @param {SVGElement} target A node or an elbow element.
 * @param {string} targetType The type of target, elbow or node.
 */
function findClosest(source, sourceType, target, targetType) {
    'use strict';

    var thePath = null;
    var node1Edges;
    var node2Edges;

    if (sourceType === 'node' && targetType === 'elbow') {
        node1Edges = [{x: source.x + nodeWidth/2, y: source.y},
                      {x: source.x + nodeWidth/2, y: source.y + nodeHeight},
                      {x: source.x + nodeWidth, y: source.y + nodeHeight/2},
                      {x: source.x, y: source.y + nodeHeight/2}];
        node2Edges = [target];
    } else if (sourceType === 'elbow' && targetType === 'node') {
        node1Edges = [source];
        node2Edges = [{x: target.x + nodeWidth/2, y: target.y},
                      {x: target.x + nodeWidth/2, y: target.y + nodeHeight},
                      {x: target.x + nodeWidth, y: target.y + nodeHeight/2},
                      {x: target.x, y: target.y + nodeHeight/2}];
    } else {
        // top, bottom, left, right
        node1Edges = [{x: source.x + nodeWidth/2, y: source.y},
                      {x: source.x + nodeWidth/2, y: source.y + nodeHeight},
                      {x: source.x + nodeWidth, y: source.y + nodeHeight/2},
                      {x: source.x, y: source.y + nodeHeight/2}];
        node2Edges = [{x: target.x + nodeWidth/2, y: target.y},
                      {x: target.x + nodeWidth/2, y: target.y + nodeHeight},
                      {x: target.x + nodeWidth, y: target.y + nodeHeight/2},
                      {x: target.x, y: target.y + nodeHeight/2}];
    }

    // find best alignment
    var best_edges = [node1Edges[0], node2Edges[0]];
    var best_dist = dist(node1Edges[0], node2Edges[0]);
    for (var i = 0; i < node1Edges.length; i++) {
        for (var j = 0; j < node2Edges.length; j++) {
            if (dist(node1Edges[i], node2Edges[j]) < best_dist) {
                best_edges = [node1Edges[i], node2Edges[j]];
                best_dist = dist(node1Edges[i], node2Edges[j]);
            } 
        }
    }

    if (sourceType === 'elbow' && targetType === 'node') {
        // only need to add end point to curPath
        thePath = 'L' + best_edges[1].x + ',' + best_edges[1].y + ' ';
    } else {
        thePath = 'M' + best_edges[0].x + ',' + best_edges[0].y + ' L' +
                    best_edges[1].x + ',' + best_edges[1].y + ' ';
    }

    return thePath;
}

/**
 * Returns the distance between point a and point b.
 * @param {object} a The coordinates of point a.
 * @param {object} b The coordinates of point b.
 * @return {number} The distance between a and b.
 */
function dist(a, b) {
    'use strict';

    return Math.sqrt(((b.x - a.x) * (b.x - a.x)) + ((b.y - a.y) * (b.y - a.y)));
}


/* MODIFY PATHS */
/**
 * In change mode, moves target of event e (the elbow) . In erase-mode, erases elbow. 
 * @param {object} e The mousedown event.
 */
function selectElbow(e) {
    'use strict';

    if (mode === 'change-mode') {
        var position = getClickPosition(e, e.currentTarget);
        elbowMoving = e.currentTarget;
        prevX = position.x;
        prevY = position.y;
    } else if (mode === 'erase-mode') {
        var indexOfElbow = 0;
        var indexOfNext;
        var thePath = document.getElementById(e.currentTarget.path);
        var thePathString = thePath.getAttribute('d');
        var elbowNum = thePath.elbows.indexOf(e.currentTarget);

        if (thePath.getAttribute('class') === 'region' && thePath.elbows.length <= 3) {
            // remove the whole region, can't have only 2 points in shape
            thePath.elbows.map(function (item) {
                svgDoc.removeChild(item);
            });
            document.getElementById('regions').removeChild(thePath);
        } else {
            if (thePath.getAttribute('class') === 'region') {
                elbowNum -= 1;
            }

            // look for elbowNum-th occurance of L
            for (var i = 0; i <= elbowNum; i++) {
                indexOfElbow = thePathString.indexOf('L', indexOfElbow + 1);
            }
            indexOfNext = thePathString.indexOf('L', indexOfElbow + 1);
            thePathString = thePathString.slice(0, indexOfElbow) + thePathString.slice(indexOfNext);
            if (thePathString[0] !== 'M') { 
                // for the case when the first elbow of a region
                thePathString = 'M' + thePathString.slice(1);
            }
            thePath.elbows.splice(thePath.elbows.indexOf(e.currentTarget), 1);
            thePath.setAttributeNS(null, 'd', thePathString);
            svgDoc.removeChild(e.currentTarget);
        } 
    }
}


/**
 * Move the partOfPath of the path by xBy and yBy units.  // !! FIX DESCRIPTION
 * @param {SVGElement} path The path being modified.
 * @param {number} xBy The amount of movement in the x direction.
 * @param {number} yBy The amount of movement in the y direction.
 * @param {string} partOfPath The part of the path (start, end or elbow) path being moved.
 * @param {number} elbowNum The number of the elbow being moved if partOfPath is elbow.
 */
function movePath(path, xBy, yBy, partOfPath, elbowNum) {
    'use strict';

    var thePath = path.getAttribute('d');
    var theX = null;
    var theY = null;
    if (partOfPath === 'start') {
        theX = parseFloat(thePath.slice(1, thePath.indexOf(',')), 10) + xBy; // exclude the M
        theY = parseFloat(thePath.slice(thePath.indexOf(',') + 1, 
                                        thePath.indexOf('L')), 10) + yBy;
        thePath = 'M' + theX + ',' + theY + thePath.slice(thePath.indexOf('L'));
    } else if (partOfPath === 'end') {
        theX = parseFloat(thePath.slice(thePath.lastIndexOf('L') + 1, 
                                        thePath.lastIndexOf(',')), 10) + xBy;
        theY = parseFloat(thePath.slice(thePath.lastIndexOf(',') + 1), 10) + yBy;
        thePath = thePath.slice(0, thePath.lastIndexOf('L') + 1) + theX + ',' + theY;
    } else if (partOfPath === 'elbow') {
        var indexOfElbow = 0; // !! elbowNum is not valid 
        if (thePath[thePath.length-1] === 'Z') {
            elbowNum -= 1; // first elbow starts with M not L
        }
        var indexOfNext;
        // look for elbowNum-th occurance of L
        for (var i = 0; i <= elbowNum; i++) {
            indexOfElbow = thePath.indexOf('L', indexOfElbow + 1);
        }
        indexOfNext = thePath.indexOf('L', indexOfElbow + 1);
        theX = parseFloat(thePath.slice(indexOfElbow + 1, indexOfNext), 10) + xBy;
        theY = parseFloat(thePath.slice(thePath.indexOf(',' , indexOfElbow) + 1,
                                        indexOfNext), 10) + yBy;
        thePath = thePath.slice(0, indexOfElbow + 1) + theX + ',' + theY + thePath.slice(indexOfNext - 1);
    }

    path.setAttribute('d', thePath); 
}


/**
 * In erase mode, delete the target of event e (a path).
 * @param {object} e The mousedown event.
 */
function pathClicked(e) {
    'use strict';

    if (mode === 'erase-mode') {
        erasePath(e.currentTarget);
    }

}


/**
 * Delete the path path.
 * @param {SVGElement} path 
 */
function erasePath(path) {
        var index = -1;
        var pathId = path.id;
        var beg = document.getElementById(pathId.slice(0, pathId.lastIndexOf('n')));
        var end = document.getElementById(pathId.slice(pathId.lastIndexOf('n')));
        
        // delete the nodes from each others' list
        index = beg.kids.indexOf(end);
        if (index > -1) {
            beg.kids.splice(index, 1);
        }
        index = end.parents.indexOf(beg);
        if (index > -1) {
            end.parents.splice(index, 1);
        }

        // delete this path from the nodes' list
        index = beg.outEdges.indexOf(path);
        if (index > -1) {
            beg.outEdges.splice(index, 1);
        }
        index = end.inEdges.indexOf(path);
        if (index > -1) {
            end.inEdges.splice(index, 1);
        }

        // delete the dummy nodes and the path itself
        path.elbows.map(function (item) {
            svgDoc.removeChild(item);
        });
        svgDoc.removeChild(path);
}
