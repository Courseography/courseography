/* CREATE PATHS */
function makeElbow(position) {
    'use-strict';

    var elbow = document.createElementNS(xmlns, 'circle');

    elbow.setAttributeNS(null, 'cx', position.x);
    elbow.setAttributeNS(null, 'cy', position.y);
    elbow.setAttributeNS(null, 'r', 4);
    elbow.setAttributeNS(null, 'class', 'elbow');

    elbow.addEventListener('mousedown', selectElbow, false);
    svgDoc.appendChild(elbow);

    curPath.elbows.push(elbow);
}


function startPath(pathString) {
    'use-strict';

    curPath = document.createElementNS(xmlns, 'path'); // note: id will get set when the path is complete
    curPath.setAttributeNS(null, 'd', pathString); // curPath will get modified until path is complete
    curPath.setAttributeNS(null, 'fill', 'none');
    curPath.setAttributeNS(null, 'stroke', 'black');
    curPath.setAttributeNS(null, 'data-active', 'drawn'); // also marker will be set at completion
    // thePath.setAttributeNS(null, 'marker-end', 'url(#arrow)');
    curPath.elbows = [];
    curPath.addEventListener('click', pathClicked, false);
    svgDoc.appendChild(curPath);
}


function finishPath(pathId, endNode) {
    'use-strict';

    if (curPath === null) { // create a new path, node to node
        var pathString = findClosest( {x: parseFloat(startNode.getAttribute('x'), 10), 
                                       y: parseFloat(startNode.getAttribute('y'), 10)},
                                       'node', 
                                      {x: parseFloat(endNode.getAttribute('x'), 10), 
                                       y: parseFloat(endNode.getAttribute('y'), 10)},
                                       'node');

        startPath(pathString);
    } else { // finish curPath, elbow to node
        var curElbow = {x: parseFloat(curPath.elbows[curPath.elbows.length - 1].getAttribute('cx'), 10), 
                        y: parseFloat(curPath.elbows[curPath.elbows.length - 1].getAttribute('cy'), 10)}
        var pathString = findClosest(curElbow, 'elbow', 
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
    curPath.setAttributeNS(null, 'marker-end', 'url(#arrow)');

    // update relationships
    startNode.kids.push(endNode);
    endNode.parents.push(startNode);
    startNode.outEdges.push(curPath);
    endNode.inEdges.push(curPath);   
}


/*
Return the best coordinates for the start and end of a edge. theNode and end could
be a node or an elbow. At least one of beg and end must be a node.
*/
function findClosest(beg, typeB, end, typeE) {
    'use-strict';

    var thePath = null;
    var node1Edges;
    var node2Edges 

    if (typeB === 'node' && typeE === 'elbow') {
        node1Edges = [{x: beg.x + nodeWidth/2, y: beg.y}, 
                      {x: beg.x + nodeWidth/2, y: beg.y + nodeHeight}, 
                      {x: beg.x + nodeWidth, y: beg.y + nodeHeight/2},
                      {x: beg.x, y: beg.y + nodeHeight/2}];
        node2Edges = [end];
    } else if (typeB === 'elbow' && typeE === 'node') {
        node1Edges = [beg];
        node2Edges = [{x: end.x + nodeWidth/2, y: end.y}, 
                      {x: end.x + nodeWidth/2, y: end.y + nodeHeight}, 
                      {x: end.x + nodeWidth, y: end.y + nodeHeight/2},
                      {x: end.x, y: end.y + nodeHeight/2}];
    } else {
        // top, bottom, left, right
        node1Edges = [{x: beg.x + nodeWidth/2, y: beg.y}, 
                      {x: beg.x + nodeWidth/2, y: beg.y + nodeHeight}, 
                      {x: beg.x + nodeWidth, y: beg.y + nodeHeight/2},
                      {x: beg.x, y: beg.y + nodeHeight/2}];
        node2Edges = [{x: end.x + nodeWidth/2, y: end.y}, 
                      {x: end.x + nodeWidth/2, y: end.y + nodeHeight}, 
                      {x: end.x + nodeWidth, y: end.y + nodeHeight/2},
                      {x: end.x, y: end.y + nodeHeight/2}];
    }

    // find best alignment
    var best_edges = [node1Edges[0], node2Edges[0]];
    var best_dist = dist(node1Edges[0], node2Edges[0]);
    console.log(best_dist);
    for (var i = 0; i < node1Edges.length; i++) {
        for (var j = 0; j < node2Edges.length; j++) {
            if (dist(node1Edges[i], node2Edges[j]) < best_dist) {
                best_edges = [node1Edges[i], node2Edges[j]];
                best_dist = dist(node1Edges[i], node2Edges[j]);
                console.log(best_dist);
            } 
        }
    }

    if (typeB === 'elbow' && typeE === 'node') {
        // only need to add end point to curPath
        thePath = 'L' + best_edges[1].x + ',' + best_edges[1].y + ' ';
    } else {
        thePath = 'M' + best_edges[0].x + ',' + best_edges[0].y + ' L' +
                    best_edges[1].x + ',' + best_edges[1].y + ' ';
    }

    return thePath;
}


function dist(a, b) {
    'use-strict';

    return Math.sqrt(((b.x - a.x) * (b.x - a.x)) + ((b.y - a.y) * (b.y - a.y)));
}


/* MODIFY PATHS */
function selectElbow(e) {
    'use-strict';

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

        // look for elbowNum-th occurance of L
        for (var i = 0; i <= elbowNum; i++) {
            indexOfElbow = thePathString.indexOf('L', indexOfElbow + 1);
        }
        indexOfNext = thePathString.indexOf('L', indexOfElbow + 1);
        thePathString = thePathString.slice(0, indexOfElbow) + thePathString.slice(indexOfNext);
        thePath.elbows.splice(thePath.elbows.indexOf(e.currentTarget), 1);
        thePath.setAttributeNS(null, 'd', thePathString);
        svgDoc.removeChild(e.currentTarget);
    }
}


function movePath(path, xBy, yBy, partOfPath, elbowNum) {
    'use-strict';

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


function pathClicked(e) {
    'use-strict';

    if (mode === 'erase-mode') {
        erasePath(e.currentTarget);
    }

}


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
        path.elbows.map(function (item) {
            svgDoc.removeChild(item);
        });
        svgDoc.removeChild(path);
}