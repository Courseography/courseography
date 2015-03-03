var nodeId = 0;
var nodeWidth = 40;
var nodeHeight = 32;
var xmlns = 'http://www.w3.org/2000/svg';
var mode = 'node-mode';
var nodeColourId = 'red';
var nodeMoving = null;      // for movement and path creation
var prevX = -1;             // for movement
var prevY = -1;             // for movement
var nodeSelected = null;    // for adding text or changing colour
var startNode = null;       // for making paths
var curPath = null;         // the path currently being created
var elbowMoving = null;     // for movement of elbow joints

function setupSVGCanvas() {
    'use-strict';

    var div = document.createElement('div');
    div.setAttribute('id', 'main');
    var bgdiv = document.createElement('div');
    bgdiv.setAttribute('id', 'background');
    var svg = document.createElementNS(xmlns, 'svg');
    svg.setAttribute('id', 'mySVG');
    svg.setAttributeNS('http://www.w3.org/2000/xmlns/', 'xmlns:xlink', 'http://www.w3.org/1999/xlink');
    
    svg.addEventListener('mousedown', makeNode, false);
    svg.addEventListener('mousemove', move, false);
    svg.addEventListener('mouseup', unclickAll, false);

    svg.appendChild(setupMarker());
    div.appendChild(bgdiv);
    div.appendChild(svg);
    document.body.appendChild(div);
}


function setupMarker() {
    'use-strict';

    var defs = document.createElementNS(xmlns, 'defs');
    var marker = document.createElementNS(xmlns, 'marker');
    var polyline = document.createElementNS(xmlns, 'polyline');
    
    marker.setAttributeNS(null, 'id', 'arrow');
    marker.setAttributeNS(null, 'class', 'path');
    marker.setAttributeNS(null, 'viewBox', '0 0 10 10');
    marker.setAttributeNS(null, 'refX', '8');
    marker.setAttributeNS(null, 'refY', '5');
    marker.setAttributeNS(null, 'markerunits', 'strokeWidth');
    marker.setAttributeNS(null, 'orient', 'auto');
    marker.setAttributeNS(null, 'markerWidth', '4.5');
    marker.setAttributeNS(null, 'markerHeight', '4.5');

    polyline.setAttributeNS(null, 'points', '0,1 10,5 0,9');
    polyline.setAttributeNS(null, 'fill', 'black');

    marker.appendChild(polyline);
    defs.appendChild(marker);

    return defs;
}


function getClickPosition(e, elem) {
    'use-strict';

    console.log(e.clientX, e.clientY);
    var parentPosition = getPosition(elem);
    var xPosition = e.clientX - parentPosition.x;
    var yPosition = e.clientY - parentPosition.y;
    // xPosition = Math.round(xPosition / 5) * 5; // for snapping!!
    // yPosition = Math.round(yPosition / 5) * 5; // note: problem nodeHeight

    return { x: xPosition, y: yPosition };
}


function getPosition(elem) {
    'use-strict';

    var xPosition = 0;
    var yPosition = 0;
      
    while (elem) {
        // || 0 -> for mozilla firefox compatability !!
        xPosition += (elem.offsetLeft || 0) - elem.scrollLeft + elem.clientLeft;
        yPosition += (elem.offsetTop || 0) - elem.scrollTop + elem.clientTop;
        console.log(elem.offsetLeft, elem.scrollLeft, elem.clientLeft);
        console.log(elem.offsetTop, elem.scrollTop, elem.clientTop);
        console.log(elem.offsetLeft, elem.offsetTop);
        console.log(elem);
        console.log(elem.offsetParent);
        elem = elem.offsetParent;
    }
    return { x: xPosition, y: yPosition };
}


function makeNode(e) {
    'use-strict';

    var position = getClickPosition(e, e.currentTarget);
    if (mode === 'node-mode') {
        var g = document.createElementNS(xmlns, 'g');
        var node = document.createElementNS(xmlns, 'rect');

        g.setAttribute('class', 'node');
        g.setAttribute('id', 'g' + nodeId);
        g.setAttribute('data-active', 'active');
        g.setAttribute('data-group', nodeColourId);
    
        node.setAttribute('x', position.x);
        node.setAttribute('y', position.y);
        node.setAttribute('rx', 4);
        node.setAttribute('ry', 4);
        node.setAttribute('id', 'n' + nodeId);
        node.setAttribute('width', nodeWidth);
        node.setAttribute('height', nodeHeight);
        node.setAttribute('class', 'node');
        node.parents = [];
        node.kids = []; 
        // note: children doesn't work because javascript objects already have a children attribute
        node.inEdges = [];
        node.outEdges = [];
        
        g.appendChild(node);
        document.getElementById('mySVG').appendChild(g);
        document.getElementById('n' + nodeId).addEventListener('mousedown', nodeClicked, false);

        select(document.getElementById('n' + nodeId));

        nodeId += 1;
    } else if (mode === 'path-mode') {
        // make elbow joint, only if the dummy point is outside the starting node
        if (startNode !== null && ((position.x < parseFloat(startNode.getAttribute('x'), 10)) || 
                                  (position.x > parseFloat(startNode.getAttribute('x'), 10) + nodeWidth) ||
                                  (position.y < parseFloat(startNode.getAttribute('y'), 10)) || 
                                  (position.y > parseFloat(startNode.getAttribute('y'), 10) + nodeHeight))) {           
            if (curPath === null) { // start node to first elbow
                var pathString = findClosest({x: parseFloat(startNode.getAttribute('x'), 10), 
                             y: parseFloat(startNode.getAttribute('y'), 10)},
                            'node', position, 'elbow');

                curPath = document.createElementNS(xmlns, 'path'); // note: id will get set when the path is complete
                curPath.setAttributeNS(null, 'd', pathString); // curPath will get modified until path is complete
                curPath.setAttributeNS(null, 'fill', 'none');
                curPath.setAttributeNS(null, 'stroke', 'black');
                curPath.setAttributeNS(null, 'data-active', 'drawn'); // also marker will be set at completion
                // thePath.setAttributeNS(null, 'marker-end', 'url(#arrow)');
                curPath.elbows = [];
                curPath.addEventListener('click', pathClicked, false);
                document.getElementById('mySVG').appendChild(curPath);

            } else { // elbow to elbow path
                curPath.setAttributeNS(null, 'd', curPath.getAttribute('d') + 'L' + position.x + ',' + position.y + ' ');   
            }
            var elbow = document.createElementNS(xmlns, 'circle');

            elbow.setAttributeNS(null, 'cx', position.x);
            elbow.setAttributeNS(null, 'cy', position.y);
            elbow.setAttributeNS(null, 'r', 4);
            elbow.setAttributeNS(null, 'class', 'elbow');

            elbow.addEventListener('mousedown', selectElbow, false);
            document.getElementById('mySVG').appendChild(elbow);

            curPath.elbows.push(elbow);
        }
    }
}


function nodeClicked(e) {
    'use-strict';

    var svgDoc = document.getElementById('mySVG');
    var index = null;

    if (mode  === 'erase-mode') { 
        // remove any paths leading to and from this node from the other node's 
        // list of paths and remove this node from the other nodes' adjacency lists
        e.currentTarget.inEdges.map(function (edge) { 
            // Remove edge from parent's outEdges and current node from parent's kids list
            var edgeParent = document.getElementById(edge.id.slice(0, edge.id.lastIndexOf('n')));
            index = edgeParent.outEdges.indexOf(edge);
            if (index > -1) {
                edgeParent.outEdges.splice(index, 1);
            }
            index = edgeParent.kids.indexOf(e.currentTarget);
            if (index > -1) {
                edgeParent.kids.splice(index, 1);
            }
            erasePath(edge);
        });
        e.currentTarget.outEdges.map(function (edge) {
            // Remove edge from children's inEdges and current node from child's parents list
            var edgeChild = document.getElementById(edge.id.slice(edge.id.lastIndexOf('n')));
            index = edgeChild.inEdges.indexOf(edge);
            if (index > -1) {
                edgeChild.inEdges.splice(index, 1);
            }
            index = edgeChild.parents.indexOf(e.currentTarget);
            if (index > -1) {
                edgeChild.parents.splice(index, 1);
            }
            erasePath(edge);
        });
        svgDoc.removeChild(e.currentTarget.parentNode);
    } else if (mode === 'change-mode') {
        var position = getClickPosition(e, e.currentTarget);
        nodeMoving = e.currentTarget;
        prevX = position.x;
        prevY = position.y;
        
        // show which node has been selected
        select(e.currentTarget);

    } else if (mode === 'path-mode') {
        if (startNode === null) {
            // this is the start node of the path about to be created, can't do that
            startNode = e.currentTarget;
            select(e.currentTarget);
        } else if (startNode === e.currentTarget) {
            if (curPath !== null) {
                curPath.elbows.map(function (item) { // modify last node in path
                        document.getElementById('mySVG').removeChild(item);
                });
                document.getElementById('mySVG').removeChild(curPath);
                curPath = null;
            }
        } else {
            // make the path from startNode to current node then make startNode Null
            var pathId = startNode.id + e.currentTarget.id;
            if (document.getElementById(pathId) === null) {
                if (curPath === null) { // create a new path
                    var pathString = findClosest( {x: parseFloat(startNode.getAttribute('x'), 10), 
                                  y: parseFloat(startNode.getAttribute('y'), 10)},
                                'node', 
                                {x: parseFloat(e.currentTarget.getAttribute('x'), 10), 
                                 y: parseFloat(e.currentTarget.getAttribute('y'), 10)},
                                'node');

                    curPath = document.createElementNS(xmlns, 'path');
                    curPath.setAttributeNS(null, 'd', pathString);
                    curPath.setAttributeNS(null, 'fill', 'none');
                    curPath.setAttributeNS(null, 'stroke', 'black');
                    curPath.setAttributeNS(null, 'data-active', 'drawn');
                    curPath.addEventListener('click', pathClicked, false);
                    curPath.elbows = [];
                    document.getElementById('mySVG').appendChild(curPath);
                } else {
                    var curElbow = {x: parseFloat(curPath.elbows[curPath.elbows.length - 1].getAttribute('cx'), 10), 
                                  y: parseFloat(curPath.elbows[curPath.elbows.length - 1].getAttribute('cy'), 10)}
                    var pathString = findClosest(curElbow, 'elbow', 
                                {x: parseFloat(e.currentTarget.getAttribute('x'), 10), 
                                 y: parseFloat(e.currentTarget.getAttribute('y'), 10)},
                                'node'); 
                    curPath.setAttributeNS(null, 'd', curPath.getAttribute('d') + pathString);
                }

                select(e.currentTarget);
                curPath.elbows.map(function (item) {
                    item.path = pathId; 
                });
                curPath.setAttributeNS(null, 'id', pathId);
                curPath.setAttributeNS(null, 'marker-end', 'url(#arrow)');

//                thePath.parents = startNode;
//                thePath.kids = e.currentTarget;

                // update relationships
                startNode.kids.push(e.currentTarget);
                e.currentTarget.parents.push(startNode);
                startNode.outEdges.push(curPath);
                e.currentTarget.inEdges.push(curPath);
                startNode = null;
                curPath = null;
            } else {
                // A path between these two nodes already exists so don't create it!
                startNode = null;
                if (curPath !== null) {
                    curPath.elbows.map(function (item) { // modify last node in path
                        document.getElementById('mySVG').removeChild(item);
                });
                document.getElementById('mySVG').removeChild(curPath);
                curPath = null;
                }
            }
        }
    }
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

    return Math.sqrt((b.x - a.x) * (b.x - a.x) + (b.y - a.y) * (b.y - a.y))
}


function select(newNode) {
    'use-strict';

    if (nodeSelected !== null) {
        nodeSelected.parentNode.setAttribute('data-active', 'unselected');
    }
    nodeSelected = newNode;
    nodeSelected.parentNode.setAttribute('data-active', 'active');
}


function move(e) {
    'use-strict';

    if (mode === 'change-mode') {
        if (nodeMoving !== null) {
            var position = getClickPosition(e, nodeMoving);
            var rectX = parseFloat(nodeMoving.getAttribute('x'), 10);
            var rectY = parseFloat(nodeMoving.getAttribute('y'), 10);
            rectX += (position.x - prevX);
            rectY += (position.y - prevY);
            nodeMoving.setAttribute('x', rectX);
            nodeMoving.setAttribute('y', rectY);

            if (nodeMoving.parentNode.childNodes.length > 1) {
                var textX = parseFloat(nodeMoving.parentNode.childNodes[1].getAttribute('x'), 10);
                var textY = parseFloat(nodeMoving.parentNode.childNodes[1].getAttribute('y'), 10);
                textX += (position.x - prevX);
                textY += (position.y - prevY);
                nodeMoving.parentNode.childNodes[1].setAttribute('x', textX);
                nodeMoving.parentNode.childNodes[1].setAttribute('y', textY);
            }
            
            // move in and out edges by the same amount
            nodeMoving.inEdges.map(function (item) { // modify last node in path
                movePath(item, (position.x - prevX), (position.y - prevY), 'end', -1);
            });
            nodeMoving.outEdges.map(function (item) { // modify the first node in path
                movePath(item, (position.x - prevX), (position.y - prevY), 'start', -1);
            });   

            prevX = position.x;
            prevY = position.y;
        } else if (elbowMoving !== null) {
            var position = getClickPosition(e, elbowMoving);
            // get position of this elbow 
            var elbowX = parseFloat(elbowMoving.getAttribute('cx'), 10);
            var elbowY = parseFloat(elbowMoving.getAttribute('cy'), 10);
            elbowX += (position.x - prevX);
            elbowY += (position.y - prevY);
            elbowMoving.setAttribute('cx', elbowX);
            elbowMoving.setAttribute('cy', elbowY);
            // the number of elbow in the path
            movePath(document.getElementById(elbowMoving.path), 
                    (position.x - prevX), (position.y - prevY), 'elbow',
                    document.getElementById(elbowMoving.path).elbows.indexOf(elbowMoving));

            prevX = position.x;
            prevY = position.y;
        }
    }
}


function unclickAll(e) {
    'use-strict';

    if (mode === 'change-mode') {
        nodeMoving = null;
        prevX = -1;
        prevY = -1;
        elbowMoving = null;
    }
}


function pathClicked(e) {
    'use-strict';

    if (mode === 'erase-mode') {
        erasePath(e.currentTarget);
    }

}


function erasePath(path) {
        var index = -1;
        var pathId = path.getAttribute('id');
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
            document.getElementById('mySVG').removeChild(item);
        });
        document.getElementById('mySVG').removeChild(path);
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
        document.getElementById('mySVG').removeChild(e.currentTarget);
    }
}


function changeMode(id) {
    'use-strict';

    //if (mode !== '') {
      $('#' + mode).toggleClass('clicked');
    //}
    mode = id;
    $('#' + mode).toggleClass('clicked');
    startNode = null;
}


function changeColour(id) {
    'use-strict';

    $('#' + nodeColourId).toggleClass('clicked');
    nodeColourId = id;
    $('#' + nodeColourId).toggleClass('clicked');

    if (mode === 'change-mode') {
        nodeSelected.parentNode.setAttribute('data-group', id);
    }
}


function addText() {
    'use-strict';

    var courseCode = document.getElementById('course-code').value;
    if (nodeSelected !== null && courseCode.length > 2) { // the rect element
        var g = nodeSelected.parentNode;
        if (g.childNodes.length > 1) {
            g.removeChild(g.childNodes[1]); 
        }
        var code = document.createElementNS(xmlns, 'text');
        code.setAttributeNS(null, 'id', 't' + nodeSelected.getAttribute('id').slice(1));
        code.setAttributeNS(null, 'x', parseFloat(nodeSelected.getAttribute('x'), 10) + 
                                        nodeWidth/2);
        code.setAttributeNS(null, 'y', parseFloat(nodeSelected.getAttribute('y'), 10) +
                                        nodeHeight/2);
        code.setAttributeNS(null, 'class', 'mylabel'); // note: label is a class in bootstrap
        var textNode = document.createTextNode(courseCode);
        code.appendChild(textNode);
        g.appendChild(code);
    }
}

// document ready
setupSVGCanvas();

$('.mode').each(function() {
    $(this). click(function () {
        changeMode(this.id);}); 
    });
$('.colour').each(function() {
    $(this). click(function () {
        changeColour(this.id);});
    });
$('#add-text').click(function (){
    addText();
});


function keyboard(e) {
    'use-strict';

    if (! $("#course-code").is(":focus")) {
        if (e.which == 78) {
            changeMode("node-mode"); // n
        } else if (e.which == 80) {
            changeMode("path-mode"); // p
        } else if (e.which == 77){
            changeMode("change-mode"); // m
        } else if (e.which == 69){
            changeMode("erase-mode"); // e
        } else if (e.which == 82){
            changeMode("region-mode"); // r
        }
    }
}

document.addEventListener('keydown', keyboard, false);


// TODO:
/*

1. regions creation
2. get substantial work done with saving graph 
3. node type buttons

- deselecting
- use https://www.dashingd3js.com/svg-paths-and-d3js

*/

// RANDOM
/*
- shortcuts: http://javascript.info/tutorial/keyboard-events
              http://unixpapa.com/js/key.html
- change mode to node-mode when colour changed ?
- document ready method ?
- when path created should end node be selected?
- key board shortcuts to switch modes
- make grid background optional
- colour picker for choosing colour of node
*/