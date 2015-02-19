var nodeId = 0;
var nodeWidth = 40;
var nodeHeight = 32;
var xmlns = 'http://www.w3.org/2000/svg';
var mode = 'node-mode';
var nodeColourId = 'red';
var colours = { 
     'red': '#D77546', 
     'green':'#2E8B57', 
     'blue':'#437699',
     'purple':'#46364A'
};
var nodeMoving = null;      // for movement and path creation
var nodeX = -1;             // for movement
var nodeY = -1;             // for movement
var nodeSelected = null;    // for adding text
var startNode = null;       // for making paths
var curPath = null;         // for making paths with elbow joints
var curElbow = null;        // for remembering the last elbow


function setupSVGCanvas() {
    'use-strict';

    var svg = document.createElementNS(xmlns, 'svg');
    svg.setAttribute('id', 'mySVG');
    svg.setAttributeNS('http://www.w3.org/2000/xmlns/', 'xmlns:xlink', 'http://www.w3.org/1999/xlink');
    
    svg.addEventListener('mousedown', makeNode, false);
    svg.addEventListener('mousemove', moveNode, false);
    svg.addEventListener('mouseup', nodeUnclicked, false);

    svg.appendChild(setupMarker());
    document.body.appendChild(svg);
}


function setupMarker() {
    'use-strict'

    var defs = document.createElementNS(xmlns, 'defs');
    var marker = document.createElementNS(xmlns, 'marker');
    var polyline = document.createElementNS(xmlns, 'polyline');
    
    marker.setAttributeNS(null, 'id', 'arrow');
    marker.setAttributeNS(null, 'class', 'path');
    marker.setAttributeNS(null, 'viewBox', '0 0 10 10');
    marker.setAttributeNS(null, 'refX', '10');
    marker.setAttributeNS(null, 'refY', '5');
    marker.setAttributeNS(null, 'markerunits', 'strokeWidth');
    marker.setAttributeNS(null, 'orient', 'auto');
    marker.setAttributeNS(null, 'markerWidth', '4.5');
    marker.setAttributeNS(null, 'markerHeight', '4.5');

    polyline.setAttributeNS(null, 'points', '0,1 10,5 0,9');
    polyline.setAttributeNS(null, 'fill', 'black');

    marker.appendChild(polyline);
    defs.appendChild(marker);

    return defs
}


function getClickPosition(e, elem) {
    'use-strict';

    var parentPosition = getPosition(elem);
    var xPosition = e.clientX - parentPosition.x;
    var yPosition = e.clientY - parentPosition.y;

    return { x: xPosition, y: yPosition };
}


function getPosition(elem) {
    'use-strict';

    var xPosition = 0;
    var yPosition = 0;
      
    while (elem) {
        xPosition += elem.offsetLeft - elem.scrollLeft + elem.clientLeft;
        yPosition += elem.offsetTop - elem.scrollTop + elem.clientTop;
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
        node.setAttribute('id', nodeId);
        node.setAttribute('width', nodeWidth);
        node.setAttribute('height', nodeHeight);
        node.setAttribute('class', 'node');
        /*node.parents = []; */ node.attributes['parents'] = [];
        /*node.children = []; */ node.attributes['children'] = [];
        /*node.inEdges = []; */ node.attributes['inEdges'] = [];
        /*node.outEdges = []; */ node.attributes['outEdges'] = [];
        g.appendChild(node);
        document.getElementById('mySVG').appendChild(g);
        document.getElementById(nodeId).addEventListener('mousedown', nodeClicked, false);

        select(document.getElementById(nodeId));

        nodeId += 1;
    } else if (mode === 'path-mode') {
        // make elbow joint, only if the dummy point is outside the starting node
        if (startNode !== null && (position.x < startNode.getAttribute('x') || 
                                    position.x > parseFloat(startNode.getAttribute('x')) + nodeWidth) &&
                    (position.y < startNode.getAttribute('y') || 
                                    position.y > parseFloat(startNode.getAttribute('y')) + nodeHeight)) {
            if (curPath === null) {
                findClosest({x: parseFloat(startNode.getAttribute('x')), y: parseFloat(startNode.getAttribute('y'))},
                            'node', position, 'elbow');
            } else {
                curPath += 'L' + position.x + ',' + position.y + ' ';   
            }
            curElbow = position;
            console.log('adding elbow to it ' + curPath);  
        }
    }
}


function nodeClicked(e) {
    'use-strict';

    var svgDoc = document.getElementById('mySVG');
    var index = null;

    if (mode  === 'erase-mode') { 
        // remove any paths leading to and from this node from the other node's list of paths
        // and remove this node from the other nodes' adjacency lists
        e.currentTarget.attributes['inEdges'].map( function (item) {
            index = item.attributes['parents'].attributes['outEdges'].indexOf(item);
            if (index > -1) {
                (item.attributes['parents'].attributes['outEdges']).splice(index, 1);
            }
            index = item.attributes['parents'].attributes['children'].indexOf(e.currentTarget);
            if (index > -1) {
                (item.attributes['parents'].attributes['children']).splice(index, 1);
            }
            svgDoc.removeChild(item);
        });
        e.currentTarget.attributes['outEdges'].map( function (item) {
            index = item.attributes['children'].attributes['inEdges'].indexOf(item);
            if (index > -1) {
                item.attributes['children'].attributes['inEdges'].splice(index, 1);
            }
            index = item.attributes['children'].attributes['parents'].indexOf(e.currentTarget);
            if (index > -1) {
                (item.attributes['children'].attributes['parents']).splice(index, 1);
            }
            svgDoc.removeChild(item);
        });
        svgDoc.removeChild(e.currentTarget.parentNode);
    } else if (mode === 'change-mode') {
        var position = getClickPosition(e, e.currentTarget);
        nodeMoving = e.currentTarget;
        nodeX = position.x;
        nodeY = position.y;
        
        // show which node has been selected
        select(e.currentTarget);

    } else if (mode === 'path-mode') {
        if (startNode === null || startNode === e.currentTarget) {
            // this is the start node of the path about to be created
            startNode = e.currentTarget;
            select(e.currentTarget);
            console.log('Path started ---------------');
            curPath = null;
            curElbow = null;
        } else {
            // make the path from startNode to current node then make startNode Null
            if (curPath === null) {
                // later we can call findClosest with 'node', 'node'
                /*curPath = 'M' + (parseFloat(startNode.getAttribute('x')) + nodeWidth/2) + 
                                ',' + parseFloat(startNode.getAttribute('y')) +
                    ' L' + (parseFloat(e.currentTarget.getAttribute('x')) + nodeWidth/2) + 
                        ',' + (parseFloat(e.currentTarget.getAttribute('y')));*/
                findClosest( {x: parseFloat(startNode.getAttribute('x')), 
                              y: parseFloat(startNode.getAttribute('y'))},
                            'node', 
                            {x: parseFloat(e.currentTarget.getAttribute('x')), 
                             y: parseFloat(e.currentTarget.getAttribute('y'))},
                            'node');
            } else {
                findClosest( curElbow, 'elbow', 
                            {x: parseFloat(e.currentTarget.getAttribute('x')), 
                             y: parseFloat(e.currentTarget.getAttribute('y'))},
                            'node'); 
            }
            select(e.currentTarget);
              
            var thePath = document.createElementNS(xmlns, 'path');
            thePath.setAttributeNS(null, 'd', curPath);
            thePath.setAttributeNS(null, 'fill', 'none');
            thePath.setAttributeNS(null, 'stroke', 'black');
            thePath.setAttributeNS(null, 'data-active', 'drawn');
            thePath.setAttributeNS(null, 'marker-end', 'url(#arrow)');
            thePath.addEventListener('click', pathClicked, false);
            console.log('creating it ' + curPath);
            document.getElementById('mySVG').appendChild(thePath);

            thePath.attributes['parents'] = startNode;
            thePath.attributes['children'] = e.currentTarget;

            // update relationships
            startNode.attributes['children'].push(e.currentTarget);
            e.currentTarget.attributes['parents'].push(startNode);
            startNode.attributes['outEdges'].push(thePath);
            e.currentTarget.attributes['inEdges'].push(thePath);
            startNode = null;
            curPath = null;
            curElbow = null;
        }
    }
}


/*
Return the best coordinates for the start and end of a edge. theNode and end could
be a node or an elbow. Atleast one of beg and end must be a node.
*/
function findClosest(beg, typeB, end, typeE) {
    'use-strict'

    if (typeB === 'node' && typeE === 'elbow') {
        var theNode = beg;
        var theElbow = end;
    } else if (typeB === 'elbow' && typeE === 'node') {
        // only need to add end point to curPath
        var theNode = end;
        var theElbow = beg;
    } else {
        // top, bottom, left, right
        var theNode = null;
        var node1Edges = [{x: beg.x + nodeWidth/2, y: beg.y}, 
            {x: beg.x + nodeWidth/2, y: beg.y + nodeHeight}, 
            {x: beg.x + nodeWidth, y: beg.y + nodeHeight/2},
            {x: beg.x, y: beg.y + nodeHeight/2}];
        var node2Edges = [{x: end.x + nodeWidth/2, y: end.y}, 
            {x: end.x + nodeWidth/2, y: end.y + nodeHeight}, 
            {x: end.x + nodeWidth, y: end.y + nodeHeight/2},
            {x: end.x, y: end.y + nodeHeight/2}];
        var best_edges = [node1Edges[0], node2Edges[0]];
        var best_dist = dist(node1Edges[0], node2Edges[0]);
        for (var i = 0; i < 4; i++) {
            for (var j = 0; j < 4; j++) {
                if (dist(node1Edges[i], node2Edges[j]) < best_dist) {
                    best_edges = [node1Edges[i], node2Edges[j]];
                    best_dist = dist(node1Edges[i], node2Edges[j]);
                } 
            }
        }
        curPath = 'M' + best_edges[0].x + ',' + best_edges[0].y + ' L' +
                best_edges[1].x + ',' + best_edges[1].y ;
    }

    var nodeCoord = '';
    if (theNode && theNode.x < theElbow.x) { // elbow is to the right of theNode
        if (theNode.x + nodeWidth > theElbow.x || theNode.y - nodeHeight > theElbow.y) {
            // elbow is above theNode, pick top edge
            nodeCoord = (theNode.x + nodeWidth/2) +  ',' + theNode.y;
        } else if (theNode.x + nodeWidth > theElbow.x || theNode.y + 2 * nodeHeight < theElbow.y) {
            // elbow is below theNode, pick bottom edge
            nodeCoord = (theNode.x + nodeWidth/2) + ',' + (theNode.y + nodeHeight);
        } else { // pick right edge
            nodeCoord = (theNode.x + nodeWidth) + ',' + (theNode.y + nodeHeight/2);
        }
    } else if (theNode) { // theNode.x >= theElbow.x, elbow is to the left of theNode
        if (theNode.y - nodeHeight > theElbow.y) {
            // elbow is above theNode, pick top edge
            nodeCoord = (theNode.x + nodeWidth/2) + ',' + theNode.y;
        } else if (theNode.y + 2 * nodeHeight < theElbow.y) {
            // elbow is below theNode, pick bottom edge
            nodeCoord = (theNode.x + nodeWidth/2) + ',' + (theNode.y + nodeHeight);
        } else { // pick left edge
            nodeCoord = theNode.x + ',' + (theNode.y + nodeHeight/2);
        }
    }
    if (typeB === 'node' && typeE === 'elbow') {
        curPath = 'M' + nodeCoord + ' L' + end.x + ',' + end.y + ' ';
    } else if (typeB === 'elbow' && typeE === 'node') {
        // only need to add end point to curPath
        curPath += 'L' + nodeCoord;
    }
}


function dist(a, b) {
    return Math.sqrt((b.x - a.x) * (b.x - a.x) + (b.y - a.y) * (b.y - a.y))
}


function select(newNode) {
    'use-strict'

    if (nodeSelected !== null) {
        nodeSelected.parentNode.setAttribute('data-active', 'unselected');
    }
    nodeSelected = newNode;
    nodeSelected.parentNode.setAttribute('data-active', 'active');
    // console.log('parents: ', nodeSelected.attributes['parents']);
    // console.log('children: ', nodeSelected.attributes['children']);
}


function moveNode(e) {
    'use-strict';

    if (mode === 'change-mode' && nodeMoving !== null) {
        var position = getClickPosition(e, nodeMoving);
        var rectX = parseFloat(nodeMoving.getAttribute('x'));
        var rectY = parseFloat(nodeMoving.getAttribute('y'));
        rectX += (position.x - nodeX);
        rectY += (position.y - nodeY);
        nodeMoving.setAttribute('x', rectX);
        nodeMoving.setAttribute('y', rectY);

        if (nodeMoving.parentNode.childNodes.length > 1){
            var textX = parseFloat(nodeMoving.parentNode.childNodes[1].getAttribute('x'));
            var textY = parseFloat(nodeMoving.parentNode.childNodes[1].getAttribute('y'));
            textX += (position.x - nodeX);
            textY += (position.y - nodeY);
            nodeMoving.parentNode.childNodes[1].setAttribute('x', textX);
            nodeMoving.parentNode.childNodes[1].setAttribute('y', textY);
        }
        
        // move in and out edges by the same amount
        nodeMoving.attributes['inEdges'].map( function (item) { // modify last node in path
            movePath(item, (position.x - nodeX), (position.y - nodeY), 'end');
        });
        nodeMoving.attributes['outEdges'].map( function (item) { // modify the first node in path
            movePath(item, (position.x - nodeX), (position.y - nodeY), 'start');
        });   

        nodeX = position.x;
        nodeY = position.y;
        // console.log(nodeMoving.x.animVal.value, nodeMoving.y.animVal.value, nodeX, nodeY);
    }
}


function nodeUnclicked(e) {
    'use-strict';

    if (mode === 'change-mode') {
        nodeMoving = null;
        nodeX = -1;
        nodeY = -1;
    }
}


function pathClicked(e) {
    'use-strict'

    if (mode === 'erase-mode') { // need to remove path from node lists!
        document.getElementById('mySVG').removeChild(e.currentTarget);
    }
}


function movePath(path, xBy, yBy, startOrEnd) {
    'use-strict'

    var thePath = path.getAttribute('d');
    var theX = null;
    var theY = null;
    if (startOrEnd === 'start') {
        theX = parseFloat(thePath.slice(1, thePath.indexOf(','))) + xBy; // exclude the M
        theY = parseFloat(thePath.slice(thePath.indexOf(',') + 1, thePath.indexOf('L'))) + yBy;
        console.log(thePath);
        thePath = 'M' + theX + ',' + theY + thePath.slice(thePath.indexOf('L'));
        console.log(thePath);
    } else if (startOrEnd === 'end') {
        theX = parseFloat(thePath.slice(thePath.lastIndexOf('L') + 1, thePath.lastIndexOf(','))) + xBy;
        theY = parseFloat(thePath.slice(thePath.lastIndexOf(',') + 1)) + yBy;
        console.log(thePath);
        thePath = thePath.slice(0, thePath.lastIndexOf('L') + 1) + theX + ',' + theY;
        console.log(thePath);
    }
    path.setAttribute('d', thePath); 
}


function changeMode(id) {
    'use-strict';

    //if (mode !== '') {
      $('#' + mode).toggleClass('clicked');
    //}
    mode = id;
    $('#' + mode).toggleClass('clicked');
    startNode = null; // necessary?
}


function changeColour(id) {
    'use-strict';

    $('#' + nodeColourId).toggleClass('clicked');
    nodeColourId = id;
    $('#' + nodeColourId).toggleClass('clicked');
}


function addText() {
    'use-strict'

    var courseCode = document.getElementById("course-code").value;
    if (nodeSelected !== null && courseCode.length > 2) { // the rect element
        var g = nodeSelected.parentNode;
        if (g.childNodes.length > 1){
            g.removeChild(g.childNodes[1]); 
        }
    //    createText(g, nodeSelected.getAttribute('id'), 't' + nodeSelected.getAttribute('id'), nodeSelected.getAttribute('x'), 
    //                nodeSelected.getAttribute('y'), nodeSelected.getAttribute('width'), nodeSelected.getAttribute('height'), "black");
        var code = document.createElementNS(xmlns, 'text');
        code.setAttributeNS(null, 'id', 't' + nodeSelected.getAttribute('id'));
        code.setAttributeNS(null, 'x', parseFloat(nodeSelected.getAttribute('x')) + nodeWidth/2);
        code.setAttributeNS(null, 'y', parseFloat(nodeSelected.getAttribute('y')) + nodeHeight/2);
        var textNode = document.createTextNode(courseCode);
        code.appendChild(textNode);
        g.appendChild(code);
    }
}

// document ready
setupSVGCanvas();

$('.mode').each(function(index) {
  $( this ). click(function () {
    changeMode(this.id);}); 
});
$('.colour').each(function(index) {
  $(this). click(function () {
    changeColour(this.id);});
});
$('#add-text').click(function (){
    addText();
});

/*
$(document).bind('keydown', 'ctrl+n', changeMode('node-mode'));
$(document).bind('keydown', 'ctrl+m', changeMode('change-mode'));
$(document).bind('keydown', 'ctrl+e', changeMode('erase-mode'));
$(document).bind('keydown', 'ctrl+p', changeMode('path-mode'));
*/

/*
// define a handler
function keyboard(e) {

    // this would test for whichever key is 40 and the ctrl key at the same time
    if (e.keyCode == 17 && e.keyCode == 77) { // m
        // call your function to do the thing
        changeMode('move-mode');
    }
}
// register the handler 
document.addEventListener('keyup', keyboard, false);
*/
// TODO:
/*
- node type buttons
- add paths with elbow joints
    x fix relationships and multiple elbows (?)
    2. don't allow elbow points in end node
    3. pick best side of start node and end node to make line, so no overlap
    4. moving path when start or end point of path move
    5. moving elbow points
    6. Show partial paths
    x delete path if start or end node deleted
    
https://www.dashingd3js.com/svg-paths-and-d3js

*/

// RANDOM
/*
- change mode to node-mode when colour changed ?
- document ready method ?
- when path created should end node be selected?
- key board shortcuts to switch modes
- make a grid background for the svg canvas, could prove to be useful if we want to add snapping
http://www.openjs.com/scripts/events/keyboard_shortcuts/#disable_in_input
- colour picker for choosing colour of node: <input type='color'/>
*/