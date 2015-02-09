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


function setupSVGCanvas() {
    'use-strict';

    var svg = document.createElementNS(xmlns, 'svg');
    svg.setAttribute('id', 'mySVG');
    svg.setAttributeNS('http://www.w3.org/2000/xmlns/', 'xmlns:xlink', 'http://www.w3.org/1999/xlink');
    //svg.setAttributeNS(xmlns, 'xmlns', xmlns);
    
    svg.addEventListener('mousedown', makeNode, false);
    svg.addEventListener('mousemove', nodeMoved, false);
    svg.addEventListener('mouseup', nodeUnclicked, false);

    svg.appendChild(setupMarker());
    document.body.appendChild(svg);
}


    /*arrow head for paths
    BLAZE:
    S.defs $ do
        S.marker ! A.id_ "arrow" ! A.viewbox "0 0 10 10" ! A.refx "1" ! A.refy "5" ! 
        A.markerunits "strokeWidth" ! A.orient "auto" ! A.markerwidth "4.5" ! A.markerheight "4.5" $ do
            S.polyline ! A.points "0,1 10,5 0,9" ! A.fill "black"
    */
function setupMarker() {
    'use-strict'

    var defs = document.createElementNS(xmlns, 'defs');
    var marker = document.createElementNS(xmlns, 'marker');
    var polyline = document.createElementNS(xmlns, 'polyline');
    
    marker.setAttributeNS(null, 'id', 'arrow');
    marker.setAttributeNS(null, 'class', 'path');
    marker.setAttributeNS(null, 'viewBox', '0 0 10 10');
    marker.setAttributeNS(null, 'refx', '10');
    marker.setAttributeNS(null, 'refy', '7');
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
    // decide what to do based on what mode it is?
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
        node.attributes['parents'] = [];
        node.attributes['children'] = [];
        node.attributes['inEdges'] = [];
        node.attributes['outEdges'] = [];
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
            curPath += 'L' + position.x + ',' + position.y + ' ';
            console.log('adding elbow to it ' + curPath);  
        }
    }
}


function nodeClicked(e) {
    'use-stri`ct';

    var svgDoc = document.getElementById('mySVG');
    var index = null;

    if (mode  === 'erase-mode') { 
        // remove any paths leading to and from this node from the other node's list
        e.currentTarget.attributes['inEdges'].map( function (item) {
            index = item.attributes['start'].attributes['outEdges'].indexOf(item);
            if (index > -1) {
                console.log('delete index from outEdges: ?  ', index);
                (item.attributes['start'].attributes['outEdges']).splice(index, 1);
            }
            svgDoc.removeChild(item);
        });
        e.currentTarget.attributes['outEdges'].map( function (item) {
            index = item.attributes['end'].attributes['inEdges'].indexOf(item);
            if (index > -1) {
                console.log('delete index from inEdges: ? ', index);
                item.attributes['end'].attributes['inEdges'].splice(index, 1);
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
        if (startNode === null) {
            // this is the start node of the path about to be created
            startNode = e.currentTarget;
            curPath = 'M' + (parseFloat(startNode.getAttribute('x')) + nodeWidth/2) + 
                                ',' + startNode.getAttribute('y') + ' ';   
            console.log('starting it ' + curPath);
            select(e.currentTarget);
        } else {
            // make the path from startNode to current node then make startNode Null
            curPath += 'L' + (parseFloat(e.currentTarget.getAttribute('x')) + nodeWidth/2 /*- 10*/) + 
                        ',' + (parseFloat(e.currentTarget.getAttribute('y')) /*- 10*/);  
            var thePath = document.createElementNS(xmlns, 'path');
            thePath.setAttributeNS(null, 'd', curPath);
            thePath.setAttributeNS(null, 'fill', 'none');
            thePath.setAttributeNS(null, 'stroke', 'black');
            thePath.setAttributeNS(null, 'data-active', 'drawn');
            // thePath.setAttributeNS(null, 'marker-end', 'url(#arrow)');
            thePath.addEventListener('click', pathClicked, false);
            console.log('creating it ' + curPath);
            document.getElementById('mySVG').appendChild(thePath);

            thePath.attributes['start'] = startNode;
            thePath.attributes['end'] = e.currentTarget;

            // update relationships
            startNode.attributes['children'].push(e.currentTarget);
            e.currentTarget.attributes['parents'].push(startNode);
            startNode.attributes['outEdges'].push(thePath);
            e.currentTarget.attributes['inEdges'].push(thePath);

            startNode = null;
            curPath = null;
        }
    }
}

function select(newNode) {
    console.log(nodeSelected);
    if (nodeSelected !== null) {
        nodeSelected.parentNode.setAttribute('data-active', 'unselected');
    }
    nodeSelected = newNode;
    nodeSelected.parentNode.setAttribute('data-active', 'active');
    console.log('parents: ', nodeSelected.attributes['parents']);
    console.log('children: ', nodeSelected.attributes['children']);
}

function nodeMoved(e) {
    'use-strict';

    if (mode === 'change-mode' && nodeMoving !== null) {
        var position = getClickPosition(e, nodeMoving);
        var rectX = parseFloat(nodeMoving.getAttribute('x'));
        var rectY = parseFloat(nodeMoving.getAttribute('y'));
        var textX = parseFloat(nodeMoving.parentNode.childNodes[1].getAttribute('x'));
        var textY = parseFloat(nodeMoving.parentNode.childNodes[1].getAttribute('y'));
        rectX += (position.x - nodeX);
        rectY += (position.y - nodeY);
        textX += (position.x - nodeX);
        textY += (position.y - nodeY);
        nodeMoving.setAttribute('x', rectX);
        nodeMoving.setAttribute('y', rectY);
        nodeMoving.parentNode.childNodes[1].setAttribute('x', textX);
        nodeMoving.parentNode.childNodes[1].setAttribute('y', textY);
        nodeX = position.x;
        nodeY = position.y;
        console.log(nodeMoving.x.animVal.value, nodeMoving.y.animVal.value, nodeX, nodeY);
    }
}


function nodeUnclicked(e) {
    'use-strict';

    if (mode === 'change-mode') {
        nodeMoving = null;
        nodeX = -1;
        nodeY = -1;
        console.log(nodeX, nodeY);
    }
}


function pathClicked(e) {
    'use-strict'

    if (mode === 'erase-mode') {
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
        console.log(nodeSelected.getAttribute('id'), nodeSelected.getAttribute('x'));
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


// TODO:
/*
- node type buttons
- add paths with elbow joints
    - pick best side of start node and end node to make line, so no overlap
    - moving path when start or end point of path move
    - moving elbow points
    - delete path if start or end node deleted
    - don't allow elbow points in end node
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