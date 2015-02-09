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
    document.body.appendChild(svg);
    document.getElementById('mySVG').addEventListener('mousedown', makeNode, false);
    document.getElementById('mySVG').addEventListener('mousemove', nodeMoved, false);
    document.getElementById('mySVG').addEventListener('mouseup', nodeUnclicked, false);

    // arrow head for paths
    // S.defs $ do
    //    S.marker ! A.id_ "arrow" ! A.viewbox "0 0 10 10" ! A.refx "1" ! A.refy "5" ! A.markerunits "strokeWidth" ! A.orient "auto" ! A.markerwidth "4.5" ! A.markerheight "4.5" $ do
    //        S.polyline ! A.points "0,1 10,5 0,9" ! A.fill "black"

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
        
        g.appendChild(node);
        document.getElementById('mySVG').appendChild(g);
        document.getElementById(nodeId).addEventListener('mousedown', nodeClicked, false);

        // select the newly created node and deselect the old selected node
        console.log(nodeSelected);
        if (nodeSelected !== null) {
            nodeSelected.parentNode.setAttribute('data-active', 'unselected');
        }
        nodeSelected = document.getElementById(nodeId);
        nodeId += 1;
    } else if (mode === 'path-mode') {
        // make elbow joint, only if the dummy point is outside the starting node
        if (startNode !== null && (position.x < startNode.getAttribute('x') || 
                                    position.x > parseFloat(startNode.getAttribute('x')) + nodeWidth)
                    && (position.y < startNode.getAttribute('y') || 
                                    position.y > parseFloat(startNode.getAttribute('y')) + nodeHeight)) {
            curPath += 'L' + position.x + ',' + position.y + ' ';
            console.log('adding elbow to it ' + curPath);  
        }
    }
}


function nodeClicked(e) {
    'use-strict';

    if (mode  === 'erase-mode') {
        document.getElementById('mySVG').removeChild(e.currentTarget.parentNode);
    } else if (mode === 'change-mode') {
        var position = getClickPosition(e, e.currentTarget);
        nodeMoving = e.currentTarget;
        nodeX = position.x;
        nodeY = position.y;
        
        // show which node has been selected
        if (nodeSelected !== null) {
            nodeSelected.parentNode.setAttribute('data-active', 'unselected');
        }
        nodeSelected = e.currentTarget;
        nodeSelected.parentNode.setAttribute('data-active', 'active');
    } else if (mode === 'path-mode') {
        if (startNode === null) {
            // this is the start node of the path about to be created
            startNode = e.currentTarget;
            curPath = 'M' + (parseFloat(startNode.getAttribute('x')) + nodeWidth/2) + ',' + startNode.getAttribute('y') + ' ';   
            console.log('starting it ' + curPath);
        } else {
            // make the path from startNode to current node then make startNode Null
            curPath += 'L' + (parseFloat(e.currentTarget.getAttribute('x')) + nodeWidth/2) + ',' + e.currentTarget.getAttribute('y');  
            var thePath = document.createElementNS(xmlns, 'path');
            thePath.setAttributeNS(null, 'd', curPath);
            thePath.setAttributeNS(null, 'fill', 'none');
            thePath.setAttributeNS(null, 'stroke', 'black');
            thePath.setAttributeNS(null, 'data-active', 'drawn');
            thePath.addEventListener('click', pathClicked, false);
            console.log('creating it ' + curPath);
            document.getElementById('mySVG').appendChild(thePath);
            startNode = null;
            curPath = null;
        }
    }
}


function nodeMoved(e) {
    'use-strict';

    if (mode === 'change-mode' && nodeMoving !== null) {
        var position = getClickPosition(e, nodeMoving);
        var rectX = parseFloat(nodeMoving.getAttribute('x'));
        var rectY = parseFloat(nodeMoving.getAttribute('y'));
        rectX += (position.x - nodeX);
        rectY += (position.y - nodeY);
        nodeMoving.setAttribute('x', rectX);
        nodeMoving.setAttribute('y', rectY);
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
x put all the jQuery .click definitions in a loop instead of for each button
x put all style related stuff in CSS especially all the clicked button stuff
x for node on click change to on mouse down on mouse move and on mouse up methods
- node type buttons
x connect CSS of main graph with the nodes and types in this graph 
x input field to add text
- add paths with elbow joints
    - pick best side of start node and end node to make line, so no overlap
    - moving path when start or end point of path move
    - moving elbow points
    - delete path if start or end node deleted
https://www.dashingd3js.com/svg-paths-and-d3js

*/

// RANDOM
/*
- change mode to node-mode when colour changed ?
- document ready method ?
- key board shortcuts to switch modes
- make a grid background for the svg canvas, could prove to be useful if we want to add snapping
http://www.openjs.com/scripts/events/keyboard_shortcuts/#disable_in_input
- colour picker for choosing colour: <input type='color'/>
*/