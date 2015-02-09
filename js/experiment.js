var nodeId = 0;
//var nodes = [];
var xmlns = 'http://www.w3.org/2000/svg';
var mode = 'node-mode';
var nodeColourId = 'red';
var colours = { 
     'red': '#D77546', 
     'green':'#2E8B57', 
     'blue':'#437699',
     'purple':'#46364A'
};
var nodeMoving = null; // for movement and path creation
var nodeX = -1; // for movement
var nodeY = -1; // for movement
var nodeSelected = null; // for adding text


function setupSVGCanvas() {
    'use-strict';

    var svg = document.createElementNS(xmlns, 'svg');
    svg.setAttribute('id', 'mySVG');
    svg.setAttributeNS('http://www.w3.org/2000/xmlns/', 'xmlns:xlink', 'http://www.w3.org/1999/xlink');
    document.body.appendChild(svg);
    document.getElementById('mySVG').addEventListener('mousedown', makeNode, false);
    document.getElementById('mySVG').addEventListener('mousemove', nodeMoved, false);
    document.getElementById('mySVG').addEventListener('mouseup', nodeUnclicked, false);
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

    // decide what to do based on what mode it is?
    if (mode === 'node-mode') {
        var g = document.createElementNS(xmlns, 'g');
        var position = getClickPosition(e, e.currentTarget);
        var node = document.createElementNS(xmlns, 'rect');

        g.setAttribute('class', 'node');
        g.setAttribute('id', 'g' + nodeId);
        g.setAttribute('data-active', 'active');
        g.setAttribute('data-group', nodeColourId);
    
        // check for overlaps and off the chart problems before creating ?
        node.setAttribute('x', position.x);
        node.setAttribute('y', position.y);
        node.setAttribute('rx', 4);
        node.setAttribute('ry', 4);
        node.setAttribute('id', nodeId);
        node.setAttribute('width', 40);
        node.setAttribute('height', 32);
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
        console.log(nodeX, nodeY);
    }
    // show which node has been selected
    if (mode !== 'erase-mode') {
        if (nodeSelected !== null) {
            nodeSelected.parentNode.setAttribute('data-active', 'unselected');
        }
        nodeSelected = e.currentTarget;
        nodeSelected.parentNode.setAttribute('data-active', 'active');
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


function changeMode(id) {
    'use-strict';

    //if (mode !== '') {
      $('#' + mode).toggleClass('clicked');
    //}
    mode = id;
    $('#' + mode).toggleClass('clicked');
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
        code.setAttributeNS(null, 'x', parseFloat(nodeSelected.getAttribute('x')) + 20);
        code.setAttributeNS(null, 'y', parseFloat(nodeSelected.getAttribute('y')) + 17);
        var textNode = document.createTextNode(courseCode);
        code.appendChild(textNode);
        console.log(code);
        g.appendChild(code);
    }
}

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
- input field to add text
- add paths with elbow joints
http://stackoverflow.com/questions/6725288/svg-text-inside-rect
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