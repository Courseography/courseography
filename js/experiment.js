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
        //node.setAttribute('fill', colours[nodeColourId]);
        node.setAttribute('class', 'node');
        
        g.appendChild(node);
        document.getElementById('mySVG').appendChild(g);
        document.getElementById(nodeId).addEventListener('mousedown', nodeClicked, false);
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


setupSVGCanvas();

$('.mode').each(function(index) {
  $( this ). click(function () {
    changeMode(this.id);}); 
});
$('.colour').each(function(index) {
  $(this). click(function () {
    changeColour(this.id);});
});


// TODO:
/*
- document ready method ?
x put all the jQuery .click definitions in a loop instead of for each button
x put all style related stuff in CSS especially all the clicked button stuff
- for node on click change to on mouse down on mouse move and on mouse up methods
    - in erase mode erase the node on up i guess
    - in change mode make it the current node and on move calculate the change in p
        position of the cursor and move the x and y coord of the current node accordingly
        on mouse up unselect the node, (i.e. make the current node var NULL).
- node type buttons
- connect CSS of main graph with the nodes and types in this graph 
- colour picker for choosing colour: <input type='color'/>
*/