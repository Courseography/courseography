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
var nodeSelected = null; // for movement and path creation
var nodeX = -1; // for movement
var nodeY = -1; // for movement

//document.getElementById('red').style.spacity = 0.5;


function setupSVGCanvas() {
    'use-strict';

    var svg = document.createElementNS(xmlns, 'svg');
    svg.setAttribute('id', 'mySVG');
    svg.setAttributeNS('http://www.w3.org/2000/xmlns/', 'xmlns:xlink', 'http://www.w3.org/1999/xlink');
    document.body.appendChild(svg);
    document.getElementById('mySVG').addEventListener('click', makeNode, false);
}


function getClickPosition(e) {
    'use-strict';

    var parentPosition = getPosition(e.currentTarget);
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
        var position = getClickPosition(e);
        var node = document.createElementNS(xmlns, 'rect');

        console.log(position.x, position.y);
        // check for overlaps and off the chart problems before creating ?
        node.setAttributeNS(null, 'x', position.x);
        node.setAttributeNS(null, 'y', position.y);
        node.setAttributeNS(null, 'id', nodeId);
        node.setAttributeNS(null, 'width', 60);
        node.setAttributeNS(null, 'height', 30);
        node.setAttributeNS(null, 'fill', colours[nodeColourId]);
        node.setAttributeNS(null, 'class', 'node');
        
        document.getElementById('mySVG').appendChild(node);
        document.getElementById(nodeId).addEventListener('mousedown', nodeClicked, false);
        document.getElementById(nodeId).addEventListener('mousemove', nodeMoved, false);
        document.getElementById(nodeId).addEventListener('mouseup', nodeUnclicked, false);
        nodeId += 1;
    }
}


function nodeClicked(e) {
    'use-strict';

    if (mode  === 'erase-mode') {
        document.getElementById('mySVG').removeChild(e.currentTarget);
    } else if (mode === 'change-mode') {
        var position = getClickPosition(e);
        nodeSelected = e.currentTarget;
        nodeX = position.x;
        nodeY = position.y;
        console.log(nodeX, nodeY);
    }
}


function nodeMoved(e) {
    'use-strict';

    if (mode === 'change-mode' && nodeSelected !== null) {
        var position = getClickPosition(e);
        var rectX = parseFloat(e.currentTarget.getAttribute('x'));
        var rectY = parseFloat(e.currentTarget.getAttribute('y'));
        nodeSelected = e.currentTarget;
        rectX += (position.x - nodeX);
        rectY += (position.y - nodeY);
        e.currentTarget.setAttribute('x', rectX);
        e.currentTarget.setAttribute('y', rectY);
        nodeX = position.x;
        nodeY = position.y;
        console.log(e.currentTarget.x.animVal.value, e.currentTarget.y.animVal.value, nodeX, nodeY);
    }
}


function nodeUnclicked(e) {
    'use-strict';

    if (mode === 'change-mode') {
        nodeSelected = null;
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

    $('#' + nodeColourId).toggleClass('clicked'); // how to start?
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