// makes grid
var nodeId = 0;
var nodeColour = '#FF8800'
var nodes = []
//var mode = 'node'

function drawRect(x, y) {
	'use-strict';

	var c = document.getElementById('theCanvas');
	var ctx = c.getContext('2d');
	ctx.fillStyle = '#FF8800';
	ctx.fillRect(x, y, 80, 80);
}

var xmlns = 'http://www.w3.org/2000/svg';
$( '#target' ).click(function() {
  alert( 'Handler for .click() called.' );
});

function setupSVGCanvas(){
	var svg = document.createElementNS(xmlns, 'svg');
	svg.setAttribute('id', 'mySVG');
	svg.setAttribute('style', 'border: 2px solid black');
	svg.setAttribute('width', '800');
	svg.setAttribute('height', '700');
	svg.setAttributeNS('http://www.w3.org/2000/xmlns/', 'xmlns:xlink', 'http://www.w3.org/1999/xlink');
	document.body.appendChild(svg);
}

setupSVGCanvas();
document.getElementById('mySVG').addEventListener('click', getClickPosition, false);

function makeNode(x, y){
	var node = document.createElementNS(xmlns, 'rect');
	// check for overlaps and off the chart problems before creating
	node.setAttributeNS(null,'x',x);
	node.setAttributeNS(null,'y',y);
	node.setAttributeNS(null,'width',50);
	node.setAttributeNS(null,'height',50);
	node.setAttributeNS(null,'fill', nodeColour);

	document.getElementById('mySVG').appendChild(node);
}

function getClickPosition(e) {
    var parentPosition = getPosition(e.currentTarget);
    var xPosition = e.clientX - parentPosition.x;
    var yPosition = e.clientY - parentPosition.y;

    // decide what to do based on what element it is?
    makeNode(xPosition, yPosition);
}
 
function getPosition(elem) {
    var xPosition = 0;
    var yPosition = 0;
      
    while (elem) {
        xPosition += (elem.offsetLeft - elem.scrollLeft + elem.clientLeft);
        yPosition += (elem.offsetTop - elem.scrollTop + elem.clientTop);
        elem = elem.offsetParent;
    }
    return { x: xPosition, y: yPosition };
}
