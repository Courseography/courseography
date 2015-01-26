var nodeId = 0;
var nodeColour = '#FF8800';
//var nodes = [];
var mode = '';
var xmlns = 'http://www.w3.org/2000/svg';

function setupSVGCanvas() {
	'use-strict';

	var svg = document.createElementNS(xmlns, 'svg');
	svg.setAttribute('id', 'mySVG');
	svg.setAttribute('style', 'border: 2px solid black');
	svg.setAttribute('width', '800');
	svg.setAttribute('height', '500');
	svg.setAttributeNS('http://www.w3.org/2000/xmlns/', 'xmlns:xlink', 'http://www.w3.org/1999/xlink');
	document.body.appendChild(svg);
}

function getClickPosition(e) {
	'use-strict';

    var parentPosition = getPosition(e.currentTarget);
    var xPosition = e.clientX - parentPosition.x;
    var yPosition = e.clientY - parentPosition.y;

    // decide what to do based on what mode it is?
    if (mode === 'node') {
    	makeNode(xPosition, yPosition);
    }
}

function getPosition(elem) {
	'use-strict';

    var xPosition = 0;
    var yPosition = 0;
      
    while (elem) {
        xPosition += (elem.offsetLeft - elem.scrollLeft + elem.clientLeft);
        yPosition += (elem.offsetTop - elem.scrollTop + elem.clientTop);
        elem = elem.offsetParent;
    }
    return { x: xPosition, y: yPosition };
}

function makeNode(x, y) {
	'use-strict';

	var node = document.createElementNS(xmlns, 'rect');
	// check for overlaps and off the chart problems before creating
	node.setAttributeNS(null,'x',x);
	node.setAttributeNS(null,'y',y);
	node.setAttributeNS(null,'width',60);
	node.setAttributeNS(null,'height',30);
	node.setAttributeNS(null,'fill', nodeColour);
	node.setAttributeNS(null,"onclick", 'nodeSelected(this)');

	document.getElementById('mySVG').appendChild(node);
}

function nodeSelected(elem) {
	if (mode  === 'erase') {
		document.getElementById("mySVG").removeChild(elem);
	} 
}

$('#node-mode').click(function () {mode = 'node';});
$('#erase-mode').click(function () {mode = 'erase';});

setupSVGCanvas();
document.getElementById('mySVG').addEventListener('click', getClickPosition, false);
