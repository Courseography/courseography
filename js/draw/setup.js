/* GLOBAL VARIABLES */
var nodeWidth = 40;
var nodeHeight = 32;
var xmlns = 'http://www.w3.org/2000/svg';
var svgDoc = null;

var nodeId = 0;
var mode = 'node-mode';
var nodeColourId = 'red';
var nodeMoving = null;      // for movement and path creation
var prevX = -1;             // for movement
var prevY = -1;             // for movement
var nodeSelected = null;    // for adding text or changing colour
var startNode = null;       // for making paths
var curPath = null;         // the path currently being created
var elbowMoving = null;     // for movement of elbow joints


/* SET UP SVG CANVAS */

/**
 * Initializes the SVG Canvas and its background.
 **/
function setupSVGCanvas() {
    'use-strict';

    var div = document.createElement('div');
    div.setAttribute('id', 'main');
    // bgdiv as sibling necessary to decrease grid opacity without effecting svg objects
    var bgdiv = document.createElement('div');
    bgdiv.setAttribute('id', 'background'); 
    var svg = document.createElementNS(xmlns, 'svg');
    svg.setAttribute('id', 'mySVG');
    svg.setAttributeNS('http://www.w3.org/2000/xmlns/', 'xmlns:xlink', 'http://www.w3.org/1999/xlink');
    
    svg.addEventListener('mousedown', makeNodePath, false);
    svg.addEventListener('mousemove', moveNodeElbow, false);
    svg.addEventListener('mouseup', unclickAll, false);

    svg.appendChild(setupMarker());
    div.appendChild(bgdiv);
    div.appendChild(svg);
    document.body.appendChild(div);
    svgDoc = document.getElementById('mySVG');
}

/**
 * Create the arrowhead marker to be used for paths.
 **/
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

setupSVGCanvas();

/* SET UP SIDEBAR AND ONCLICKS FOR BUTTONS */

$('.mode').each(function() {
    $(this). click(function () {
        changeMode(this.id);}); 
    });
$('.colour').each(function() {
    $(this). click(function () {
        changeColour(this.id);});
    });
$('#add-text').click(function () {
    addText();
});


/**
 * Handles keydown event e, possibly switching modes.
 * @param {object} e The keydown event.
 **/
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


/**
 * Changes the current mode mode to the new mode with id id.
 * @param {object} id The id of the new mode to be selected.
 **/
function changeMode(id) {
    'use-strict';

    //if (mode !== '') {
      $('#' + mode).toggleClass('clicked');
    //}
    if (mode === "path-mode") { 
        // clean up partial temp path
        if (curPath !== null) {
            startNode = null;
            curPath.elbows.map(function (item) {
                svgDoc.removeChild(item);
            });
            svgDoc.removeChild(curPath);
            curPath = null;
        }
    }

    mode = id;
    $('#' + mode).toggleClass('clicked');
}


/**
 * Changes the current colour to the new colour with id id.
 * @param {object} id The id of the new colour to be selected.
 **/
function changeColour(id) {
    'use-strict';

    $('#' + nodeColourId).toggleClass('clicked');
    nodeColourId = id;
    $('#' + nodeColourId).toggleClass('clicked');

    if (mode === 'change-mode') {
        nodeSelected.parentNode.setAttribute('data-group', id);
    }
}


/**
 * Adds the text from the input box to the currently selected node.
 **/
function addText() {
    'use-strict';

    var courseCode = document.getElementById('course-code').value;
    if (nodeSelected !== null && courseCode.length > 2) {
        var g = nodeSelected.parentNode;
        if (g.childNodes.length > 1) {
            g.removeChild(g.childNodes[1]); 
        }
        var code = document.createElementNS(xmlns, 'text');
        code.setAttributeNS(null, 'id', 't' + nodeSelected.id.slice(1));
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