/* SET UP SIDEBAR AND ONCLICKS FOR BUTTONS */

for (const node of document.getElementsByClassName('mode')) {
    node.addEventListener('click', () => changeMode(node.id))
}

document.getElementById('add-text').addEventListener('click', addText)
document.getElementById('finish-region').addEventListener('click', finishRegion)

$('#colour-table').on('click', 'td', function() {
    document.getElementById('select-colour').jscolor.fromString($(this).css('backgroundColor'));
});

document.getElementById('save-graph').addEventListener('click', () => {
    $.ajax({
        url: '/save-json',
        data: {'jsonData' : convertSvgToJson(),
               'nameData' : document.getElementById('area-of-study').value},
        method: 'POST',
        success: function(status) {
            console.log(status);
        },
        error: function(xhr, status, err) {
            console.error('save-graph:', status, err.toString());
        }
    });
});

document.getElementById('submit-graph-name').addEventListener('click', () => {
    $.ajax({
        url: '/get-json-data',
        data: {graphName : document.getElementById('area-of-study').value},
        dataType: 'json',
        success: function(data) {
            var div = document.getElementById('main');
            document.body.removeChild(div);
            setupSVGCanvas();
            svgDoc.appendChild(setupMarker());
            renderJson(JSON.stringify(data));
        },
        error: function(xhr, status, err) {
            console.error('graphs', status, err.toString());
        }
    });
});

document.addEventListener('keydown', keyboard, false);


/**
 * Handles keydown event e, possibly switching modes.
 * @param {object} e The keydown event.
 */
function keyboard(e) {
    'use strict';

    if (document.getElementById("course-code") == document.activeElement) {
        if (e.which === 78) {
            changeMode("node-mode"); // n
        } else if (e.which === 80) {
            changeMode("path-mode"); // p
        } else if (e.which === 77){
            changeMode("change-mode"); // m
        } else if (e.which === 69){
            changeMode("erase-mode"); // e
        } else if (e.which === 82){
            changeMode("region-mode"); // r
        } else if (e.which === 70){
            finishRegion(); // f
        }
    }
}


/**
 * Changes the current mode mode to the new mode with id id.
 * @param {object} id The id of the new mode to be selected.
 */
function changeMode(id) {
    'use strict';

    document.getElementById(mode).classList.toggle('clicked');

    if (mode === 'path-mode') {
        // clean up partial temp path
        if (curPath !== null) {
            startNode = null;
            curPath.elbows.map(function (item) {
                svgDoc.removeChild(item);
            });
            svgDoc.removeChild(curPath);
            curPath = null;

        }
    } else if (mode === 'region-mode') {
        //svgDoc.removeChild(startPoint);
        if (curPath !== null) {
            curPath.elbows.map(function (item) {
                    svgDoc.removeChild(item);
                });
            document.getElementById('regions').removeChild(curPath);
            curPath = null;
            startPoint = null;
        } else if (startPoint !== null) {
            svgDoc.removeChild(startPoint);
            startPoint = null;
        }

    }

    mode = id;
    document.getElementById(mode).classList.toggle('clicked');
}


/**
 * Adds the text from the input box to the currently selected node.
 */
function addText() {
    'use strict';

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

        document.getElementById('t' + nodeSelected.id.slice(1)).addEventListener('mousedown', nodeClicked, false);
    }
}
