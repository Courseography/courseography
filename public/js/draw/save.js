
function saveClicked() { // create the modal, correct way to do this?
	'use strict';

	var blackout = document.createElement('div');
	blackout.setAttribute('id', 'save-modal-blackout');
	var div = document.createElement('div');
    div.setAttribute('id', 'save-modal');
    var subdiv = document.createElement('div');
    subdiv.setAttribute('id', 'save-modal-subdiv');
    subdiv.innerHTML = 'SAVE GRAPH: ';
    document.body.appendChild(blackout);
    document.body.appendChild(div);
    blackout.addEventListener('click', removeSaveModal, false);
	if (savedAs === '') {
		var input = document.createElement('input'); 
	    input.setAttribute('id', 'save-name');
	    input.setAttribute('class', 'save-name');
	    input.setAttribute('name', 'save-name');
	    input.setAttribute('placeholder', 'Name of File');
	    input.setAttribute('size', '30');
	    input.setAttribute('autocomplete', 'off');
	    input.setAttribute('type', 'text');
	    var sbutton = document.createElement('div');
	    sbutton.setAttribute('id', 'save-modal-button');
	    sbutton.setAttribute('class', 'button');
	    sbutton.innerHTML = 'Save';

	    var cbutton = document.createElement('div');
	    cbutton.setAttribute('id', 'save-modal-button');
	    cbutton.setAttribute('class', 'button');
	    cbutton.innerHTML = 'Cancel';

	    subdiv.appendChild(input);
	    subdiv.appendChild(sbutton);
	    subdiv.appendChild(cbutton);
	    div.appendChild(subdiv);
	    sbutton.addEventListener('click', saveGraph, false);
	    cbutton.addEventListener('click', removeSaveModal, false);
	} else { // if saved before then don't ask for name
		saveGraph(); // should remove graph already stored and restore everything?
	}
}


function saveGraph() {
	'use strict';

	if (savedAs === '') {
		savedAs = document.getElementById('save-name').value;
		if (savedAs.length > 0 && savedAs.indexOf(' ') == -1) { // non empty and no spaces
			document.getElementById('save-modal').innerHTML = 'Saving ' + savedAs + ' ...';
		} else {
			savedAs = '';
		}
	}

	if (savedAs !== '') {
		// remove any incomplete paths or regions


		// convert svg elems to json
		var rectAndText = getRect();
		// make request
		$.ajax({
			url: 'save',
			async: false,
			method: 'get',
			data: 'objs=' + JSON.stringify({'graph': {'gId': 100, 'title': savedAs}, 
											'rects': rectAndText.rects, 
											'texts': rectAndText.texts}),
			success: function () { console.log('Ajax request sent!'); },
			error: function (xhr, status, error) {
						console.log('Error with Ajax request!');
						console.log(status);
						console.log(error);
					}
		});

		removeSaveModal();
	}
}


/*
Shape
    gId Int64
    id_ String
    pos Point
    width Double
    height Double
    fill String
    stroke String
    text [Text]
    tolerance Double
    type_ ShapeType
Text
    gId Int64
    rId String
    pos Point
    text String
    deriving Show
*/

function getRect() {
	'use strict';

	var rects = [];
	var texts = [];
	var rect;
	var text = null;
	var textEl;
	$('rect').each(function(element) {
		if ($(this).parent().children().length > 1) {
                textEl = $(this).parent().children()[1];
                text = {'gid': 		100,
                		'rId': 		$(this).attr('id'),
						'pos': 		{x: $(this).attr('x') , y: $(this).attr('y')},
						'text': 	textEl.innerHTML}
				console.log(text);
				texts.push(text);
        }

		rect = {'gId': 		100, 
				'id_': 		$(this).attr('id'),
				'pos': 		{x: $(this).attr('x') , y: $(this).attr('y')},
				'width': 	nodeWidth,
				'height': 	nodeHeight,
				// easier/better way? 
				// document.defaultView.getComputedStyle(document.getElementById('n0'), null).getPropertyValue('fill')
				'fill': 	$(this).parent().attr('data-active'), 
				'stroke': 	$(this).parent().attr('data-group'),
				'text': 	[text],
				'tolerance': 0, // ?
				'type_': 	'rect'};
		console.log(rect);
		rects.push(rect);
		text = null;
	});

	return {'rects': rects, 'texts': texts}
}


function removeSaveModal() { // remove the modal
	'use strict';

	$('#save-modal').fadeOut('slow');
	document.body.removeChild(document.getElementById('save-modal'));
	document.body.removeChild(document.getElementById('save-modal-blackout'));
}

/*
Path
    gId Int64
    id_ String
    points [Point]
    fill String
    stroke String
    isRegion Bool
    source String
    target String
    deriving Show
 */
function getPath() {
	'use strict';

	var paths = [];
	var path;
	var points;
	$('path').each(function(element) {
		/*points = $(this).attr('elbows').map(function (elbow) {
			console.log({x: elbow.attr('cx'), y: elbow.attr('cy')})
		}); */ // not working
		if ($('path').attr('class') === 'region') {
		path = {'gId': 		100, 
				'id_': 		$(this).attr('id'),
				'points': 	[],// {x: $(this).attr('x') , y: $(this).attr('y')},
				// easier/better way? 
				// document.defaultView.getComputedStyle(document.getElementById('n0'), null).getPropertyValue('fill')
				'fill': 	$(this).attr('data-group'), 
				'stroke': 	$(this).attr('data-active'), // always black for regions and paths
				'isRegion': true,
				'source': 	$(this).attr('id'),// this value doesn't matter
				'target': 	$(this).attr('id')
				}; // this value doesn't matter
		} else {
		path = {'gId': 		100, 
				'id_': 		$(this).attr('id'),
				'points': 	[],// {x: $(this).attr('x') , y: $(this).attr('y')},
				// easier/better way? 
				// document.defaultView.getComputedStyle(document.getElementById('n0'), null).getPropertyValue('fill')
				'fill': 	'black', 
				'stroke': 	$(this).attr('data-active'), // always black for regions and paths
				'isRegion': false,
				'source':   $(this).attr('id').slice(0, $(this).attr('id').indexOf('n', 1)),
				'target': 	$(this).attr('id').slice($(this).attr('id').indexOf('n', 1))
				};
		}
		console.log(path);
		paths.push(path);
	});

	return paths
}