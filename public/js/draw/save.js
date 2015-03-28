function saveClicked() { // create the modal
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
		saveGraph();
	}
}


function saveGraph() {
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
		
		// turn svg elements to JSON objects

		// SEND TO DATABASE?

		removeSaveModal();
	}
}

function removeSaveModal() {
	$('#save-modal').fadeOut('slow');
	document.body.removeChild(document.getElementById('save-modal'));
	document.body.removeChild(document.getElementById('save-modal-blackout'));
}