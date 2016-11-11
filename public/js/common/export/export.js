$(document).ready(function () {
  $('#nav-export').click(function () {
    openExportModal();
  });
});


/**
 * Creates and displays the Export modal content div.
 */
function openExportModal() {
    'use strict';

    var context = $('#courseography-header').attr('context');
    var session = 'fall';
    var img = (context === 'graph') ? getGraphImage() : getGridImage(session);
}


/* Request bytestring of PDF of graph and timetable and convert to PDF*/
function getPDF() {
    'use strict';

    $.ajax({
        url: 'timetable-pdf',
        type: 'post',
        success: function(b64Data) {
         	var contentType = "application/pdf";
        	var blob = b64toBlob(b64Data, contentType);
			var blobUrl = window.URL.createObjectURL(blob);
			var link = document.createElement('a');
			link.href = blobUrl;
			link.download="graph_and_timeable.pdf";
            link.click();
        }
    });
}



function b64toBlob(b64Data, contentType, sliceSize) {
	contentType = contentType || '';
	sliceSize = sliceSize || 512;

	var byteCharacters = atob(b64Data);
	var byteArrays = [];

	for (var offset = 0; offset < byteCharacters.length; offset += sliceSize) {
		var slice = byteCharacters.slice(offset, offset + sliceSize);

		var byteNumbers = new Array(slice.length);
		for (var i = 0; i < slice.length; i++) {
			byteNumbers[i] = slice.charCodeAt(i);
		}

		var byteArray = new Uint8Array(byteNumbers);
		byteArrays.push(byteArray);
	}

	var blob = new Blob(byteArrays, {type: contentType});
	return blob;
}
