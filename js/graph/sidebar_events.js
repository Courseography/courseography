$(".focus").click(function(e){
	'use strict';

	var id = $(this).attr('id');
	updateActiveFocus(id);
});

$("#close-focus").click(function(e) {
	clearFocus();
	$('ellipse.spotlight').remove();
	setMouseCallbacks();
})