$(".focus").click(function(e){
	'use strict';

	var id = $(this).attr('id');

	console.log($('#' + id + '-details').css('height'));
	if ($('#' + id + '-details').css('height') === '180px') {
		clearFocus();
		$('ellipse.spotlight').remove();
		setMouseCallbacks();
		$('#' + id + '-details').animate({height: '2px'}, 'fast');
		$('#' + id + '-details').html("");
	} else {
		$('.details').css('height', '2px');
		updateActiveFocus(id);
		$('#' + id + '-details').animate({height: '180px'}, 'fast');
		$('#' + id + '-details').html(window[id + 'Description']);
	} 
});
