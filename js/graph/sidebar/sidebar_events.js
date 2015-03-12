$(".focus").click(function(e){
	'use strict';

	var id = $(this).attr('id');

	console.log($('#' + id + '-details').css('height'));
	if ($('#' + id + '-details').css('height') === '2px') {
		console.log('first satisfied');
		$('.details').css('height', '2px');
		updateActiveFocus(id);
		$('#' + id + '-details').animate({height: '180px'}, 'fast');
	} else if ($('#' + id + '-details').css('height') !== '2px') {
		console.log('second satisfied');
		clearFocus();
		$('ellipse.spotlight').remove();
		setMouseCallbacks();
		$('#' + id + '-details').animate({height: '2px'}, 'fast');
	}
});
