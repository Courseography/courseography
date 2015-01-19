$(document).ready(function() {
    'use-strict';
	
    $('#div_specialist, #div_major, #div_minor').hide();
    $('.reqs').hide();
});


$('#specialist').click(function (e) {
    'use-strict';
	
    e.preventDefault();
    $('#div_specialist, #div_major, #div_minor').hide();
    $('.reqs').hide();
    $('#div_specialist').show();
});
	
	
$('#major').click(function (e) {
    'use-strict';
	
    e.preventDefault();
    $('#div_specialist, #div_major, #div_minor').hide();
    $('.reqs').hide();
    $('#div_major').show();
});
	
	
$('#minor').click (function (e) {
    'user-strict';
	
    e.preventDefault();
    $('#div_specialist, #div_major, #div_minor').hide();
    $('.reqs').hide();
    $('#div_minor').show();
});


$('.csc').click (function (e) {
    'use-strict';

    e.preventDefault();
    $('.reqs').hide();
    if ($(this).parent().parent().parent().attr('id') == 'div_specialist') {
        $('#specialist_csc').show();
    }
});


$('.mat').click (function (e) {
    'use-strict';

    e.preventDefault();
    $('.reqs').hide();
    if ($(this).parent().parent().parent().attr('id') == 'div_specialist') {
        $('#specialist_mat').show();
    }
});