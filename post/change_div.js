$(document).ready(function() {
    'use-strict';
	
    $('#div_specialist, #div_major, #div_minor').hide();
});


$('#specialist').click(function (e) {
    'use-strict';
	
    e.preventDefault();
    $('#div_specialist, #div_major, #div_minor').hide();
    $('#div_specialist').show();
});
	
	
$('#major').click(function (e) {
    'use-strict';
	
    e.preventDefault();
    $('#div_specialist, #div_major, #div_minor').hide();
    $('#div_major').show();
});
	
	
$('#minor').click (function (e) {
    'user-strict';
	
    e.preventDefault();
    $('#div_specialist, #div_major, #div_minor').hide();
    $('#div_minor').show();
});
