$(document).ready(function() {
    'use-strict';
	
    $('#div_specialist, #div_major, #div_minor').hide();
});


$('#specialist').click(function (e) {
    'use-strict';
	
    e.preventDefault();
	// reset nav bar colours
	$('nav ul li').css('background-color', 'white');
    $('nav > ul > li > a').css('color', 'black');

	
	// hide all divs
    $('#div_specialist, #div_major, #div_minor').hide();
	
	// show current div
    $('#div_specialist').show();
	
	// change current nav
	$('#specialist').css('background-color', '#9C9C9C');
    $('#specialist').css('#specialist > a').css('color', 'white');
});
	
	
$('#major').click(function (e) {
    'use-strict';
	
    e.preventDefault();
	// reset nav bar colours
	$('nav ul li').css('background-color', 'white');
    $('nav > ul > li > a').css('color', 'black');
	
	// hide all divs
    $('#div_specialist, #div_major, #div_minor').hide();
	
	// show current div
    $('#div_major').show();
	
	// change current nav
	$('#major').css('background-color', '#9C9C9C');
    $('#major').css('#major > a').css('color', 'white');
});
	
	
$('#minor').click (function (e) {
    'user-strict';
	
    e.preventDefault();
	// reset nav bar colours
	$('nav ul li').css('background-color', 'white');
    $('nav > ul > li > a').css('color', 'black');

	
	// hide all divs
    $('#div_specialist, #div_major, #div_minor').hide();
	
	// show current div
    $('#div_minor').show();
	
	// change current nav
	$('#minor').css('background-color', '#9C9C9C');
    $('#minor').css('#minor > a').css('color', 'white');
});

