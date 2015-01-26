$(document).ready(function() {
    'use-strict';
	
    resetAttributes();
});


$('#specialist').click(function (e) {
    'use-strict';
	
    e.preventDefault();
    resetAttributes();
    $('#div_specialist').show();
	$('#specialist').css('background-color', '#9C9C9C');	
});
	
	
$('#major').click(function (e) {
    'use-strict';
	
    e.preventDefault();
	resetAttributes();
    $('#div_major').show();
	$('#major').css('background-color', '#9C9C9C');
});
	
	
$('#minor').click (function (e) {
    'user-strict';
	
    e.preventDefault();
	resetAttributes();
    $('#div_minor').show();
	$('#minor').css('background-color', '#9C9C9C');
});

/**
    Hides all currently open divs and resets navbar to display none of the links as clicked.
**/
function resetAttributes() {
    document.getElementById('div_specialist').style.display = 'none';
	document.getElementById('div_major').style.display = 'none';
	document.getElementById('div_minor').style.display = 'none';
    $('#specialist, #major, #minor').css('background-color', 'white');
};



