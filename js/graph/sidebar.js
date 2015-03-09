var toggled = false;


$("#sidebar-button").click(function(){
	'use strict';

    if (toggled) {
        toggled = false;
        resetDivs();
    	$('#sidebar').animate({width: "20px"}, "fast", undefined, function() {
    		$("#fcecount").html("");
    	});
    } else {
        toggled = true;
        $('#sidebar').animate({width: "300px"}, "fast");
        fillFCECount();
    }
});

$('#focuses-nav').click(function (e) {
	'use strict';

	e.preventDefault();
	resetDivs();
	$('#focuses').show();
	$('#focuses-nav').css('background-color', '#CD96CD');

});


$('#graphs-nav').click(function (e) {
	'use strict';

	e.preventDefault();
	resetDivs();
	$('#graphs').show();
	$('#graphs-nav').css('background-color', '#CD96CD');

});


function resetDivs() {
	'use strict';

	$('#focuses').css('display', 'none');
	$('#graphs').css('display', 'none');
	$('#graphs-nav, #focuses-nav').css('background-color', '#CD96CD');
}

function fillFCECount() {
	$("#fcecount").show();
	$('#fcecount').html("FCE Count: " + FCEs);
}