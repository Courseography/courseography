var toggled = false;


$("#sidebar-button").click(function(){
	'use strict';

	toggleSidebar('button');
});

$('#focuses-nav').click(function (e) {
	'use strict';

	e.preventDefault();
	resetDivs();
	$('#focuses').show();
	$('#focuses-nav').css('background-color', '#46364A');

});


$('#graphs-nav').click(function (e) {
	'use strict';

	e.preventDefault();
	resetDivs();
	$('#graphs').show();
	$('#graphs-nav').css('background-color', '#46364A');

});

$('#graph').click(function (e) {
	'use strict';

	e.preventDefault();
	toggleSidebar('graph');
});

function resetDivs() {
	'use strict';

	$('#focuses').css('display', 'none');
	$('#graphs').css('display', 'none');
	$('#graphs-nav, #focuses-nav').css('background-color', '#CD96CD');
}

function fillFCECount() {
	'use strict';
	
	$("#fcecount").show();
	$('#fcecount').html("FCE Count: " + FCEs);
}

function toggleSidebar(location) {

    if (toggled) {
        toggled = false;
        resetDivs();
    	$('#sidebar').animate({width: "20px"}, "fast", undefined, function() {
    		$("#fcecount").html("");
    	});
    } else if (!toggled && location === 'button') {
        toggled = true;
        $('#sidebar').animate({width: "300px"}, "fast");

        fillFCECount();
        $('#focuses').show();
        $('#focuses-nav').css('background-color', '#46364A');

    }
};
