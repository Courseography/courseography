var toggled = false;

$(document).ready (function () {
    'use strict';

    getGraphsInDatabase();
    updateFCECount();
});


$('#sidebar-button').click(function() {
    'use strict';

    toggleSidebar('button');
});


$('#focuses-nav').click(function (e) {
    'use strict';

    e.preventDefault();
    resetDivs();
    $('#focuses').show();
    $('#focuses-nav').addClass('active');
});


$('#graphs-nav').click(function (e) {
    'use strict';

    e.preventDefault();
    resetDivs();
    $('#graphs').empty();
    $('#graphs').show();
    $('#graphs-nav').addClass('active');
    createGraphButtons();
});


$('#graph').click(function (e) {
    'use strict';

    e.preventDefault();
    toggleSidebar('graph');
});


/**
 * Hides all currently open divs and resets navbar to display none of the links as clicked.
**/
function resetDivs() {
    'use strict';

    $('#focuses').hide();
    $('#graphs').hide();
    $('#graphs-nav, #focuses-nav').removeClass('active');
}


/**
 * Fills the count of FCEs in the sidebar.
**/
function fillFCECount() {
    'use strict';

    $('#fcecount').show();
    $('#fcecount').html('FCE Count: ' + currentFCEs.toFixed(1));
}


/**
 * Opens and closes the sidebar.
 * @param{string} location The location where you are clicking (either the sidebar button or the graph).
**/
function toggleSidebar(location) {
    'use strict';

    if (toggled) {
        toggled = false;
        resetDivs();
        $('#sidebar').animate({width: '40px'}, 'fast', undefined, function() {
            $('#fcecount').html('');
        });
        $('#reset').hide();
    } else if (!toggled && location === 'button') {
        toggled = true;
        $('#sidebar').animate({width: '400px'}, 'fast');
        fillFCECount();
        $('#focuses').show();
        $('#focuses-nav').addClass('active');

        $('#reset').show();

        enableReset();
    }
};
