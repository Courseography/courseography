'use strict';

import $ from 'jquery';

import { enableReset } from './sidebar_events';

var toggled = false;

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
    getGraphsInDatabase();
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

    $('#focuses').css('display', 'none');
    $('#graphs').css('display', 'none');
    $('#graphs-nav, #focuses-nav').removeClass('active');
}


/**
 * Fills the count of FCEs in the sidebar.
**/
export function fillFCECount() {
    'use strict';

    $('#fcecount').show();
    $('#fcecount').html('FCE Count: ' + FCEs);
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
        $('#reset').css('display', 'none');
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
