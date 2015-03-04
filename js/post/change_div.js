$(document).ready(function () {
    'use strict';
	
    openLastActiveTab();
    updateAllCategories();
    updateNavPost();
});


$('#specialist').click(function (e) {
    'use strict';
	
    e.preventDefault();
    openTab('specialist');
    updateNavPost();
});


$('#major').click(function (e) {
    'use strict';
	
    e.preventDefault();
    openTab('major');
    updateNavPost();
});


$('#minor').click (function (e) {
    'use strict';
	
    e.preventDefault();
    openTab('minor');
    updateNavPost();
});

$('.code').click (function (e) {
    'use strict';
    
    e.preventDefault();
    $(this).parent().find('.more-info').toggle();
});

/**
 * Hides all currently open divs and resets navbar to display none of the links as clicked.
**/
function resetAttributes() {
    'use strict';

    $('#div_specialist').css('display', 'none');
    $('#div_major').css('display', 'none');
    $('#div_minor').css('display', 'none');
    $('#specialist, #major, #minor').css('background-color', 'white');
};

/**
 * Resets cookies of all tabs to 'inactive' - not open
**/
function resetTabCookies() {
    'use strict';

    setCookie('specialist', 'inactive');
    setCookie('major', 'inactive');
    setCookie('minor', 'inactive');
}

/**
 * Opens a specific tab.
 * @param {string} tab The tab that we want to open
**/
function openTab(tab) {
    'use strict';

    resetAttributes();
    resetTabCookies();

    if (tab === 'specialist') {
        $('#div_specialist').show();
        $('#specialist').css('background-color', '#9C9C9C');
        setCookie('specialist', 'active');
    } else if (tab === 'major') {
        $('#div_major').show();
        $('#major').css('background-color', '#9C9C9C');
        setCookie('major', 'active');
    } else if (tab === 'minor') {
         $('#div_minor').show();
        $('#minor').css('background-color', '#9C9C9C');
        setCookie('minor', 'active');
    }
}

/**
 * Opens the tab that was last opened. 
**/
function openLastActiveTab() {
    'use strict';

    if (getCookie('minor') === 'active') {
       openTab('minor');
    } else if (getCookie('major') === 'active') {
        openTab('major');
    } else {
        openTab('specialist');
    }
}
