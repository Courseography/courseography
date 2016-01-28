$('#specialist').click(function (e) {
    'use strict';

    e.preventDefault();
    openTab('specialist');
});


$('#major').click(function (e) {
    'use strict';

    e.preventDefault();
    openTab('major');
});


$('#minor').click(function (e) {
    'use strict';

    e.preventDefault();
    openTab('minor');
});


$('.code').click(function (e) {
    'use strict';

    e.preventDefault();
    $(this).parent().find('.more-info').toggle();
});


/**
 * Hides all currently open divs and resets navbar to display none of the links as clicked.
 */
function resetAttributes() {
    'use strict';

    $('#post_specialist').hide();
    $('#post_major').hide();
    $('#post_minor').hide();
    $('#specialist, #major, #minor').css('background-color', 'white');
}


/**
 * Resets cookies of all tabs to 'inactive' - not open.
 */
function resetTabCookies() {
    'use strict';

    setCookie('specialist', 'inactive');
    setCookie('major', 'inactive');
    setCookie('minor', 'inactive');
}


/**
 * Opens a specific tab.
 * @param {string} tab The tab that we want to open.
 */
function openTab(tab) {
    'use strict';

    resetAttributes();
    resetTabCookies();

    if (tab === 'specialist') {
        $('#post_specialist').show();
        $('#specialist').css('background-color', '#9C9C9C');
        setCookie('specialist', 'active');
    } else if (tab === 'major') {
        $('#post_major').show();
        $('#major').css('background-color', '#9C9C9C');
        setCookie('major', 'active');
    } else if (tab === 'minor') {
        $('#post_minor').show();
        $('#minor').css('background-color', '#9C9C9C');
        setCookie('minor', 'active');
    }
}


/**
 * Opens the tab that was last opened.
 */
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
