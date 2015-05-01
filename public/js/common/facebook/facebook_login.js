$(document).ready(function () {
    'use strict';

    $.ajaxSetup({ cache: true });
    $.getScript('//connect.facebook.net/en_UK/all.js', function() {
        FB.init({
            appId   : '432140593606098',
            xfbml   : true,
            version : 'v2.1'
        });
        FB.Event.subscribe('auth.statusChange', function (response) {
            FB.getLoginStatus(function (response) {
                if (response.status === 'connected') {
                    $('#nav-fb-post').css('display', 'inline-block');
                } else {
                    $('#nav-fb-post').hide();
                }
            });
        });
    });

    $('#post-fb').click(openFBPostModal);
});


/**
 * Adds the user's name to the navigation bar.
 */
function addNameToNavBar() {
    'use strict';

    FB.api('/me', function (response) {
        $('#facebook-name').html(response.name);
    });
}


/**
 * Removes the name from the navigation bar.
 */
function removeNameFromNavBar() {
    'use strict';

    $('#facebook-name').empty();
}
