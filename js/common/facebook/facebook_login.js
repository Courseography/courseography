$(document).ready(function() {
    $.ajaxSetup({ cache: true });
    $.getScript('//connect.facebook.net/en_UK/all.js', function() {

        FB.init({
            appId      : '442286309258193',
            xfbml      : true,
            version    : 'v2.1'
        });
        FB.Event.subscribe('auth.statusChange', function (response) {
            FB.getLoginStatus(function (response) {
                if (response.status === 'connected') {
                    addNameToNavBar();
                } else {
                    removeNameFromNavBar();
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
