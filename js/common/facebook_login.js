$(document).ready(function() {
    $.ajaxSetup({ cache: true });
    $.getScript('//connect.facebook.net/en_UK/all.js', function() {

        FB.init({
            appId      : '442286309258193',
            xfbml      : true,
            version    : 'v2.1'
        });


        FB.getLoginStatus(function (response) {
            if (response.status === 'connected') {
                FB.login(function() {}, {scope: 'publish_actions'});
                addNameToNavBar();
            }
        
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
});


/**
 * Adds the user's name to the navigation bar.
 */
function addNameToNavBar() {
    FB.api('/me', function (response) {
        console.log('Successful login for: ' + response.name);
        demoAPI(response);
        $('#facebook-name').html(response.name);
    });
}


/**
 * Removes the name from the navigation bar.
 */
function removeNameFromNavBar() {
    $('#facebook-name').empty();
}


function demoAPI(response) {
    FB.api('/me', function (response) {
    	console.log(response);
    });

    FB.api('/me?fields=birthday', function (response) {
    	console.log(response);
    });

    FB.api('/me/friends', {fields: 'name'}, function (response) {
    	console.log(response);
    });

    FB.api('/442286309258193/accounts/', {access_token: FB.getAccessToken()}, function (response) {
    	console.log(response);
    	console.log(FB);
    });
}