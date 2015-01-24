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
                    setPostFBClick();
                } else {
                    removeNameFromNavBar();
                }
            });
        });

    });
});

function setPostFBClick() {
    var postFBButton = $("#post-to-facebook-button");
    var response;
    var id;

    FB.api('/me', function (response) {
        id = response.id;
        console.log(id);
        console.log(response);
    });

    postFBButton.click(function () {
        $.ajax({
            url: 'post-fb?at=' + FB.getAuthResponse()['accessToken'] +
                 '&id=' + id,
            async: false,
            success: function (data) {
                response = data;
            },
            error: function () {
                alert("Could not post to facebook. Hmm.");
            }
        });
        postFBButton.html(response);
    });
}


/**
 * Adds the user's name to the navigation bar.
 */
function addNameToNavBar() {
    FB.api('/me', function (response) {
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