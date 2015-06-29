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
                    $('.fb-login-button').hide();
                } else {
                    $('#nav-fb-post').hide();
                    $('.fb-login-button').show();
                }
            });
        });
    });

    $('#post-fb').click(openFBPostModal);
});
