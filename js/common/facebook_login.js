$(document).ready(function() {
    $.ajaxSetup({ cache: true });
    $.getScript('//connect.facebook.net/en_UK/all.js', function() {

        FB.init({
          appId: '442286309258193',
            xfbml      : true,
            version    : 'v2.1'
        });

        FB.getLoginStatus(function (response) {
            if (response.status === 'connected') {
                console.log('Logged in.');
                console.log('Welcome!  Fetching your information.... ');

                // Add the user's name to the navigation bar.
                FB.api('/me', function (response) {
                    console.log('Successful login for: ' + response.name);
                    $('#facebook-name').html(response.name);
                });

            } else {
                console.log('Not logged in.');
            }

        
        });

	    FB.Event.subscribe('auth.statusChange', function (response) {
	        FB.getLoginStatus(function (response) {
	            if (response.status !== 'connected') {
	                $('#facebook-name').empty();
	            }
	        });
	    });

    });
});