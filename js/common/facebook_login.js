window.fbAsyncInit = function() {

    FB.init({
        appId      : '442286309258193',
        xfbml      : true,
        version    : 'v2.1'
    });

    FB.getLoginStatus(function(response) {
	    if (response.status === 'connected') {
	        console.log('Logged in.');
	        console.log('Welcome!  Fetching your information.... ');

	        FB.api('/me', function(response) {
	            console.log('Successful login for: ' + response.name);
	            $('#facebook-name').html(response.name);
	        });
	        
	    }
	    else {
	        console.log('Logged in.');
	    }
	});
};

// Includes the Facebook JavaScript SDK
(function(d, s, id) {
  var js, fjs = d.getElementsByTagName(s)[0];
  if (d.getElementById(id)) return;
  js = d.createElement(s); js.id = id;
  js.src = "//connect.facebook.net/en_US/sdk.js";
  fjs.parentNode.insertBefore(js, fjs);
}(document, 'script', 'facebook-jssdk'));

