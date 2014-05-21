function set_cookie (cookie_name, cookie_value, lifespan_in_days) {
    var domain_string = '' ;
    document.cookie = cookie_name +
                       "=" + encodeURIComponent( cookie_value ) +
                       "; max-age=" + 60 * 60 *
                       24 * lifespan_in_days +
                       "; path=/" + domain_string ;
    console.log("Cookie storage:" + cookie_name + ": " + cookie_value);
}

function getCookie(cname) {
    var name = cname + "=";
    var ca = document.cookie.split(';');
    for(var i=0; i<ca.length; i++) {
      var c = ca[i].trim();
      if (c.indexOf(name)==0) {
            console.log("Cookie retrieval " + name + ": " + c.substring(name.length,c.length));
            return c.substring(name.length,c.length);
          }
        }
      return "inactive";
}