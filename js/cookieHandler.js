function set_cookie (cookie_name, cookie_value, lifespan_in_days) {
    var domain_string = '' ;
    if(cookie_value === "inactive") {
      deleteCookie(cookie_name);
    } else {
      document.cookie = cookie_name +
                         "=" + encodeURIComponent( cookie_value ) +
                         "; max-age=" + 60 * 60 *
                         24 * lifespan_in_days +
                         "; path=/" + domain_string ;
    }
    //console.log("Cookie storage:" + cookie_name + ": " + cookie_value);
}

function getCookie(cookie_name) {
    var name = cookie_name + "=";
    var cookie = document.cookie.split(';');
    for(var i=0; i<cookie.length; i++) {
      var c = cookie[i].trim();
      if (c.indexOf(name)==0) {
            //console.log("Cookie retrieval " + name + ": " + c.substring(name.length,c.length));
            return c.substring(name.length,c.length);
      }
    }
    return "inactive";
}

function deleteCookie(cookie_name) {
  document.cookie = cookie_name + "=; expires=Thu, 01 Jan 1970 00:00:00 GMT";
}