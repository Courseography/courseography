function setCookie (cookie_name, cookie_value) {
    var lifespan_in_days = 300;
    var domain_string    = '' ;
    document.cookie = cookie_name 
                      + "=" + encodeURIComponent(cookie_value)
                      + "; max-age=" + 60 * 60 * 24 * lifespan_in_days;
}

function getCookie(cookie_name) {
    var name = cookie_name + "=";
    var cookie = document.cookie.split(';');
    for(var i = 0; i < cookie.length; i++) {
      var c = cookie[i].trim();
      if (c.indexOf(name) === 0) {
            return c.substring(name.length, c.length);
      }
    }
    return "inactive";
}