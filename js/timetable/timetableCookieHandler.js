function setCookie (cookieName, cookieValue) {
    document.cookie = cookieName +
        "=" + cookieValue +
        "; max-age=" + 60 * 60 * 24 * 1000;
}


function getCookie(cookieName) {
    var name = cookieName + "=";
    var cookie = document.cookie.split(";");
    for(var i = 0; i < cookie.length; i++) {
      var c = cookie[i].trim();
      if (c.indexOf(name) === 0) {
            return c.substring(name.length, c.length);
      }
    }
    return [];
}