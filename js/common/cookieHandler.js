function setCookie(cookieName, cookieValue) {
    'use strict';

    var lifeSpanInDays = 300;
    document.cookie = cookieName +
                      '=' + cookieValue +
                      '; max-age=' + 60 * 60 * 24 * lifeSpanInDays;
}


// Right now, only used to get the status of a node in the graph.
function getCookie(cookieName) {
    'use strict';

    var name = cookieName + '=';
    var cookie = document.cookie.split(';');
    for (var i = 0; i < cookie.length; i++) {
      var c = cookie[i].trim();
      if (c.indexOf(name) === 0) {
            return c.substring(name.length, c.length);
      }
    }
    return 'inactive';
}


// Right now, only used to get JSON for timetable (grid)
function getJSONCookie(cookieName) {
    'use strict';

    var name = cookieName + '=';
    var cookie = document.cookie.split(';');
    for(var i = 0; i < cookie.length; i++) {
      var c = cookie[i].trim();
      if (c.indexOf(name) === 0) {
            return c.substring(name.length, c.length);
      }
    }
    return [];
}