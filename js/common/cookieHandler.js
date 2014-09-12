/**
 *
 * @param {string} cookieName
 * @param {string} cookieValue
 */
function setCookie(cookieName, cookieValue) {
    'use strict';

    var lifeSpanInDays = 300;
    document.cookie = cookieName +
                      '=' + cookieValue +
                      '; max-age=' + 60 * 60 * 24 * lifeSpanInDays;
}


/**
 *
 * @param {string} cookieName
 * @returns {string}
 */
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
    return '';
}