'use strict';

/**
 * Sets a cookie with name cookieName and value cookieValue.
 * @param {string} cookieName The name of the cookie.
 * @param {string} cookieValue The cookies value.
 */
export function setCookie(cookieName, cookieValue) {
    'use strict';

    var lifeSpanInDays = 300;
    document.cookie = cookieName +
                      '=' + cookieValue +
                      '; max-age=' + 60 * 60 * 24 * lifeSpanInDays;
}


/**
 * Gets a cookie with name cookieName.
 * @param {string} cookieName The name of the cookie being retrieved.
 * @returns {string} The cookie.
 */
export function getCookie(cookieName) {
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
