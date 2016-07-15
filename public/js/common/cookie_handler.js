/**
 * Sets a cookie with name cookieName and value cookieValue.
 * @param {string} cookieName The name of the cookie.
 * @param {string} cookieValue The cookies value.
 */
function setCookie(cookieName, cookieValue) {
    'use strict';

    var lifeSpanInDays = 300;
    document.cookie = cookieName.replace(/[^0-9a-zA-Z_\-]/g, '-') +
                      '=' + cookieValue.replace(/[^0-9a-zA-Z_\-]/g, '-') +
                      '; max-age=' + 60 * 60 * 24 * lifeSpanInDays;
}


/**
 * Gets a cookie with name cookieName.
 * @param {string} cookieName The name of the cookie being retrieved.
 * @returns {string} The cookie.
 */
function getCookie(cookieName) {
    'use strict';

    var name = cookieName.replace(/[^0-9a-zA-Z_\-]/g, '-') + '=';
    var cookies = document.cookie.split(';');
    for (var i = 0; i < cookies.length; i++) {
        var cookie = cookies[i].trim();
        if (cookie.indexOf(name) === 0) {
            return cookie.substring(name.length, cookie.length);
        }
    }
    return '';
}
