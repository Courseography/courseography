/**
 * Sets a local storage with name LSName and value LSValue.
 * @param {string} LSName The name of the local storage.
 * @param {string} LSValue The local storage value.
 */
function setLocaStorage(LSName, LSValue) {
    'use strict';
    localStorage.setItem(
        LSName.replace(/[^0-9a-zA-Z_\-]/g, '-'),
        LSValue.replace(/[^0-9a-zA-Z_\-]/g, '-'))
}


/**
 * Gets a local storage with name LSName.
 * @param {string} LSName The name of the local storage being retrieved.
 * @returns {string} The local storage.
 */
function getLocaStorage(LSName) {
    'use strict';

    var name = LSName.replace(/[^0-9a-zA-Z_\-]/g, '-') + '=';
    if (!localStorage.getItem(name)) {
        return '';
    } else {
        return localStorage.getItem(name);
    }
}
