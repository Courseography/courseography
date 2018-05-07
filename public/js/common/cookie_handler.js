/**
 * Sets a local storage with name LSName and value LSValue.
 * @param {string} LSName The name of the local storage.
 * @param {string} LSValue The local storage value.
 */
function setLocalStorage(LSName, LSValue) {
    'use strict';
    localStorage.setItem(LSName, LSValue)
}


/**
 * Gets a local storage with name LSName.
 * @param {string} LSName The name of the local storage being retrieved.
 * @returns {string} The local storage.
 */
function getLocalStorage(LSName) {
    'use strict';

    if (!localStorage.getItem(LSName)) {
        return '';
    } else {
        return localStorage.getItem(LSName);
    }
}
