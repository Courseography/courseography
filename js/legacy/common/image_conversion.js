/**
 * Converts Base 64 encoded URI data to binary blob.
 * Base 64 encoded data is not accepted when uploading to Facebook.
 * This snippet of code has been adapted from this thread:
 * http://stackoverflow.com/questions/4998908/convert-data-uri-to-file-then-append-to-formdata
 * @param dataURI {string} Base 64 URI encoded data.
 * @returns A binary blob of the decoded URI data.
 */
export function dataURItoBlob(dataURI) {
    'use strict';

    var byteString = atob(dataURI);
    var ab = new ArrayBuffer(byteString.length);
    var ia = new Uint8Array(ab);
    for (var i = 0; i < byteString.length; i++) {
        ia[i] = byteString.charCodeAt(i);
    }
    return new Blob([ab], { type: 'image/png' });
}
