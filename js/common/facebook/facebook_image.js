/**
 * Requests an image from the server. The server automatically generates this
 * image.
 * @returns {String} The base64 representation of an image.
 */
function getImage() {
    'use strict';

    var img;

    $.ajax({
        url: 'image',
        async: false,
        success: function (data) {
            img = data;
        },
        error: function () {
            throw 'No image generated';
        }
    });

    return img;
}


/**
 * Posts an image to facebook.
 * @param authToken The user's access token.
 * @param img A Base 64 encoded string of an image.
 * @param message A message to be passed to facebook.
 */
function postImage(authToken, img, message) {
    blob = dataURItoBlob(img);

    /* When uploading images to Facebook, the data needs to be encoded as
     * form data/multipart.
     */
    var fd = new FormData();
    fd.append('access_token', authToken);
    fd.append('source', blob);
    fd.append('message', message);

    /* Note: Under normal circumstances, it would be preferable to use the FB.api call.
     * Using FB.api() has proved to be a difficult task. This function accomplishes
     * the same task as the FB.api() call would, but does it with an ajax call.
     */
    $.ajax({
        url: 'https://graph.facebook.com/me/photos?',
        type: 'POST',
        data: fd,
        processData: false,
        contentType: false,
        cache: false,
        success: function(data) {
            console.log('Image posted');

            // TODO: Interact with user to let them know that all is well.
            // TODO: Implement error handling.
        }
    });

}


/**
 * Converts Base 64 encoded URI data to binary blob.
 * Base 64 encoded data is not accepted when uploading to Facebook.
 * This snippet of code has been adapted from this thread:
 * http://stackoverflow.com/questions/4998908/convert-data-uri-to-file-then-append-to-formdata
 * @param dataURI {string} Base 64 URI encoded data.
 * @returns A binary blob of the decoded URI data.
 */
function dataURItoBlob(dataURI) {
    var byteString = atob(dataURI);
    var ab = new ArrayBuffer(byteString.length);
    var ia = new Uint8Array(ab);
    for (var i = 0; i < byteString.length; i++) {
        ia[i] = byteString.charCodeAt(i);
    }
    return new Blob([ab], { type: 'image/png' });
}