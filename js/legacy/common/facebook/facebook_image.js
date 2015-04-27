'use strict';

import $ from 'jquery';

import { dataURItoBlob } from '../image_conversion';

/**
 * Posts an image to facebook.
 * @param authToken The user's access token.
 * @param img A Base 64 encoded string of an image.
 * @param message A message to be passed to facebook.
 */
export function postImage(authToken, img, message) {
    'use strict';

    var blob = dataURItoBlob(img);

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
