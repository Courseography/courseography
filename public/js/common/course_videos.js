/**
 * Enables VideoJS.
 * Note: Must be called after the video player is set up.
 */
function enableVideoJS() {
    'use strict';

    if (document.getElementsByClassName('vjs-default-skin').length > 0) {
        videojs(document.getElementsByClassName('vjs-default-skin')[0], {},
            function () {});
    }
}
