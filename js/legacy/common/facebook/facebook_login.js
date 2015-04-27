'use strict';

import $ from 'jquery';
import FB from 'fb';

import { openFBPostModal } from './facebook_modal';

$(document).ready(function () {
    FB.init({
        appId: '432140593606098',
        xfbml: true,
        version: 'v2.1'
    });
    FB.Event.subscribe('auth.statusChange', function () {
        FB.getLoginStatus(function (response) {
            if (response.status === 'connected') {
                $('#nav-fb-post').css('display', 'inline-block');
            } else {
                $('#nav-fb-post').hide();
            }
        });
    });

    $('#post-fb').click(openFBPostModal);
});


/**
 * Adds the user's name to the navigation bar.
 */
export function addNameToNavBar() {
    FB.api('/me', function (response) {
        $('#facebook-name').html(response.name);
    });
}


/**
 * Removes the name from the navigation bar.
 */
export function removeNameFromNavBar() {
    $('#facebook-name').empty();
}
