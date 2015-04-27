'use strict'

import $ from 'jquery'
import FB from 'fb'

import { openModal } from '../modal'
import { getGraphImage } from '../graph_image'
import { getGridImage } from '../grid_image'
import { postImage } from './facebook_image'

/**
 * Opens the Facebook Posting modal.
 */
export function openFBPostModal() {
    var div = createFBModalDiv();

    openModal('Post To Facebook', div);
}


/**
 * Creates and returns the Facebook modal content div.
 * @returns {jQuery} The Facebook modal content div.
 */
function createFBModalDiv() {
    'use strict';

    var context = $('#courseography-header').attr('context');
    var session = 'fall';
    var img = (context === 'graph') ? getGraphImage() : getGridImage(session);
    var contentDiv = $('<div></div>');
    var topContentDiv = $('<div></div>');
    var bottomContentDiv = $('<div id="modal-buttons"></div>');
    var authToken = FB.getAuthResponse().accessToken;
    var input = $('<input class="form-control" placeholder="Enter a message" name="message" type="text" maxlength="1000" id="fb-message"/>');
    var postButton = $('<button type="button" class="btn btn-primary">Post To Facebook</button>');
    var sessionButton = $('<button type="button" class="btn btn-primary">Switch Sessions</button>');

    contentDiv.attr('id', 'modal-content-container');

    postButton.click(function () {
        var val = $('#fb-message').val();
        postImage(authToken, img, val);
        contentDiv.dialog('close');
    });

    sessionButton.click(function () {
        session = session === 'fall' ? 'spring' : 'fall';
        img = getGridImage(session);
        $('#post-image').attr('src', 'data:image/png;base64,' + img);
    });

    postButton.css('padding', '0.5em');
    sessionButton.css('padding', '0.5em');


    bottomContentDiv.append(postButton);

    topContentDiv.html('<img id="post-image" src="data:image/png;base64,' + img + '" />');

    if (context === 'grid') {
        bottomContentDiv.append(sessionButton);
    }

    topContentDiv.append(input);
    contentDiv.append(topContentDiv);
    contentDiv.append(bottomContentDiv);
    return contentDiv;
}
