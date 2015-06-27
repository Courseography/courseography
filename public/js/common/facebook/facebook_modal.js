/**
 * Opens the Facebook Posting modal.
 */
function openFBPostModal() {
    'use strict';

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
    var instructions = $('<p>Post your graph/grid selections as images on ' +
        'Facebook. (You can also right-click and select "Save Image As..." ' +
        'to download the image to your computer.)</p>');
    var bottomContentDiv = $('<div id="modal-buttons"></div>');
    var authToken = FB.getAuthResponse()['accessToken'];
    var input = $('<textarea class="form-control" placeholder="Photo description (optional)" name="message" rows="2" cols="200" id="fb-message"/>');
    var postButton = $('<button type="button" class="btn btn-primary">Post To Facebook</button>');
    var sessionButton = $('<button type="button" class="btn btn-primary">Switch Sessions</button>');

    contentDiv.attr('id', 'modal-content-container');

    postButton.click(function () {
        FB.login(function (response) {
            postButton.prop('disabled', true);
            postButton.text('Posting Image...');
            var val = $('#fb-message').val();
            postImage(authToken, img, val);
        }, {scope: 'publish_actions'});
    });

    sessionButton.click(function () {
        session = session === 'fall' ? 'spring' : 'fall';
        img = getGridImage(session);
        $('#post-image').attr('src', 'data:image/png;base64,' + img);
    });

    postButton.css('padding', '0.5em');
    sessionButton.css('padding', '0.5em');

    bottomContentDiv.append(postButton);

    topContentDiv.append(instructions);
    topContentDiv.append(
        $('<img id="post-image" src="data:image/png;base64,' + img + '" />'));

    if (context === 'grid') {
        bottomContentDiv.append(sessionButton);
    }

    topContentDiv.append(input);
    contentDiv.append(topContentDiv);
    contentDiv.append(bottomContentDiv);
    return contentDiv;
}
