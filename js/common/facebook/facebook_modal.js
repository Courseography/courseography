/**
 * Opens the Facebook Posting modal.
 */
function openFBPostModal() {
    'use strict';

    if ($('.modal').length === 0) {
        var div = createFBModalDiv();

        div.attr('title', 'Post To Facebook')
            .addClass('modal').dialog({
                autoOpen: true,
                modal: true,
                minWidth: 1000,
                minHeight: 600,
                closeText: 'X',
                open: function(event, ui) {
                    $('.ui-widget-overlay').bind('click', function () {
                        div.dialog('close');
                    }); },
                close: function () {
                    $(this).remove();
                    $.each(nodes, function (index, elem) {
                        window[elem].updateSVG();
                    });
                    $('body').css('background', 'rgb(255,255,255)');
                }});

        $('.node, .hybrid').attr('data-active', 'unlit');
        $('body').css('background', 'rgb(40,40,40)');

        $('.tooltip-group').remove();
    }
}


/**
 * Creates and returns the Facebook modal content div.
 * @returns {jQuery} The Facebook modal content div.
 */
function createFBModalDiv() {
    'use strict';

    var img = getImage();

    var contentDiv = $('<div></div>');
    contentDiv.attr('id', 'modal-content-container');
    var postButton = $('<a></a>');

    postButton.click(function () {
        var val = $('#fb-message').attr('value');
        postImage(authToken, img, val);
        contentDiv.dialog('close');
    });

    var authToken = FB.getAuthResponse()['accessToken'];
    postButton.html('Post Image To Facebook');
    contentDiv.append(postButton);

    var p = $('<p></p>').html('Post to Facebook');
    var input = $('<input name="message" type="text" maxlength="1000" id="fb-message"/>');
    var bottomContentDiv = $('<div></div>');
    bottomContentDiv.attr('id', 'bottom-content-container');
    bottomContentDiv.html('<img height="500" width="500" src="data:image/png;base64,' + img + '" />');

    contentDiv.append(p);
    contentDiv.append(input);
    contentDiv.append(bottomContentDiv);
    return contentDiv;
}