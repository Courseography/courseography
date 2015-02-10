$(document).ready(function() {
    $.ajaxSetup({ cache: true });
    $.getScript('//connect.facebook.net/en_UK/all.js', function() {

        FB.init({
            appId      : '442286309258193',
            xfbml      : true,
            version    : 'v2.1'
        });
        FB.Event.subscribe('auth.statusChange', function (response) {
            FB.getLoginStatus(function (response) {
                if (response.status === 'connected') {
                    addNameToNavBar();
                } else {
                    removeNameFromNavBar();
                }
            });
        });

    });
    $('#post-fb').click(openFBPostModal);
});

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
    postButton.attr('href', 'post-fb');
    postButton.html('Post Image To Facebook');
    contentDiv.append(postButton);

    var p = $('<p></p>').html('Post to Facebook');

    var bottomContentDiv = $('<div></div>');
    bottomContentDiv.attr('id', 'bottom-content-container');
    bottomContentDiv.html('<img height="500" width="500" src="data:image/png;base64,' + img + '" />');

    contentDiv.append(p);
    contentDiv.append(bottomContentDiv);
    return contentDiv;
}

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
 * Adds the user's name to the navigation bar.
 */
function addNameToNavBar() {
    'use strict';
    
    FB.api('/me', function (response) {
        $('#facebook-name').html(response.name);
    });
}


/**
 * Removes the name from the navigation bar.
 */
function removeNameFromNavBar() {
    'use strict';
    
    $('#facebook-name').empty();
}
