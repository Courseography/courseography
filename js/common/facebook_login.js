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

    var img =getImage();
    postPhoto(img);
    var contentDiv = $('<div></div>');
    contentDiv.attr('id', 'modal-content-container');
    var postButton = $('<a></a>');
    postButton.attr('href', 'post-fb');
    postButton.html('Post Image To Facebook');
    contentDiv.append(postButton);

    var p = $('<p></p>').html('Post to Facebook');

    var bottomContentDiv = $('<div></div>');
    bottomContentDiv.attr('id', 'bottom-content-container');
    bottomContentDiv.html('<img height="500" width="500" src="data:image/png;base64,'+img+'" />');

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


/**
 * Posts a photograph to facebook.
 * @param img A Base 64 encoded string of an image.
 */
function postPhoto(img) {
    var access_token = FB.getAuthResponse()['accessToken'];
    blob = dataURItoBlob(img);

    /* When uploading images to Facebook, the data needs to be encoded as
     * form data/multipart.
     */
    var fd = new FormData();
    fd.append("access_token",img);
    fd.append("source", blob);
    fd.append("message","New Photo Text");

    /* Note: Under normal circumstances, it would be preferable to use the FB.api call.
     * Using FB.api() has proved to be a difficult task. This function accomplishes
     * the same task as the FB.api() call would, but does it with an ajax call.
     */
    $.ajax({
        url:"https://graph.facebook.com/me/photos?access_token=" + authToken,
        type:"POST",
        data:fd,
        processData:false,
        contentType:false,
        cache:false,
        success:function(data){
            console.log("Image posted");

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