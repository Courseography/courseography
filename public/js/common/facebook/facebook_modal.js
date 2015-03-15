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

    var context = $("#courseography-header").attr("context");
    var session = "fall";
    var img = (context === "graph") ? getGraphImage() : getGridImage(session) ;

    var contentDiv = $('<div></div>');
    contentDiv.attr('id', 'modal-content-container');
    var postButton = $('<button type="button" class="btn btn-primary">Post to Facebook</button>');
    var sessionButton =
        $('<button type="button" class="btn btn-primary">Switch Sessions</button>');

    postButton.click(function () {
        var val = $('#fb-message').val();
        postImage(authToken, img, val);
        contentDiv.dialog('close');
    });

    sessionButton.click(function () {
        session = session === "fall" ? "spring" : "fall";
        var sessionImage = getGridImage(session);
        $("#post-image").attr("src",
            'data:image/png;base64,' + sessionImage);
        console.log("clol");
    });

    var authToken = FB.getAuthResponse()['accessToken'];
    postButton.html('Post Image To Facebook').css("margin-bottom", "10px");

    var input = $('<input class="form-control" placeholder="Enter a message" name="message" type="text" maxlength="1000" id="fb-message"/>');
    var leftContentDiv = $('<div></div>');
    var rightContentDiv = $('<div></div>');

    rightContentDiv.append(postButton);

    if (context === "grid") {
        rightContentDiv.append(sessionButton);
    }

    leftContentDiv.attr('id', 'left-modal-div')
                  .css('float', 'left');
    rightContentDiv.attr('id', 'right-modal-div')
                   .css('float', 'right')
                   .css('width', '40%');
    leftContentDiv.html('<img id="post-image" style="border-radius:15px;" height="500" width="500" src="data:image/png;base64,' + img + '" />');

    rightContentDiv.append(input);
    contentDiv.append(leftContentDiv);
    contentDiv.append(rightContentDiv);
    return contentDiv;
}