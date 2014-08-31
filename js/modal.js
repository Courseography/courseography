/* global $ */
/* global nodes */

function createModalDiv(id) {
    "use strict";

    var div = $("<div></div>");
    div.attr('id', 'modal-content-container');
    var p = $("<p></p>").css("color", "white").html(fetchCourseDescription(id));
    div.append(p);
    var video = setupVideoPlayer();
    var timetable = setupTimeslot(id);
    var relatedLinks = setupRelatedLinks(id);
    div.css('overflow', 'auto');
    div.append(timetable);
    div.append(video);
    div.append(relatedLinks);
    return div;
}

function setupVideoPlayer() {
    "use strict";

    // Not divided up into 'attr' yet because 'controls preload' cannot be added that way...
    var videoDiv = $('<div></div>');
    videoDiv.css('display', 'inline')
            .css('float', 'left')
            .css('width', '45%');
    var video = $('<video id="course_video" class="video-js vjs-default-skin" controls preload="auto" width="100%" height="250"></video>');
    var src1 = $("<source></source>")
        .attr("src", "http://video-js.zencoder.com/oceans-clip.webm")
        .attr("type", "video/webm");
    var src2 = $("<source></source>")
        .attr("src", "http://video-js.zencoder.com/oceans-clip.ogv")
        .attr("type", "video/ogv");
    var src3 = $("<source></source>")
        .attr("src", "http://video-js.zencoder.com/oceans-clip.mp4")
        .attr("type", "video/mp4");
    video.append(src1).append(src2).append(src3);
    videoDiv.append(video);
    return videoDiv;
}

function setupTimeslot(id) {
    "use strict";

    var courseName;
    var timeslot = $('<div></div>');
    var title = $('<h3></h3>');
    timeslot.append(title);
    timeslot.append($('#timetableMain').children('tbody').children('tr').first().clone());

    $('.searchClass').each(function () {
        courseName = $(this).children('td').first().html();
        if (courseName.indexOf(id) > -1) {
            timeslot.append($(this).clone());
        }
    });
    timeslot.children('.searchClass').children('td').first().remove();
    timeslot.children('tr').children('td').first().remove();
    return timeslot;
}

function setupRelatedLinks(id) {
    "use strict";

    var relatedLinksDiv = $('<div></div>').css('display', 'inline')
                                          .css('float', 'right')
                                          .css('width', '45%')
                                          .css('height', '250');
    var title = $('<h3></h3>');
    relatedLinksDiv.append(title);
    return relatedLinksDiv;
}

function displayToolTip(nodeId) {
    "use strict";

    var rectObject = $("#" + nodeId).find("rect");
    var xPos = rectObject.attr("x") - 65;
    var yPos = rectObject.attr("y");
    var g = createG(nodeId);
    createRect(g, "node-tooltip", nodeId + "-tooltip", xPos, yPos, 60, 30, "black");
    createText(g, nodeId, "node-tooltip", nodeId + "-tooltip", xPos, yPos, 60, 30, "black");
}


/**
 * Creates an svg rect object and appends it to #graphRootSVG.
 * @param rectClass
 * @param rectId
 * @param posX The x position of the rect.
 * @param posY The y position of the rect.
 * @param width The width of the rect.
 * @param height The height of the rect.
 * @param color The fill and stroke color of the rect.
 */
function createRect(g, rectClass, rectId, posX, posY, width, height, color) {
    "use strict";

    var rect = $(document.createElementNS('http://www.w3.org/2000/svg', 'rect'))
        .attr("class", rectClass + "-rect " + rectId + "-rect")
        .attr("id", rectId + "-rect")
        .attr("x", posX)
        .attr("y", posY)
        .attr("rx", 10)
        .attr("ry", 10)
        .attr("fill", "white")
        .attr("stroke", color)
        .attr("stroke-width", 2)
        .attr("width", width)
        .attr("height", height);

    g.append(rect);

    $('.tooltip-group').hover(
        function () {
        clearAllTimeouts();
    }, function () {
        $('.tooltip-group').fadeOut(1000, function () {$(this).remove();});
    });
}


/**
 * Creates an svg text object and appends it to #graphRootSVG.
 * @param nodeId
 * @param rectClass
 * @param rectId
 * @param posX The x position of the rect.
 * @param posY The y position of the rect.
 * @param width The width of the rect.
 * @param height The height of the rect.
 * @param color The fill and stroke color of the rect.
 */
function createText(g, nodeId, rectClass, rectId, posX, posY, width, height, color) {
    "use strict";

    var text = $(document.createElementNS('http://www.w3.org/2000/svg', 'text'))
        .text("Info")
        .attr("class", rectClass + "-text " + rectId + "-text")
        .attr("id", rectId + "-text")
        .attr("x", parseFloat(posX)+ 30)
        .attr("y", parseFloat(posY) + 20);
    g.append(text);
}


function createG(nodeId) {
    "use strict";

    var g = $(document.createElementNS('http://www.w3.org/2000/svg', 'g'));
    g.attr('class', 'tooltip-group')
        .css("cursor", "pointer")
        .click(function () {
            if ($(".modal").length === 0) {
                var div = createModalDiv(nodeId);
                div.attr("title", nodeId)
                    .addClass("modal").dialog({
                        autoOpen: true,
                        show: {
                            effect: "blind",
                            duration: 1000
                        },
                        hide: {
                            effect: "blind",
                            duration: 1000
                        },
                        modal: true,
                        minWidth: 1000,
                        minHeight: 600,
                        closeText: "X",
                        close: function () {
                            $(this).remove();
                            $.each(nodes, function (index, elem) {
                                window[elem].updateSVG();
                            });
                            $('body').css('background', 'rgb(255,255,255)');
                        }});
                $('.node, .hybrid').attr('data-active', 'unlit');
                $('body').css('background', 'rgb(40,40,40)');
                setMouseCallbacks();
                enableVideoJS();
                $('.tooltip-group').remove();
            }
        });
    $('#graphRootSVG').append(g);
    return g;
}

function enableVideoJS() {
    "use strict";

    videojs(document.getElementsByClassName('vjs-default-skin')[0], {}, function () {});
}