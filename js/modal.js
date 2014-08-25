/* global d3 */
/* global $ */
/* global nodes */

function createModalDiv(id) {
    "use strict";

    var div = $("<div></div>");
    var p = $("<p></p>").css("color", "white").html(fetchCourseDescription(id));
    div.append(p);

    var video = $('<video id="course_video" class="video-js vjs-default-skin" controls preload="auto" width="50%" height="250"></video>');
    var src1 = $("<source></source>")
                .attr("src", "http://video-js.zencoder.com/oceans-clip.webm").attr("type", "video/webm");
    var src2 = $("<source></source>")
                .attr("src", "http://video-js.zencoder.com/oceans-clip.ogv").attr("type", "video/ogv");
    var src3 = $("<source></source>")
                .attr("src", "http://video-js.zencoder.com/oceans-clip.mp4").attr("type", "video/mp4");
    video.append(src1).append(src2).append(src3);
    div.append(video);
    return div;
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

    var rect = g.append('rect')
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
        .attr("height", height)
        .attr("fill-opacity", 0)
        .attr("stroke-opacity", 0)
        .transition()
        .duration(1000)
        .ease('elastic')
        .attr("fill-opacity", 1)
        .attr("stroke-opacity", 1);
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

    g.append("text").text("Info")
        .attr("class", rectClass + "-text " + rectId + "-text")
        .attr("id", rectId + "-text")
        .attr("x", parseFloat(posX)+ 30)
        .attr("y", parseFloat(posY) + 20)
        .attr("fill-opacity", 0)
        .attr("stroke-opacity", 0)
        .transition()
        .duration(1000)
        .ease('elastic')
        .attr("fill-opacity", 1)
        .attr("stroke-opacity", 1);
}


function createG(nodeId) {
    var g = d3.select('#graphRootSVG').append('g')
        .attr('class', 'tooltip-group')
        .style("cursor", "pointer")
        .on("click", function () {
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
        }).on('mouseover', function () {
            clearAllTimeouts();
        });

    $('.tooltip-group').mouseout(function () {
        $('.tooltip-group').remove();
    });
    return g;
}

function enableVideoJS() {
    "use strict";
    videojs(document.getElementsByClassName('vjs-default-skin')[0], {}, function () {});
}