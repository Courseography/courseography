function createModalDiv(id) {
    var div = $("<div></div>");
    var p = $("<p></p>").css("color", "white").html(fetchCourseDescription(id));
    div.append(p);

    var video = $('<video id="example_video_1" class="jj video-js vjs-default-skin" controls preload="auto" width="100%" height="500"></video>');
    var src1 = $("<source></source>").attr("src", "http://video-js.zencoder.com/oceans-clip.webm").attr("type", "video/webm");
    var src2 = $("<source></source>").attr("src", "http://video-js.zencoder.com/oceans-clip.ogv").attr("type", "video/ogv");
    var src3 = $("<source></source>").attr("src", "http://video-js.zencoder.com/oceans-clip.mp4").attr("type", "video/mp4");
    video.append(src1).append(src2).append(src3);
    div.append(video);
    return div;
}


function displayToolTip(nodeId) {
    var rectObject = $("#" + nodeId).find("rect");
    var xPos = rectObject.attr("x") - 65;
    var yPos = rectObject.attr("y");
    createRect(nodeId, "node-tooltip", nodeId + "-tooltip", xPos, yPos, 60, 30, "black");
}


/**
 * Creates an svg rect and appends it to #graph0.
 * @param posX The x position of the rect.
 * @param posY The y position of the rect.
 * @param width The width of the rect.
 * @param height The height of the rect.
 * @param color The fill and stroke color of the rect.
 */
function createRect(nodeId, rectClass, rectId, posX, posY, width, height, color) {
    "use strict";
    console.log(d3.select("#" + rectId));
    d3.select('#graphRootSVG').append('rect')
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
        .duration(5000)
        .ease('cube')
        .attr("fill-opacity", 1)
        .attr("stroke-opacity", 1);

    d3.select('#graphRootSVG').append("text").text("Info")
        .attr("class", rectClass + "-text " + rectId + "-text")
        .attr("id", rectId + "-text")
        .attr("x", parseFloat(posX) + 20)
        .attr("y", parseFloat(posY) + 20)
        .attr("fill-opacity", 0)
        .attr("stroke-opacity", 0)
        .style("cursor", "pointer")
        .on("click", function() {
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
                    close: function() {
                        $(this).remove();
                        $.each(nodes, function(index, elem) {
                            window[elem].updateSVG();
                        });
                    }});
            $('.node, .hybrid').attr('data-active', 'unlit');
            setMouseCallbacks();
            videojs(document.getElementsByClassName('jj')[0], {}, function(){
                // Player (this) is initialized and ready.
            });
        })
        .transition()
        .duration(5000)
        .ease('cube')
        .attr("fill-opacity", 1)
        .attr("stroke-opacity", 1);
}