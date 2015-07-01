/**
 * Gets the video URLs of the courses associated with names.
 * @param names The list of names of a particular course.
 * @returns {Array} The video URLs associated with the course.
 */
function getCourseVideoUrls(names) {
    'use strict';

    var courseVideoUrls = [];

    $.each(names, function (i, name) {
        var course = getCourse(name);
        courseVideoUrls = courseVideoUrls.concat(course.videoUrls);
    });
    return courseVideoUrls;
}


/**
 * Displays the course video.
 * @param courseVideoUrls The video URLs associated with the course.
 * @param contentDiv The modal div.
 */
function displayCourseVideo(courseVideoUrls, contentDiv) {
    'use strict';

    if (courseVideoUrls.length > 0) {
        // Only display first video right now
        var videoDiv = setupVideoPlayer(courseVideoUrls[0]);
        contentDiv.append(videoDiv);
    }
}


/**
 * Sets up and returns the video player, false if the video URL does not exist.
 * @param {string} url The video url.
 * @returns {jQuery}
 */
function setupVideoPlayer(url) {
    'use strict';

    // Not divided up into 'attr' yet because 'controls preload'
    // cannot be added that way.
    var videoDiv = $('<div id="course-video-div"></div>');
    var video = $('<video id="course-video" class="video-js vjs-default-skin"' +
        'controls preload="auto"></video>');
    var src = $('<source></source>').attr('src', url)
        .attr('type', 'video/mp4');

    video.append(src);
    videoDiv.append(video);

    return videoDiv;
}


/**
 * Enables VideoJS.
 * Note: Must be called after the video player is set up.
 */
function enableVideoJS() {
    'use strict';

    if (document.getElementsByClassName('vjs-default-skin').length > 0) {
        videojs(document.getElementsByClassName('vjs-default-skin')[0], {},
            function () {});
    }
}
