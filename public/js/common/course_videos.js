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
