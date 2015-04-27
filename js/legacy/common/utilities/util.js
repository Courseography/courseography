'use strict';

import $ from 'jquery';

/* Array utilities */
/**
 * Returns whether item is in array.
 * @param item The item being searched for in array.
 * @param array The array that is being searched.
 * @returns {boolean} Whether item is in array.
 * TODO: This function needs to be removed.
 */
export function inArray(item, array) {
    'use strict';

    return $.inArray(item, array) > -1;
}


/**
 * Removes item from array.
 * @param {*} item The item being removed.
 * @param {Array} array The array from which the item is being removed.
 * @returns {number} The index of item in array before item was removed.
 */
export function removeFromArray(item, array) {
    'use strict';

    var index = array.indexOf(item);
    array.splice(index, 1);
    return index;
}


/* These specifically manipulate the two global arrays,
courseObjects and selectedSections. */
/**
 * Removes a Course with courseName from courseObjects.
 * @param {string} courseName The Course code.
 */
export function removeCourseObject(courseName) {
    'use strict';

    for (var i = 0; i < courseObjects.length; i++) {
        if (courseName === courseObjects[i].name) {
            courseObjects.splice(i, 1);
            break;
        }
    }
}


/**
 * Gets a course JSON object with courseName from courseArray.
 * @param {string} courseName The course's course code.
 * @param {Array} courseArray The array from which the Course is retrieved.
 * @returns {JSON} The retrieved course.
 */
export function getCourseObject(courseName, courseArray) {
    'use strict';

    for (var i = 0; i < courseArray.length; i++) {
        if (courseArray[i].name === courseName) {
            return courseArray[i];
        }
    }
    return undefined;
}


/**
 * Gets a course.
 * @param {string} name The course's course code.
 * @returns {JSON} The course being retrieved.
 * TODO: Perhaps clean up this function a bit.
 */
export function getCourse(name) {
    'use strict';
    var course = getCourseObject(name, courseCache);
    if (course === undefined) {
        course = fetchCourse(name);
    }
    return course;
}


/**
 * Retrieves a course from file.
 * @param {string} courseName The course code. This + '.txt' is the name of the file.
 * @returns {undefined|JSON} The JSON object representing the course.
 */
export function fetchCourse(courseName) {
    'use strict';

    var course;
    $.ajax({
        url: 'course',
        dataType: 'json',
        data: {name : courseName},
        async: false,
        success: function (data) {
            course = data;
        },
        error: function () {
            throw 'No course file';
        }
    });

    courseCache.push(course);
    return course;
}


/**
 * Converts times from format used in file to format used in td IDs.
 * @param {string[]} times The times to be converted.
 * @returns {string[]} The converted times.
 */
export function convertTimes(times) {
    'use strict';

    var timeList = [];
    var time;
    var stime;

    for (var i = 0; i < times.length; i++) {
        var timeString = 'MTWRF'.charAt(times[i][0]);
        time = times[i][1];

        if (time.charAt(time.length - 1) === '0') {
            if (i === times.length-1) {
                timeString = timeString + time + 'E';
                timeList.push(timeString);
            } else {
                stime = time.replace('-0', '-5');
                if (times[i+1][1] === stime) {
                    timeString = timeString + time;
                    timeList.push(timeString);
                } else {
                    timeString = timeString + time + 'E';
                    timeList.push(timeString);
                }

            }
        } else {
            if (i === 0) {
                timeString = timeString + time + 'H';
                timeList.push(timeString);
            } else {
                stime = time.replace('-5', '-0');
                if (times[i-1][1] !== stime) {
                    timeString = timeString + time + 'H';
                    timeList.push(timeString);
                }
            }
        }
    }

    return timeList;
}

/**
 * Extracts the optional time codes to be at the end.
 * @param {string[]} times The times to be converted.
 * @returns {string[]} The converted times.
 */
export function cleanUpTimes(times) {
    'use strict';

    var timeList = [];
    var timeString;
    var n;
    var m;

    for (var i = 0; i < times.length; i++) {

        n = times[i].indexOf('H');
        m = times[i].indexOf('E');

        if (n !== -1) {
            timeString = times[i].slice(0,n) + times[i].slice(n+1) + 'H';
        } else if (m !== -1) {
            timeString = times[i].slice(0,m) + times[i].slice(m+1) + 'E';
        } else {
            timeString = times[i];
        }

        timeList.push(timeString);
    }

    return timeList;
}

/**
 * Returns whether the url exists.
 * @param {string} url The URL.
 * @returns {boolean} Whether the url exists.
 */
export function urlExists(url) {
    'use strict';

    var exists;
    $.ajax({
        type: 'HEAD',
        async: false,
        url: url,
        success: function (){
            exists = true;
        },
        error: function () {
            exists = false;
        }
    });

    return exists;
}
