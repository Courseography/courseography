/* Array utilities */
/**
 *
 * @param item
 * @param array
 * @returns {boolean}
 */
function inArray(item, array) {
    'use strict';

    return $.inArray(item, array) > -1;
}


/**
 * Removes item from array.
 * @param {*} item The item being removed.
 * @param {Array} array The array from which the item is being removed.
 * @returns {number} The index of item in array before item was removed.
 */
function removeFromArray(item, array) {
    'use strict';

    var index = array.indexOf(item);
    array.splice(index, 1);
    return index;
}


/* These specifically manipulate the two global arrays,
courseObjects and selectedLectures. */
/**
 * Removes a Course with courseName from courseObjects.
 * @param {string} courseName The Course code.
 */
function removeCourseObject(courseName) {
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
 * @param courseName The course's course code.
 * @param courseArray The array from which the Course is retrieved.
 * @returns {JSON} The retrieved course.
 */
function getCourseObject(courseName, courseArray) {
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
function getCourse(name) {
    'use strict';

    var course = getCourseObject(name, courseCache);
    if (course === undefined) {
        course = fetchCourse(name);
    }
    return course;
}


/**
 * Retrieves a course from file.
 * @param name The course code. This + '.txt' is the name of the file.
 * @returns {undefined|JSON} The JSON object representing the course.
 */
function fetchCourse(name) {
    'use strict';

    var course;
    $.ajax({
        url: 'res/courses/' + name + '.txt',
        dataType: 'json',
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
 * @param {string[]}times The times to be converted.
 * @returns {string[]} The converted times.
 */
function convertTimes(times) {
    'use strict';

    var timeList = [];
    var time;

    for (var i = 0; i < times.length; i++) {
        var timeString = 'MTWRF'.charAt(times[i][0]);
        time = times[i][1];
        timeString = timeString + time;
        timeList.push(timeString);
    }

    return timeList;
}


// Used to determine if course requires manual practical enrolment
/**
 * Returns whether section has manual practical enrolment. (P sections).
 * @param {JSON} section The section.
 * @returns {boolean} Whether section has manual practical enrolment.
 */
function hasManualPractical(section) {
    'use strict';

    return (section[0].charAt(0) === 'P');
}


// Used to determine if course requires manual tutorial enrolment
/**
 * Returns whether section has manual tutorial enrolment. (T sections).
 * @param {JSON} section The section.
 * @returns {boolean} Whether section has manual tutorial enrolment.
 */
function hasManualTutorial(section) {
    'use strict';

    return (section[0].charAt(0) === 'T');
}