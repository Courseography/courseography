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
 *
 * @param item
 * @param array
 * @returns {number|Number}
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
 *
 * @param courseName
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
 *
 * @param courseName
 * @param courseArray
 * @returns {*}
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
 *
 * @param name
 * @returns {*}
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
 *
 * @param name
 * @returns {*}
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
 *
 * @param times
 * @returns {Array}
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
 *
 * @param section
 * @returns {boolean}
 */
function hasManualPractical(section) {
    'use strict';

    return (section[0].charAt(0) === 'P');
}


// Used to determine if course requires manual tutorial enrolment
/**
 *
 * @param section
 * @returns {boolean}
 */
function hasManualTutorial(section) {
    'use strict';

    return (section[0].charAt(0) === 'T');
}