/* Array utilities */
/**
 * Returns whether item is in array.
 * @param item The item being searched for in array.
 * @param array The array that is being searched.
 * @returns {boolean} Whether item is in array.
 * TODO: This function needs to be removed.
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

/* URL Utilities */
/**
 * Gets the value of a parameter in the query string by name.
 * @param name The name of the parameter to retrieve.
 * @returns {string|null} The value of the parameter as a string, or null if it does not exist.
 */
function getURLParameter(name) {
    'use strict';

    return decodeURIComponent((new RegExp('[?|&]' + name + '=' + '([^&;]+?)(&|#|;|$)').exec(location.search)||[,""])[1].replace(/\+/g, '%20'))||null;
}


var courseObjects = [];     // All selected course JSON files.


/* These specifically manipulate the two global arrays,
courseObjects and selectedSections. */
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
 * @param {string} courseName The course's course code.
 * @param {Array} courseArray The array from which the Course is retrieved.
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
 * Retrieves a course from file.
 * @param {string} courseName The course code. This + '.txt' is the name of the file.
 * @returns {Promise} Promise object representing the JSON object containing course information.
 */
function getCourse(courseName) {
    'use strict';

    return fetch(
        'course?name=' + courseName,
        )
        .then(response => response.json())
        .catch(error => {
            throw(error);
        });
}


/**
 * Converts times from format used in file to format used in td IDs.
 * @param {object} times The times to be converted.
 * @returns {string[]} The converted times.
 */
function convertTimes(times) {
    'use strict';

    // Need to do some preprocessing on the "Time" objects,
    // which correspond to the "Time" data type in Database/Tables.hs.
    var newTimes = times.map(function(t) {
        var day = t.timeField[0];
        var timeFloat = parseFloat(t.timeField[1]).toFixed(1)
        var time = String(timeFloat).replace('.', '-');
        return [day, time];
    });

    var timeList = [];
    var time;
    var stime;

    for (var i = 0; i < newTimes.length; i++) {
        var timeString = 'MTWRF'.charAt(newTimes[i][0]);
        time = newTimes[i][1];

        if (time.charAt(time.length - 1) === '0') {
            if (i === newTimes.length - 1) {
                timeString = timeString + time + 'E';
                timeList.push(timeString);
            } else {
                stime = time.replace('-0', '-5');
                if (newTimes[i+1][1] === stime) {
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
                if (newTimes[i-1][1] !== stime) {
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
function cleanUpTimes(times) {
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
 * Returns a copy of lectures with duplicate lecture sessions removed, and lectures
 * sorted by section.
 * @param {object[]} lectures The lectures to remove duplicates from.
 * @returns {object[]} The lectures without duplicates.
 */
function removeDuplicateLectures(lectures) {
    'use strict'

    return lectures.filter((lecture, index, lectures) =>
            lectures.map(lect => lect.section).indexOf(lecture.section) === index)
        .sort((lec1, lec2) => lec1.section > lec2.section);
}
