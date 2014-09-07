/* Array utilities */
function inArray(item, array) {
    'use strict';

    return $.inArray(item, array) > -1;
}


function removeFromArray(item, array) {
    'use strict';

    var index = array.indexOf(item);
    array.splice(index, 1);
    return index;
}


/* These specifically manipulate the two global arrays,
courseObjects and selectedLectures. */
function removeCourseObject(courseName) {
    'use strict';

    for (var i = 0; i < courseObjects.length; i++) {
        if (courseName === courseObjects[i].name) {
            courseObjects.splice(i, 1);
            break;
        }
    }
}


function getCourseObject(courseName, courseArray) {
    'use strict';

    for (var i = 0; i < courseArray.length; i++) {
        if (courseArray[i].name === courseName) {
            return courseArray[i];
        }
    }
    return undefined;
}


function getCourse(name) {
    'use strict';

    var course = getCourseObject(name, courseCache);
    if (course === undefined) {
        course = fetchCourse(name);
    }
    return course;
}


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


function convertTimes(times) {
    'use strict';

    var timeList = [];
    var time;

    for(var i = 0; i < times.length; i++) {
        var timeString = 'MTWRF'.charAt(times[i][0]);
        time = times[i][1];
        timeString = timeString + time;
        timeList.push(timeString);
    }

    return timeList;
}