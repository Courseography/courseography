/**
 * Updates and displays total number of selected FCEs.
 */
function updateFCECount() {
    'use strict';

    totalFCEs = 0;

    currentFCEs = currentFCEs100 + currentFCEs200 + currentFCEs300 + currentFCEs400 + currentFCEsMAT;
    setCookie(getCookie('active-graph') + '-fce', currentFCEs);

    for (var i = 0; i < graphs.length; i++) {
        var fce = getCookie(graphs[i].gId + '-fce');
        if (fce === '') {
            fce = 0.0;
        }
        totalFCEs += parseFloat(fce);
    }

}


/**
 * Updates structures used to measure clicked courses.
 * Note: Not called on hybrids.
 * @param {string} courseCode The course code of the clicked course.
 * @param {boolean} active Whether the course is active.
 */
function updateClickedCourses(courseCode, active) {
    'use strict';

    var weight = getCourseFCECount(courseCode);

    if (!active) {
        diff *= -1;
    }

    if (math.indexOf(courseCode) > -1) {
        currentFCEsMAT += weight;
    } else if (courseCode.charAt(3) === '1') {
        currentFCEs100 += weight;
    } else if (courseCode.charAt(3) === '2') {
        currentFCEs200 += weight;
    } else if (courseCode.charAt(3) === '3') {
        currentFCEs300 += weight;
    } else if (courseCode.charAt(3) === '4') {
        currentFCEs400 += weight;
    }
}


/**
 * Gets the number of FCEs the course with the course code courseCode is worth.
 */
function getCourseFCECount(courseCode) {
    'use strict';

    return (name === 'CSC200' || name === 'Calc1') ? 1 : 0.5;
}

