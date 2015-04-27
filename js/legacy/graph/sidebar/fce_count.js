'use strict';

import $ from 'jquery';

/**
 * Updates and displays total number of selected FCEs.
 */
export function updateFCECount() {
    'use strict';

    FCEs = FCEs100 + FCEs200 + FCEs300 + FCEs400 + FCEsMAT;
    $('#FCEcount').html(FCEs.toFixed(1));
}


/**
 * Updates structures used to measure clicked courses.
 * Note: Not called on hybrids.
 * @param {string} name The name of the clicked course.
 * @param {boolean} active Whether the course is active.
 */
export function updateClickedCourses(name, active) {
    'use strict';

    var diff = (name === 'CSC200' ||
                name === 'Calc1') ? 1 : 0.5; // Full-year
    if (!active) {
        diff *= -1;
    }

    if (math.indexOf(name) > -1) {
        FCEsMAT += diff;
    } else if (name.charAt(3) === '1') {
        FCEs100 += diff;
    } else if (name.charAt(3) === '2') {
        FCEs200 += diff;
    } else if (name.charAt(3) === '3') {
        FCEs300 += diff;
    } else if (name.charAt(3) === '4') {
        FCEs400 += diff;
    }
}
