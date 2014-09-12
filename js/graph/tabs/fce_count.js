// Update the total FCE count, and display total
/**
 *
 */
function updateFCECount() {
    'use strict';

    FCEs = FCEs100 + FCEs200 + FCEs300 + FCEs400 + FCEsMAT;
    $('#FCEcount').html(FCEs.toFixed(1));
}

// Note: not called on hybrids
/**
 *
 * @param {string} name
 * @param {boolean} active
 */
function updateClickedCourses(name, active) {
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