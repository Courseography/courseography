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

    $('#FCEcount').html(totalFCEs.toFixed(1));

}


/**
 * Updates structures used to measure clicked courses.
 * Note: Not called on hybrids.
 * @param {string} name The name of the clicked course.
 * @param {boolean} active Whether the course is active.
 */
function updateClickedCourses(name, active) {
    'use strict';

    var diff = (name === 'CSC200' ||
                name === 'Calc1') ? 1 : 0.5; // Full-year
    if (!active) {
        diff *= -1;
    }

    if (math.indexOf(name) > -1) {
        currentFCEsMAT += diff;
    } else if (name.charAt(3) === '1') {
        currentFCEs100 += diff;
    } else if (name.charAt(3) === '2') {
        currentFCEs200 += diff;
    } else if (name.charAt(3) === '3') {
        currentFCEs300 += diff;
    } else if (name.charAt(3) === '4') {
        currentFCEs400 += diff;
    }
}
