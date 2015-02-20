/**
 * Updates number of completed courses in Specialist.
**/
function updateCompletedSpecCourses () {
    'use-strict';

    for (var courseCode in completed_spec) {
        if (completed_spec.hasOwnProperty(courseCode)) {
            if (getCookie(courseCode) === 'active' || getCookie(courseCode) === 'overridden') {
                if (completed_spec[courseCode] < 1) {
                    completed_spec[courseCode] += 1;
                    if (courseCode === 'Calc1') {
                        creditCountSpec += 1;
                    } else {
                        creditCountSpec += 0.5;
                    }
                } 
            } else if ((getCookie(courseCode) === 'inactive' || getCookie(courseCode) === 'takeable')
                       && (completed_spec[courseCode] > 0)) {
                    completed_spec[courseCode] -= 1;
                    if (courseCode === 'Calc1') {
                        creditCountSpec -= 1;
                    } else {
                        creditCountSpec -= 0.5;
                    }
            }      
        }
    }
}


/**
 * Updates number of completed courses in Major.
**/
function updateCompletedMajCourses () {
    'use-strict';

    for (var courseCode in completed_maj) {
        if (completed_maj.hasOwnProperty(courseCode)) {
            if (getCookie(courseCode) === 'active' || getCookie(courseCode) === 'overridden') {
                if (completed_maj[courseCode] < 1) {
                    completed_maj[courseCode] += 1;
                    if (courseCode === 'Calc1') {
                        creditCountMaj += 1.0;
                    } else {
                        creditCountMaj += 0.5;
                    }
                } 
            } else if ((getCookie(courseCode) === 'inactive' || getCookie(courseCode) === 'takeable')
                        && (completed_maj[courseCode] > 0)) {
                completed_maj[courseCode] -= 1;
                if (courseCode === 'Calc1') {
                    creditCountMaj -= 1.0;
                } else {
                    creditCountMaj -= 0.5;
                }
            }
        }       
    }
}


/**
 * Updates number of completed courses in Minor.
**/
function updateCompletedMinCourses() {
    'use-strict';

    for (var courseCode in completed_min) {
        if (completed_min.hasOwnProperty(courseCode)) {
            if (getCookie(courseCode) === 'active' || getCookie(courseCode) === 'overridden') {
                if (completed_min[courseCode] < 1) {
                    completed_min[courseCode] += 1;
                    creditCountMin += 0.5;
                } 
            } else if ((getCookie(courseCode) === 'inactive' || getCookie(courseCode) === 'takeable')
                           && (completed_min[courseCode] > 0)) {
                completed_min[courseCode] -= 1;
                creditCountMin -= 0.5;
            }
        }       
    }
}


/**
 * Updates number of 300 level category completed courses.
 **/
function update300s() {
    'use-strict';

    for (var courseCode in level300) {
        if (level300.hasOwnProperty(courseCode)) {
            if (getCookie(courseCode) === 'active' || getCookie(courseCode) === 'overridden') {
                if (level300[courseCode] < 1) {
                    level300[courseCode] += 1;
                    creditCount300 += 0.5;
                } if ((CSCinq.indexOf(courseCode) > -1) && (activeInq.indexOf(courseCode) === -1)) { // check if Inquiry Course
                    activeInq.push(courseCode);
                }
            } else if ((getCookie(courseCode) === 'inactive' || getCookie(courseCode) === 'takeable') 
                       && (level300[courseCode] > 0)) {
                level300[courseCode] -= 1;
                creditCount300 -= 0.5;
                var index = activeInq.indexOf(courseCode);
                if (index > -1) {
                    activeInq.splice(index, 1);
                }
            }
        }       
    }
}


/**
 * Updates number of 400 level category completed courses.
**/
function update400s() {
    'use-strict';

    for (var courseCode in level400) {
        if (level400.hasOwnProperty(courseCode)) {
            if (getCookie(courseCode) === 'active' || getCookie(courseCode) === 'overridden') {
                if (level400[courseCode] < 1) {
                    level400[courseCode] += 1;
                    creditCount400 += 0.5;
                } if ((CSCinq.indexOf(courseCode) > -1) && (activeInq.indexOf(courseCode) === -1)) { // check if Inquiry Course
                    activeInq.push(courseCode);
                }
            } else if ((getCookie(courseCode) === 'inactive' || getCookie(courseCode) === 'takeable') 
                       && (level400[courseCode] > 0)) {
                level400[courseCode] -= 1;
                creditCount400 -= 0.5;
                var index = activeInq.indexOf(courseCode);
                if (index > -1) {
                    activeInq.splice(index, 1);
                }
            }
        }       
    }
}


/**
 * Updates Credit Count for each POSt.
 * TODO: Fix credit count to account for all constraints
 **/
function updateCreditCount() {
    'use-strict';

    // account for not needing to take CSC108
    specCount = creditCountSpec - (completed_spec['CSC108'] * 0.5) + creditCount300 + creditCount400;
    majCount = creditCountMaj - (completed_maj['CSC108'] * 0.5) + creditCount300 + creditCount400;
    minCount = creditCountMin - (completed_min['CSC108'] * 0.5) + creditCount300 + creditCount400;


    if (creditCountSpec >= 12) {
        $('#spec_creds').html('(12/12.0)');
        $('#maj_creds').html('(8/8.0)');
        $('#min_creds').html('(4/4.0)');
    } else if (creditCountSpec >= 8) {
        $('#spec_creds').html('(' + creditCountSpec + '/12.0)');
        $('#maj_creds').html('(8/8.0)');
        $('#min_creds').html('(4/4.0)');
    } else if (creditCountSpec >= 4) {
        $('#spec_creds').html('(' + creditCountSpec + '/12.0)');
        $('#maj_creds').html('(' + creditCountSpec + '/8.0)');
        $('#min_creds').html('(4/4.0)');
    } else {
        $('#spec_creds').html('(' + creditCountSpec + '/12.0)');
        $('#maj_creds').html('(' + creditCountSpec + '/8.0)');
        $('#min_creds').html('(' + creditCountSpec + '/4.0)');
    }
}