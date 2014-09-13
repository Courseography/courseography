/* global $ */

// TODO: File too long
// TODO: Bad function names\

/**
 * Updates the POSt requirements that correspond with a certain course.
 * @param {string} course The course code.
 * @param {boolean} active Whether this course is selected.
 */
function updatePOSt(course, active) {
    'use strict';

    if (reqs.indexOf(course) > -1) { // Required course
        $('#' + course + 'check').prop('checked', active);
        $('#' + course + 'checkMajor').prop('checked', active);
    } else {
        var ind;
        if (course.substr(0, 5) === 'CSC49') {
            ind = projectCourses.indexOf(course);
            if (active && ind === -1) {
                projectCourses.push(course);
            } else if (!active && ind > -1) {
                projectCourses.splice(ind, 1);
            }
        } else if (course.substr(0, 4) === 'CSC4' ||
                   course.substr(0, 4) === 'ECE4') { // 4th year course
            ind = active400s.indexOf(course);
            if (active && ind === -1) {
                active400s.push(course);
            } else if (!active && ind > -1) {
                active400s.splice(ind, 1);
            }
        } else if (course.substr(0, 4) === 'CSC3' ||
                   course.substr(0, 4) === 'ECE3') { // 3rd year course
            ind = active300s.indexOf(course);
            if (active && ind === -1) {
                active300s.push(course);
            } else if (!active && ind > -1) {
                active300s.splice(ind, 1);
            }
        } else if (course.substr(0, 4) === 'CSC2') { // 2nd year course
            ind = active200s.indexOf(course);
            if (active && ind === -1) {
                active200s.push(course);
            } else if (!active && ind > -1) {
                active200s.splice(ind, 1);
            }
        }

        if (CSCinq.indexOf(course) > -1) {
            $('#' + course + 'check').prop('checked', active);
            $('#' + course + 'checkMajor').prop('checked', active);
            $('#' + course + 'checkMinor').prop('checked', active);
        }
    }

    $('#' + course + 'check').prop('checked', active);
    $('#' + course + 'checkMajor').prop('checked', active);
    $('#' + course + 'checkMinor').prop('checked', active);
}


/**
 * Updates the 'Specialist' POSt interface.
 */
function updatePostInterface() {
    'use strict';

    updateCSCReqs();
    updateMATReqs();
    updateCSC400s();
    updateElecs();
    updatePEY();
    updatePOStTotal();

    setIcon('specCheck',
        cscReqSat && matReqSat && elec400sSat && elecSat && peySat);
}


/**
 * Updates the 'Major' POSt interface.
 */
function updateMajorPostInterface() {
    'use strict';

    updateCSCReqsMajor();
    updateMATReqsMajor();
    updateElecsMajor();
    updatePEYMajor();
    updatePOStTotalMajor();

    setIcon('majorCheck',
        cscReqSatMajor && matReqSatMajor && elecSatMajor && peySatMajor);
}


/**
 * Updates the 'Minor' POSt interface.
 */
function updateMinorPostInterface() {
    'use strict';

    updateCSCReqsMinor();
    updateElecsMinor();

    setIcon('minorCheck',
        cscReqSatMinor && elecSatMinor);
}


/**
 * Sets the complete/incomplete icon.
 * @param {string} id The ID of the img container.
 * @param {boolean} sat Whether the complete icon should be displayed.
 */
function setIcon(id, sat) {
    'use strict';

    if (sat) {
        $('a[href="#' + id + '"] img').attr('src', 'res/ico/check.ico');
    } else {
        $('a[href="#' + id + '"] img').attr('src', 'res/ico/delete.ico');
    }
}


/**
 * Updates the specialist CSC requirements.
 */
function updateCSCReqs() {
    'use strict';

    cscReqTotal = $('#cscReqs input:checkbox:checked').length / 2;
    $('#cscReqTotal').html(cscReqTotal.toFixed(1));
    cscReqSat = cscReqTotal >= 5;
    setIcon('cscReqs', cscReqSat);
}


/**
 * Updates the major CSC requirements.
 */
function updateCSCReqsMajor() {
    'use strict';

    cscReqTotalMajor = $('#cscReqsMajor input:checkbox:checked').length / 2;
    $('#cscReqTotalMajor').html(cscReqTotal.toFixed(1));
    cscReqSatMajor = cscReqTotalMajor >= 3.5;
    setIcon('cscReqsMajor', cscReqSatMajor);
}


/**
 * Updates the minor CSC requirements.
 */
function updateCSCReqsMinor() {
    'use strict';

    cscReqTotalMinor = $('#cscReqsMinor input:checkbox:checked').length / 2;

    if (cscReqTotalMinor > 2.5) {
        cscReqTotalMinor = 2.5;
    }

    $('#cscReqTotalMinor').html(cscReqTotalMinor.toFixed(1));
    cscReqSatMinor = cscReqTotalMinor >= 2.5;
    setIcon('cscReqsMinor', cscReqSatMinor);
}


/**
 * Updates the specialist MAT requirements.
 */
function updateMATReqs() {
    'use strict';

    matReqTotal = $('#matReqs input:checkbox:checked').length / 2;
    if ($('#Calc1check').prop('checked')) {
        matReqTotal += 0.5;
    }
    $('#matReqTotal').html(matReqTotal.toFixed(1));
    matReqSat = matReqTotal >= 2;
    setIcon('matReqs', matReqSat);
}


/**
 * Updates the major MAT requirements.
 */
function updateMATReqsMajor() {
    'use strict';

    matReqTotalMajor = $('#matReqsMajor input:checkbox:checked').length / 2;
    if ($('#Calc1checkMajor').prop('checked')) {
        matReqTotalMajor += 0.5;
    }
    $('#matReqTotalMajor').html(matReqTotalMajor.toFixed(1));
    matReqSatMajor = matReqTotalMajor >= 1.5;
    setIcon('matReqsMajor', matReqSatMajor);
}


/**
 * Updates all electives for the minor.
 */
function updateAllElecsMinor() {
    'use strict';

    var numProjects = 2;
    var active300sMinor = active300s.concat();
    var tmpMinor = active300s.concat(active400s, projectCourses.slice(0, numProjects));


    if (elecTotalMinor >= 1.5) {
        elecTotalMinor = 1.5;
    }

    if (CSC373.status === 'active' || CSC373.status === 'overridden') {
        active300sMinor.push('CSC373');
        tmpMinor.push('CSC373');
        extraMinor += 0.5;
    }

    if (CSC369.status === 'active' || CSC369.status === 'overridden') {
        active300sMinor.push('CSC369');
        tmpMinor.push('CSC369');
        extraMinor += 0.5;
    }

    if (CSC258.status === 'active' || CSC258.status === 'overridden') {
        tmpMinor.push('CSC258');
        extraMinor += 0.5;
    }

    if (CSC209.status === 'active' || CSC209.status === 'overridden') {
        tmpMinor.push('CSC209');
        extraMinor += 0.5;
    }

    if (CSC200.status === 'active' || CSC200.status === 'overridden') {
        tmpMinor.push('CSC200');
        extraMinor += 1;
    }

    for (var i = 1; i <= 7; i++) {
        if (i <= tmpMinor.length) {
            $('#inputMinorCSC' + i).attr('value', tmpMinor[i - 1]);
        } else {
            $('#inputMinorCSC' + i).attr('value', '');
        }
    }

    elecTotalMinor = (tmpMinor.length) / 2;

    if (elecTotalMinor > 1.5) {
        elecTotalMinor = 1.5;
    }

    $('#elecTotalMinor').html(elecTotalMinor.toFixed(1));
    elecSatMinor = (elecTotalMinor >= 1.5) && (1 <= active300sMinor.length + active400s.length + projectCourses.length);
    setIcon('cscElecsMinor', elecSatMinor);
}


/**
 * Updates minor electives.
 */
function updateElecsMinor() {
    'use strict';

    updateAllElecsMinor();
//    var numProjects = 2;
    postTotalMinor = elecTotalMinor + cscReqTotalMinor;
    //elecSatMinor = elecTotalMinor >= 1.5
    //  && (active300s.length + active400s.length + projectCourses.length > 0)
    //  && (active300s.length + active400s.length + projectCourses.length < 3)
    //  && (active300s.concat(active400s, projectCourses.slice(0, numProjects)).length + extraMinor >= 3);

    $('#postTotalMinor').html(postTotalMinor.toFixed(1));

    setIcon('ElecsMinor', elecSatMinor);
}


/**
 * Updates the major 200 series electives.
 */
function update200sElecsMajor() {
    'use strict';

    elec200sTotalMajor = 0;
    if ($('#Calc2checkMajor').prop('checked')) {
        elec200sTotalMajor += 1;
    }
    if ($('#CSC200checkMajor').prop('checked')) {
        elec200sTotalMajor += 1;
    }
    if ($('#CSC209checkMajor').prop('checked')) {
        elec200sTotalMajor += 0.5;
    }
    if ($('#Lin1checkMajor').prop('checked')) {
        elec200sTotalMajor += 0.5;
    }

    if (elec200sTotalMajor > 1) {
        elec200sTotalMajor = 1;
    }
}


/**
 * Updates the major 300 series electives.
 */
function update300sElecsMajor() {
    'use strict';

    numBCBMajor = $('#300sElecsMajor input:checkbox:checked').length;
    var numProjects = 2;
    if ($('#BCB430checkMajor').prop('checked')) {
        numBCBMajor += 1;
        numProjects = 0;
    }

    var tmp = active300s.concat(active400s, projectCourses.slice(0, numProjects));
    // Manually add active 3rd year courses required by specialist
    if (CSC373.status === 'active' || CSC373.status === 'overridden') {
        tmp.push('CSC373');
        extraMajor += 0.5;
    }

    if (CSC369.status === 'active' || CSC369.status === 'overridden') {
        tmp.push('CSC369');
        extraMajor += 0.5;
    }

    for (var i = 1; i <= 6; i++) {
        if (i <= tmp.length) {
            $('#3xx' + i + 'Major').attr('value', tmp[i - 1]);
        } else {
            $('#3xx' + i + 'Major').attr('value', '');
        }
    }


    elec300sTotalMajor = tmp.length + numBCBMajor;

    for (var i = 1; i <= 3; i++) {
        if ($('#MAT' + i + 'Major').prop('value').substr(0, 3) === 'MAT') {
            elec300sTotalMajor += 1;
        }
    }

    elec300sTotalMajor /= 2;
}


/**
 * Updates the major electives.
 */
function updateElecsMajor() {
    'use strict';

    update200sElecsMajor();
    update300sElecsMajor();

    elecTotalMajor = elec200sTotalMajor + elec300sTotalMajor;
    if (elecTotalMajor >= 3) {
        elecTotalMajor = 3;
    }

    var numProjects = 2;
    if ($('#BCB430check').prop('checked')) {
        numProjects = 0;
    }

    elecSatMajor = elecTotalMajor >= 3 && (active400s.length > 0 || numBCBMajor > 0) && (active300s.concat(active400s, projectCourses.slice(0, numProjects)).length + extraMajor >= 3);

    $('#elecTotalMajor').html(elecTotalMajor.toFixed(1));

    setIcon('200sElecsMajor', elecSatMajor);
    setIcon('300sElecsMajor', elecSatMajor);
}


/**
 * Updates 400 series CSC courses.
 */
function updateCSC400s() {
    'use strict';

    numBCB = $('#csc400s input:checkbox:checked').length;

    var numProjects = 2;
    if ($('#BCB430check').prop('checked')) {
        numBCB += 1;
        numProjects = 0;
    }

    var tmp = active400s.concat(projectCourses.slice(0, numProjects));

    for (var i = 1; i <= 3; i++) {
        if (i <= 3 - numBCB && i <= tmp.length) {
            $('input#4xx' + i).attr('value', tmp[i - 1]);
        } else {
            $('input#4xx' + i).attr('value', '');
        }
    }

    elec400sSat = numBCB + tmp.length >= 3;
    setIcon('csc400s', elec400sSat);
}


/**
 * Updates specialist electives.
 * Note: Must be called after updateCSC400s because of numBCB.
 */
function updateElecs() {
    'use strict';

    var BCB430Check = $('#BCB430check');

    var numProjects = 2;
    if (BCB430Check.prop('checked')) {
        numProjects = 0;
    }
    var tmp = active300s.concat(active400s.slice(3 - numBCB), projectCourses.slice(Math.max(3 - numBCB - active400s.length, 0), numProjects));
    for (var i = 1; i <= 7; i++) {
        if (i <= tmp.length) {
            $('#inputCSC' + i).attr('value', tmp[i - 1]);
        } else {
            $('#inputCSC' + i).attr('value', '');
        }
    }

    var matElecs = 0;

    $('#matElecs input:text').each(function () {
        if (this.value === 'MAT235' || this.value === 'MAT237' ||
            this.value === 'MAT257' || this.value === 'MAT235Y1' ||
            this.value === 'MAT237Y1' || this.value === 'MAT257Y1') {
            matElecs += 1;
        } else if (this.value.substr(0, 3) === 'MAT' ||
                   this.value.substr(0, 3) === 'STA') {
            matElecs += 0.5;
        }
    });

    elecTotal = (active300s.length + active400s.length + numBCB) / 2 + matElecs;
    if (!BCB430Check.prop('checked')) {
        elecTotal += Math.min(projectCourses.length / 2, 1);
    }
    if (elecTotal >= 5) {
        elecTotal = 5;
    }

    $('#elecTotal').html(elecTotal.toFixed(1));
    elecSat = elecTotal === 5;
    setIcon('cscElecs', elecSat);
    setIcon('matElecs', elecSat);
}


/**
 * Updates the major PEY requirement.
 */
function updatePEYMajor() {
    'use strict';

    peySatMajor = $('#peycheckMajor').prop('checked') ||
                  $('#peyReqMajor input:checkbox:checked').length > 0;
    setIcon('peyReqMajor', peySatMajor);
}


/**
 * Updates the specialist PEY requirement.
 */
function updatePEY() {
    'use strict';

    peySat = $('#peycheck').prop('checked') ||
             $('#peyReq input:checkbox:checked').length > 0;
    setIcon('peyReq', peySat);
}


/**
 * Updates total specialist requirements.
 */
function updatePOStTotal() {
    'use strict';

    $('#postTotal').html((cscReqTotal + matReqTotal + elecTotal).toFixed(1));
}


/**
 * Updates total major requirements.
 */
function updatePOStTotalMajor() {
    'use strict';

    $('#postTotalMajor').html((cscReqTotalMajor + matReqTotalMajor + elecTotalMajor).toFixed(1));
}


/**
 * Updates total major requirements.
 * TODO: Not used?
 */
function updatePostMinor() {
    'use strict';

    $('#postTotalMinor').html((cscReqTotalMinor + elecTotalMinor).toFixed(1));
}
