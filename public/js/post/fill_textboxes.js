/**
 * Fills textboxes with 300 level courses.
 * @param {object} post Object corresponding to the POSt being dealt with.
 * @param {HTMLElement[]} postElement Array of textboxes to fill.
 */
function fill300Textboxes(post, postElement) {
    'use strict';

    for (var m = post.index300; m < activeCourses.length &&
        post.filledTextboxes300 !== post.textboxes300; m++) {

        var course = activeCourses[m];

        if (course.indexOf('CSC3') !== -1 &&
            (post.name === 'major' ||
             (post.name === 'specialist' && notSpecialistCourse(course)))) {
            postElement[post.filledTextboxes300].value = activeCourses[m];
            post.index300 = m + 1;
            post.filledTextboxes300 += 1;
            post.creditCount += 0.5;
        }
    }
}


/**
 * Fills textboxes with 400 level courses.
 * @param {object} post Object corresponding to the POSt being dealt with.
 * @param {HTMLElement[]} postElement Array of textboxes to fill.
 * @param {string} category Level of category we are filling.
 */
function fill400Textboxes(post, postElement, category) {
    'use strict';

    for (var m = post.index400; m < activeCourses.length &&
        post['filledTextboxes' + category] !== post['textboxes' + category]; m++) {
        var course = activeCourses[m];
        if (course.indexOf('CSC4') !== -1) {
            postElement[post['filledTextboxes' + category]].value = activeCourses[m];
            post.index400 = m + 1;
            post['filledTextboxes' + category] += 1;
            post.creditCount += 0.5;
        }
    }
}


/**
 * Autofills textboxes for 300 level courses.
 */
function fill300s() {
    'use strict';

    var spec300s = $('#spec300')[0].getElementsByTagName('input');
    var maj300s = $('#maj300')[0].getElementsByTagName('input');

    // Clear textboxes
    for (var i = 0; i < 3; i++) {
        spec300s[i].value = '';
        spec300s[i].disabled = true;
        if (i < 2) {
            maj300s[i].value = '';
            maj300s[i].disabled = true;
        }
    }

    // Fill courses that have been selected
    fill300Textboxes(specialist, spec300s);
    fill300Textboxes(major, maj300s);

    if (specialist.filledTextboxes300 < specialist.textboxes300) {
        fill400Textboxes(specialist, spec300s, '300');
    }
    if (major.filledTextboxes300 < major.textboxes300) {
        fill400Textboxes(major, maj300s, '300');
    }
}


/**
 * Autofills textboxes for 400 level courses.
 */
function fill400s() {
    'use strict';

    var spec400s = $('#spec400')[0].getElementsByTagName('input');
    var maj400s = $('#maj400')[0].getElementsByTagName('input');

    // Clear textboxes
    for (var i = 0; i < 3; i++) {
        spec400s[i].value = '';
        spec400s[i].disabled = true;
        if (i < 1) {
            maj400s[i].value = '';
            maj400s[i].disabled = true;
        }
    }

    // Fill courses that have been selected
    fill400Textboxes(specialist, spec400s, '400');
    fill400Textboxes(major, maj400s, '400');
}


/**
 * Fills textboxes in Extra level category.
 * @param {object} post Object corresponding to the POSt being dealt with.
 * @param {HTMLElement[]} postElement Array of textboxes to fill.
 * @param {string} level Level of course that we are filling textbox with.
 */
function fillExtraTextboxes(post, postElement, level) {
    'use strict';

    for (var i = post['index' + level]; i < activeCourses.length &&
        post.filledTextboxesExtra !== post.textboxesExtra; i++) {

        var course = activeCourses[i];

        if (postElement[post.filledTextboxesExtra].value === '' &&
            course.indexOf('CSC' + level.charAt(0)) != -1 &&
            (post.name === 'major' || post.name === 'minor' ||
             (post.name === 'specialist' && notSpecialistCourse(course))) &&
            notReqCourse(post, course)) {

            postElement[post.filledTextboxesExtra].value = activeCourses[i];
            postElement[post.filledTextboxesExtra].disabled = true;
            post['index' + level] = i;
            post.filledTextboxesExtra += 1;
            post.creditCount += 0.5;
        } else if (postElement[post.filledTextboxesExtra].value !== '') {
            post.filledTextboxesExtra += 1;
        }
    }
}


/**
 * Autofills the textboxes for the extra 300+ credits category
 */
function fillExtra() {
    'use strict';

    var specExtra = $('#specextra')[0].getElementsByTagName('input');
    var majExtra = $('#majextra')[0].getElementsByTagName('input');
    var minExtra = $('#minextra')[0].getElementsByTagName('input');

    for (var k = 0; k < 4; k++) {

        // Clear text boxes
        if (specExtra[k].value.indexOf('MAT') === -1 && specExtra[k].value.indexOf('STA') === -1) {
            specExtra[k].value = '';
            specExtra[k].disabled = false;
        }
        if (k < 3) {
            if (majExtra[k].value.indexOf('MAT') === -1 && majExtra[k].value.indexOf('STA') === -1) {
                majExtra[k].value = '';
                majExtra[k].disabled = false;
            }
            minExtra[k].value = '';
            minExtra[k].disabled = true;
        }
    }

    // Fill courses that have been selected
    fillExtraTextboxes(specialist, specExtra, '300');
    fillExtraTextboxes(major, majExtra, '300');
    fillExtraTextboxes(minor, minExtra, '300');

    if (specialist.filledTextboxesExtra < specialist.textboxesExtra) {
        fillExtraTextboxes(specialist, specExtra, '400');
    }
    if (major.filledTextboxesExtra < major.textboxesExtra) {
        fillExtraTextboxes(major, majExtra, '400');
    }
    if (minor.filledTextboxesExtra < minor.textboxesExtra) {
        fillExtraTextboxes(minor, minExtra, '400');
    }

    // add 200-level extra courses if there is extra space
    if (major.filledTextboxesExtra < major.textboxesExtra) {
        fillExtraTextboxes(major, majExtra, '200');
    }
    if (minor.filledTextboxesExtra < minor.textboxesExtra) {
        fillExtraTextboxes(minor, minExtra, '200');
    }
}


/**
 * Autofills textboxes and updates category for Inquiry courses.
 */
function fillMisc() {
    'use strict';

    var specInq = $('#spec_misc')[0].getElementsByTagName('input');
    var majInq = $('#maj_misc')[0].getElementsByTagName('input');

    // Clear textboxes
    if (specInq[0].value.indexOf('PEY') === -1) {
        specInq[0].value = '';
        specInq[0].disabled = false;
    } else {
        specialist.activeInq = 1;
    }

    if (majInq[0].value.indexOf('PEY') === -1) {
        majInq[0].value = '';
        majInq[0].disabled = false;
    } else {
        major.activeInq = 1;
    }

    // Fill active inquiry course
    for (var i = 0; i < activeCourses.length; i++) {
        if (CSCinq.indexOf(activeCourses[i]) !== -1) {
            specInq[0].value = activeCourses[i];
            specialist.activeInq = 1;
            specInq[0].disabled = true;
            majInq[0].value = activeCourses[i];
            major.activeInq = 1;
            majInq[0].disabled = true;
            break;
        }
    }

    // Update category
    updateInqCategory();
}
