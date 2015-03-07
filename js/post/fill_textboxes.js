
function fill300Textboxes(post, postElement, category) {
    'use strict';
    //var postObject = window[post];

    for (var m = window[post].index300 + 1; m < activeCourses.length; m++) {
        if (window[post].index300 === activeCourses.length || window[post].filledTextboxes300 === window[post].textboxes300) {
            break;
        }
        var course = activeCourses[m];
        if (course.indexOf('CSC3') != -1 && course.indexOf('CSC373') === -1 && course.indexOf('CSC369') === - 1) {
            postElement[window[post].filledTextboxes300].value = activeCourses[m];
            //postElement[i].disabled = true;
            window[post].index300 = m;
            window[post]['filledTextboxes' + category] += 1;
            creditCount300and400.spec += 0.5;
        }
    }
}


function fill400Textboxes(post, postElement, category) {
    'use strict';

    //var postObject = window[post];

    for (var m = window[post].index400 + 1; m < activeCourses.length; m++) {
        if (window[post].index400 === activeCourses.length || window[post].filledTextboxes400 === window[post].textboxes400) {
            break;
        }
        var course = activeCourses[m];
        if (course.indexOf('CSC4') != -1) {
            postElement[window[post].filledTextboxes400].value = activeCourses[m];
            //postElement[i].disabled = true;
            window[post].index400 = m;
            window[post]['filledTextboxes' + category] += 1;
            creditCount300and400.spec += 0.5;
        }
    }
}

/**
 * Autofills textboxes for 300 level courses. 
**/
function fill300s() {
    'use strict';

    var spec300s = $('#spec300')[0].getElementsByTagName('input');
    var maj300s = $('#maj300')[0].getElementsByTagName('input');
    //var min300s = $('#minextra')[0].getElementsByTagName('input');

    
    // clear textboxes
    for (var k = 0; k < 3; k++) {
        spec300s[k].value = '';
        spec300s[k].disabled = true;
        if (k < 2) {
            maj300s[k].value = '';
            maj300s[k].disabled = true;
        }
        /*
        if (min300s[k].value.indexOf('CSC4') === -1) {
            min300s[k].value = '';
            min300s[k].disabled = true;
        }
        */
    }
    
    // fill courses that have been selected
    fill300Textboxes('specialist', spec300s, '300');
    fill300Textboxes('major', maj300s, '300');
    //fill300Textboxes('minor', min300s, '300');

    if (specialist.filledTextboxes300 < specialist.textboxes300) {
        fill400Textboxes('specialist', spec300s, '300');
    }
    if (major.filledTextboxes300 < major.textboxes300) {
        fill400Textboxes('major', maj300s, '300');
    }
    /*if (minor.filledTextboxesExtra < specialist.textboxesExtra) {
        fill400Textboxes('minor', min300s, 'Extra');
    }*/
    
    // add extra 200 courses for minor if extra space
    if (minor.filledTextboxesExtra < specialist.textboxesExtra) {

    }
}  


/**
 * Autofills textboxes for 400 level courses. 
**/
function fill400s() {
    'use strict';

    var spec400s = $('#spec400')[0].getElementsByTagName('input');
    var maj400s = $('#maj400')[0].getElementsByTagName('input');
    //var min400s = $('#minextra')[0].getElementsByTagName('input');

    
    // clear textboxes
    for (var k = 0; k < 3; k++) {
        spec400s[k].value = '';
        spec400s[k].disabled = true;
        if (k < 1) {
            maj400s[k].value = '';
            maj400s[k].disabled = true;
        }
        /*
        min400s[k].value = '';
        min400s[k].disabled = true;
        */
    }
    
    // fill courses that have been selected
    fill400Textboxes('specialist', spec400s, '400');
    fill400Textboxes('major', maj400s, '400');
    //fill400Textboxes('minor', min400s, '400');

}
 
function fillExtraTextboxes(post, postElement, level) {
    for (var i = window[post]['index' + level] + 1; i < activeCourses.length; i++) {
        if (window[post]['index' + level] === activeCourses.length || window[post].filledTextboxesExtra === window[post].textboxesExtra) {
            break;
        }
        var course = activeCourses[i];
        if (postElement[window[post].filledTextboxesExtra].value === '' && course.indexOf('CSC' + level.charAt(0)) != -1 
            && course.indexOf('CSC373') === -1 && course.indexOf('CSC369') === - 1) {
            postElement[window[post].filledTextboxesExtra].value = activeCourses[i];
            postElement[window[post].filledTextboxesExtra].disabled = true;
            window[post]['index' + level] = i;
            window[post].filledTextboxesExtra += 1;
            creditCount300and400.maj += 0.5;
        }
    }
}

/**
 * Autofills the textboxes for the extra 300+ credits category
**/
function fillExtra() {
    'use strict';

    var i = 0;
    var spec_extra = $('#specextra')[0].getElementsByTagName('input');
    var maj_extra = $('#majextra')[0].getElementsByTagName('input');
    var min_extra = $('#minextra')[0].getElementsByTagName('input');

    for (var k = 0; k < 4; k++) {

        // clear text boxes
        if (spec_extra[k].value.indexOf('MAT') === -1 && spec_extra[k].value.indexOf('STA') === -1) {
            spec_extra[k].value = '';
            spec_extra[k].disabled = false;
        } 
        if (k < 3) {
            if (maj_extra[k].value.indexOf('MAT') === -1 && maj_extra[k].value.indexOf('STA') === -1) {
                maj_extra[k].value = '';
                maj_extra[k].disabled = false;
            }
            min_extra[k].value = '';
            min_extra[k].disabled = true;
        } 

        // add credit count for MAT and STA courses
        if (spec_extra[k].value.indexOf('MAT') > -1 || spec_extra[k].value.indexOf('STA') > -1) {
            creditCount300and400.spec += 0.5;
            specialist.filledTextboxesExtra += 1;
        } 
        if (k < 3) {
            if (maj_extra[k].value.indexOf('MAT') > -1 || maj_extra[k].value.indexOf('STA') > -1) {
                creditCount300and400.maj += 0.5;
                major.filledTextboxesExtra += 1;
            }
        }
    }

    // fill courses that have been selected
    fillExtraTextboxes('specialist', spec_extra, '300');
    fillExtraTextboxes('major', maj_extra, '300');
    fillExtraTextboxes('minor', min_extra, '300')

    if (specialist.filledTextboxesExtra < specialist.textboxesExtra) {
        fillExtraTextboxes('specialist', spec_extra, '400');
    } 
    if (major.filledTextboxesExtra < major.textboxesExtra) {
        fillExtraTextboxes('major', maj_extra, '400');
    }
    if (minor.filledTextboxesExtra < minor.textboxesExtra) {
        fillExtraTextboxes('minor', min_extra, '400');
    }

}


/**
 * Autofills textboxes and updates category for Inquiry courses
**/

function fillMisc() {
    'use strict';

    var spec_inq = $('#spec_misc')[0].getElementsByTagName('input');
    var maj_inq = $('#maj_misc')[0].getElementsByTagName('input');

    // clear textboxes
    if (spec_inq[0].value.indexOf('PEY') === -1) {
        spec_inq[0].value = '';
        spec_inq[0].readOnly = false;
    } if (maj_inq[0].value.indexOf('PEY') === -1) {
        maj_inq[0].value = '';
        maj_inq[0].readOnly = false;
    }

    // fill textboxes
    if (activeInq.length > 0) {
        spec_inq[0].value = activeInq[0];
        spec_inq[0].readOnly = true;
    } if (activeInq.length > 0){
        maj_inq[0].value = activeInq[0];
        maj_inq[0].readOnly = true;
    }
    
    // update category
    if (spec_inq[0].value != '') {
        updateCategory($('#spec_misc')[0].getElementsByClassName('code')[0], 'fulfilled');
        categoriesCompleted.spec += 1;
    } if (maj_inq[0].value != '') {    
        updateCategory($('#maj_misc')[0].getElementsByClassName('code')[0], 'fulfilled'); 
        categoriesCompleted.spec += 1;
    } if (spec_inq[0].value === '') {
        updateCategory($('#spec_misc')[0].getElementsByClassName('code')[0], 'not fulfilled');
    } if (maj_inq[0].value === '') {
        updateCategory($('#maj_misc')[0].getElementsByClassName('code')[0], 'not fulfilled');
    }

}

