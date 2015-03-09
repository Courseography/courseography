/**
 * Fills textboxes for each POSt for 300 and 400 level categories
 *
 * @param {number} specBound Number of textboxes for specialist
 * @param {number} majBound Number of textboxes for major
 * @param {string} textboxLevel Level of textboxes we are filling
 * @param {string} activeLevel Level of courses we are filling textboxes with
 * @param {number} i The textbox we are at now
 * @param {HTMLElement} specElement The textboxes for Specilist to fill
 * @param {HTMLElement} majElement The textboxes for Major to fill
 * @param {HTMLElement} minElement The textboxes for Minor to fill
 * @return {number} The next textbox that has not been filled 
**/
function fill300and400Textboxes(specBound, majBound, textboxLevel, activeLevel, i, specElement, majElement, minElement) {
    'use strict';

    var index = 'index' + activeLevel;

    for (var m = 0; m < specBound; m++) {
        if (window[index].spec === window['active' + activeLevel + 's'].length || i === specBound) {
            break;
        }
        specElement[i].value = window['active' + activeLevel + 's'][window[index].spec];
        specElement[i].readOnly = true;
        window[index].spec += 1;
        window['filledTextboxes' + textboxLevel].spec += 1;
        creditCount300and400.spec += 0.5;
        if (i < majBound) {
            majElement[i].value = window['active' + activeLevel + 's'][window[index].maj];
            majElement[i].readOnly = true;
            window[index].maj += 1;
            window['filledTextboxes' + textboxLevel].maj += 1;
            creditCount300and400.maj += 0.5;
        }
        if (minElement[i].value === '') {
            minElement[i].value = window['active' + activeLevel + 's'][window[index].min];
            minElement[i].readOnly = true;
            window[index].min += 1;
            window['filledTextboxes' + textboxLevel].min += 1;
            creditCount300and400.min += 0.5;
        }
        i += 1; 
    }

    return i;
}


/**
 * Autofills textboxes for 300 level courses. 
**/
function fill300s() {
    'use strict';

    var spec300s = $('.lvl300spec');
    var maj300s = $('.lvl300maj');
    var min300s = $('.lvl300min');

    
    // clear textboxes
    for (var k = 0; k < 3; k++) {
        spec300s[k].value = '';
        spec300s[k].readOnly = false;
        if (k < 2) {
            maj300s[k].value = '';
            maj300s[k].readOnly = false;
        }
        if (min300s[k].value.indexOf('CSC4') === -1) {
            min300s[k].value = '';
            min300s[k].readOnly = false;
        }
    }
    
    // fill courses that have been selected
    var k = fill300and400Textboxes(3, 2, '300', '300', 0, spec300s, maj300s, min300s);

    if (k < 3) {
        var m = fill300and400Textboxes(3, 2, '300', '400', k, spec300s, maj300s, min300s);
        if (m < 3) {
            // add extra 200 level courses for min
            addExtraMinCourses(m, min300s);
        }
    }
    
}  


/**
 * Autofills textboxes for 400 level courses. 
**/
function fill400s() {
    'use strict';

    var spec400s = $('.lvl400spec');
    var maj400s = $('.lvl400maj');
    var min400s = $('.lvl400min');

    
    // clear textboxes
    for (var k = 0; k < 3; k++) {
        spec400s[k].value = '';
        spec400s[k].readOnly = false;
        if (k < 1) {
            maj400s[k].value = '';
            maj400s[k].readOnly = false;
        }
        min400s[k].value = '';
        min400s[k].readOnly = false;
    }
    
    // fill courses that have been selected
    fill300and400Textboxes(active400s.length, 1, '400', '400', 0, spec400s, maj400s, min400s);

}
 
/**
 * Autofills the textboxes for the extra 300+ credits category
**/
function fillExtra() {
    'use strict';

    var i = 0;
    var spec_extra = $('#spec_extra')[0].getElementsByTagName('input');
    var maj_extra = $('#maj_extra')[0].getElementsByTagName('input');

    for (var k = 0; k < 4; k++) {

        // clear text boxes
        if (spec_extra[k].value.indexOf('MAT') === -1 && spec_extra[k].value.indexOf('STA') === -1) {
            spec_extra[k].value = '';
            spec_extra[k].readOnly = false;
        } 
        if (k < 3) {
            if (maj_extra[k].value.indexOf('MAT') === -1 && maj_extra[k].value.indexOf('STA') === -1) {
                maj_extra[k].value = '';
                maj_extra[k].readOnly = false;
            }
        } 

        // add credit count for MAT and STA courses
        if (spec_extra[k].value.indexOf('MAT') > -1 || spec_extra[k].value.indexOf('STA') > -1) {
            creditCount300and400.spec += 0.5;
            filledTextboxesExtra.spec += 1;
        } 
        if (k < 3) {
            if (maj_extra[k].value.indexOf('MAT') > -1 || maj_extra[k].value.indexOf('STA') > -1) {
                creditCount300and400.maj += 0.5;
                filledTextboxesExtra.maj += 1;
            }
        }
    }

    // fill courses that have been selected
    for (var m = 0; m < active300s.length; m++) {
        if (index300.spec === active300s.length || i === 4) {
            break;
        }
        if ((i < 2) && (maj_extra[i].value === '')) {
            maj_extra[i].value = active300s[index300.maj];
            maj_extra[i].readOnly = true;
            index300.maj += 1;
            filledTextboxesExtra.maj += 1;
            creditCount300and400.maj += 0.5;
        }
        if (spec_extra[i].value === '') {
            spec_extra[i].value = active300s[index300.spec];
            spec_extra[i].readOnly = true;
            index300.spec += 1;
            filledTextboxesExtra.spec += 1;
            creditCount300and400.spec += 0.5;
        } 
        i += 1;
    }

    if (i < 4) {
        for (var m = 0; m < active400s.length; m++) {
            if (((index400.spec === active400s.length) && (index400.maj === active400s.length)) || (i === 4)) {
                break;
            }
            if ((i < 3) && (maj_extra[i].value === '')) {
                maj_extra[i].value = active400s[index400.maj];
                maj_extra[i].readOnly = true;
                index400.maj += 1;
                filledTextboxesExtra.maj += 1;
                creditCount300and400.maj += 0.5;
            }
            if ((spec_extra[i].value === '') && (index400.spec < active400s.length)){
                spec_extra[i].value = active400s[index400.spec];
                spec_extra[i].readOnly = true;
                index400.spec += 1;
                filledTextboxesExtra.spec += 1;
                creditCount300and400.spec += 0.5;
            }
            i += 1;
        }
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
