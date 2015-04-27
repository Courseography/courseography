'use strict'

import $ from 'jquery'

import { fill300s, fill400s, fillMisc, fillExtra } from './fill_textboxes'
import { updateReqsCategory, update300Categories, update400Categories } from './update_categories'

var activeCourses = [];
var specialist = {'index300': 0, 'index400': 0, 'categoriesCompleted': 0, 'filledTextboxes300': 0,
                  'filledTextboxes400': 0, 'filledTextboxesExtra': 0, 'specCount': 0,
                  'reqs': ['CSC108', 'CSC148', 'CSC165', 'CSC207', 'CSC209', 'CSC236', 'CSC258', 'CSC263',
                  'Sta1', 'Lin1', 'Calc1', 'CSC369', 'CSC373'], 'textboxes300': 3, 'textboxes400': 3,
                  'textboxesExtra': 4, 'categories': 17, 'creditCount': 0, 'name': 'specialist'};
var major = {'index300': 0, 'index400': 0, 'categoriesCompleted': 0, 'filledTextboxes300': 0,
             'filledTextboxes400': 0, 'filledTextboxesExtra': 0, 'majCount': 0, 'reqs': ['CSC108',
             'CSC148', 'CSC165', 'CSC207', 'CSC236', 'CSC258', 'CSC263', 'Sta1', 'Calc1'], 'textboxes300': 2,
             'textboxes400': 1, 'textboxesExtra': 3, 'categories': 13, 'creditCount': 0, 'name': 'major'};
var minor = {'index300': 0, 'index400': 0, 'categoriesCompleted': 0, 'filledTextboxesExtra': 0,
             'reqs': ['CSC108', 'CSC148', 'CSC165', 'CSC207', 'CSC236'], 'textboxesExtra': 3, 'categories': 6,
             'creditCount': 0, 'additionalMin200': ['CSC209', 'CSC258', 'CSC263'], 'name': 'minor'};


/**
 * Updates POSts when button is clicked.
 */
$('#update').click(function () {
    'use strict';

    updateAllCategories();
});


/**
 * Updates all categories to see if they are fulfilled or not.
 */
function updateAllCategories() {
    'use strict';

    resetValues();

    updateActiveCourses();

    fill400s();
    fill300s();
    fillMisc();
    fillExtra();

    updateReqsCategory(specialist, 'spec');
    updateReqsCategory(major, 'maj');
    updateReqsCategory(minor, 'min');

    update300Categories();
    update400Categories();
    updateExtraCategories();
    updateMatCreditCount();

    fillCreditCount();
    checkPostCompleted();
}


/**
 * Resets all values to initial starting values
 */
function resetValues() {
    'use strict';

    activeCourses = [];
    specialist = {'index300': 0, 'index400': 0, 'categoriesCompleted': 0, 'filledTextboxes300': 0, 'filledTextboxes400': 0,
                  'filledTextboxesExtra': 0, 'specCount': 0, 'reqs': ['CSC108', 'CSC148', 'CSC165', 'CSC207', 'CSC209', 'CSC236',
                  'CSC258', 'CSC263', 'Sta1', 'Lin1', 'Calc1', 'CSC369', 'CSC373'], 'textboxes300': 3, 'textboxes400': 3, 'textboxesExtra': 4,
                  'categories': 17, 'creditCount': 0, 'name': 'specialist'};
    major = {'index300': 0, 'index400': 0, 'categoriesCompleted': 0, 'filledTextboxes300': 0, 'filledTextboxes400': 0,
             'filledTextboxesExtra': 0, 'majCount': 0, 'reqs': ['CSC108', 'CSC148', 'CSC165', 'CSC207', 'CSC236',
             'CSC258', 'CSC263', 'Sta1', 'Calc1'], 'textboxes300': 2, 'textboxes400': 1, 'textboxesExtra': 3, 'categories': 13,
             'creditCount': 0, 'name': 'major'};
    minor = {'index300': 0, 'index400': 0, 'categoriesCompleted': 0, 'filledTextboxesExtra': 0, 'reqs': ['CSC108', 'CSC148', 'CSC165', 'CSC207', 'CSC236'],
             'textboxesExtra': 3, 'categories': 6, 'creditCount': 0, 'additionalMin200': ['CSC209', 'CSC258', 'CSC263'], 'name': 'minor'};
}


/**
 * Records a course as clicked.
 * @param {string} courseCode The course code
 */
function activateCourse(courseCode) {
    'use strict';

    var elements = document.getElementsByClassName(courseCode);
    for (var i = 0; i < elements.length; i++) {
        elements[i].style.backgroundColor = '#99ff99';
    }
}


/**
 * Records a course as not clicked.
 * @param {string} courseCode The course code
 */
function deactivateCourse(courseCode) {
    'use strict';

    var elements = document.getElementsByClassName(courseCode);
    for (var i = 0; i < elements.length; i++) {
            elements[i].style.backgroundColor = '#BABABA';
    }
}


/**
 * Records a category as fulfilled or not fulfilled
 * @param {Element} category Element of category
 * @param {string} status Whether it is 'fulfilled' or 'not fulfilled'
 */
function updateCategory(category, status) {
    'use strict';

    if (status === 'fulfilled') {
        category.style.backgroundColor = "#3CB371";
    } else if (status === 'not fulfilled') {
        category.style.backgroundColor = '#ebe8e4';
    }
}


/**
 * Updates Credit Count for each POSt.
 */
function fillCreditCount() {
    'use strict';

    fillSpecCreditCount();
    fillMajCreditCount();
    fillMinCreditCount();
}



/**
 * Autofills the credit count for specialist
 */
function fillSpecCreditCount() {
    'use strict';

    if (specialist.creditCount >= 12) {
        $('#spec_creds').html('(12.0/12.0)');
    } else {
        $('#spec_creds').html('(' + specialist.creditCount.toFixed(1) + '/12.0)');
    }
}


/**
 * Autofills the credit count for major
 */
function fillMajCreditCount() {
    'use strict';

    if (major.creditCount >= 8) {
        $('#maj_creds').html('(8.0/8.0)');
    } else {
        $('#maj_creds').html('(' + major.creditCount.toFixed(1) + '/8.0)');
    }
}


/**
 * Autofills the credit count for minor.
 */
function fillMinCreditCount() {
    'use strict';

    if (minor.creditCount >= 4) {
        $('#min_creds').html('(4.0/4.0)');
    } else {
        $('#min_creds').html('(' + minor.creditCount.toFixed(1) + '/4.0)');
    }
}


/**
 * Autofills extra 200-level courses for last minor constraint.
 */
function addExtraMinCourses() {
    'use strict';

    var minExtra = $('#minextra')[0].getElementsByTagName('input');
    var current = minor.filledTextboxesExtra;

    for (var m = 0; m < 3 && current < 3; m++) {
        if (getCookie(minor.additionalMin200[m].toLowerCase()) === 'active' ||
            getCookie(minor.additionalMin200[m].toLowerCase()) === 'overridden') {
            minExtra[current].value = minor.additionalMin200[m];
            minExtra[current].disabled = true;
            minor.creditCount += 0.5;
            minor.filledTextboxesExtra += 1;
            current += 1;
        }

    }
}


/**
 * Checks whether a POSt is completed and updates credit count colour if it is.
 */
function checkPostCompleted() {
    'use strict';

    $('#spec_creds').css('color', specialist.categoriesCompleted === specialist.categories ? 'green' : 'red');

    $('#maj_creds').css('color', major.categoriesCompleted === major.categories ? 'green' : 'red');

    $('#min_creds').css('color', minor.categoriesCompleted === minor.categories ? 'green' : 'red');
}


/**
 * Updates list of current active (selected) courses in Graph.
 */
function updateActiveCourses() {
    'use strict';

    activeCourses = [];

    // check for active CSC courses
    for (var i = 0; i < allCourses.length; i++) {
        if (getCookie(allCourses[i].toLowerCase()) === 'active' || getCookie(allCourses[i].toLowerCase()) === 'overridden') {
            activeCourses.push(allCourses[i]);
        }
    }

    // check for active math courses
    for (var i = 0; i < math.length; i++) {
        if (getCookie(math[i].toLowerCase()) === 'active') {
            activeCourses.push(math[i]);
        }
    }
}


/**
 * Updates Credit count for MAT and STA courses.
 */
function updateMatCreditCount() {
    'use strict';

    var specExtra = $('#specextra')[0].getElementsByTagName('input');
    var majExtra = $('#majextra')[0].getElementsByTagName('input');

    for (var k = 0; k < 4; k++) {
        if (specExtra[k].value.indexOf('MAT') > -1 || specExtra[k].value.indexOf('STA') > -1) {
            specialist.creditCount += 0.5;
            specialist.filledTextboxesExtra += 1;
        }
        if (k < 3 && (majExtra[k].value.indexOf('MAT') > -1 || majExtra[k].value.indexOf('STA') > -1)) {
                major.creditCount += 0.5;
                major.filledTextboxesExtra += 1;
        }
    }
}

/**
 * Returns whether course is a specialist course or not
 * @param {string} Name of course
 * @return {boolean} True if course is a specialist, False otherwise
**/
function notSpecialistCourse(course) {
    'use strict';

    return specialistCourses.indexOf(course) === -1;
}
