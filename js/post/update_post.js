// Objects to store how many courses in each category have been completed
var completedSpec = {'CSC108': 0, 'CSC148': 0, 'CSC165': 0, 'Calc1': 0, 'CSC207': 0, 'CSC209': 0, 
                      'CSC236': 0, 'CSC258': 0, 'CSC263': 0, 'Sta1': 0, 'Lin1': 0, 'CSC369': 0, 'CSC373': 0};

var completedMaj = {'CSC108': 0, 'CSC148': 0, 'CSC165': 0, 'Calc1': 0, 'CSC207': 0, 'CSC236': 0, 
                     'CSC258': 0, 'CSC263': 0, 'Sta1': 0};

var completedMin = {'CSC108': 0, 'CSC148': 0, 'CSC165': 0, 'CSC236': 0, 'CSC207': 0}; 

var level300 = ['CSC300', 'CSC301', 'CSC302', 'CSC309', 'CSC310', 'CSC318', 'CSC320', 'CSC321', 
                'CSC324', 'CSC336','CSC343', 'CSC358', 'CSC372', 'CSC384', 'ECE385', 'ECE489', 
                'BCB410', 'BCB420', 'BCB430'];

var level400 = ['CSC401', 'CSC404', 'CSC411', 'CSC412', 'CSC418', 'CSC420', 'CSC428', 'CSC436',
                'CSC438', 'CSC443', 'CSC446', 'CSC448', 'CSC454', 'CSC456', 'CSC458', 'CSC463', 
                'CSC465', 'CSC469', 'CSC486', 'CSC488', 'CSC490', 'CSC491', 'CSC494', 'CSC495', 
                'BCB410', 'BCB420', 'BCB430', 'CSC410'];

var additionMin200s = ['CSC209', 'CSC258', 'CSC263'];

activeInq = [];
active400s = [];
active300s = [];
var index300 = {'spec': 0, 'maj': 0, 'min': 0};
var index400 = {'spec': 0, 'maj': 0, 'min': 0};
var categoriesCompleted = {'spec': 0, 'maj': 0, 'min': 0};
var filledTextboxes300 = {'spec': 0, 'maj': 0, 'min': 0};
var filledTextboxes400 = {'spec': 0, 'maj': 0, 'min': 0};
var filledTextboxesExtra = {'spec': 0, 'maj': 0, 'min': 0};
var index200 = 0;
var filledTextboxes200 = 0;
var minCount = 0;
var majCount = 0;
var specCount = 0;

/**
 * Updates POSts when button is clicked.
**/
$('#update').click(function (e) {
    'use strict';

    updateAllCategories();
    updateNavPost();
});


/**
 * Updates all categories to see if they are fulfilled or not.
**/
function updateAllCategories() {
    'use strict';

    resetValues();

    updateCompletedMinCourses();
    updateCompletedMajCourses();
    updateCompletedSpecCourses();
    update300s();
    update400s();
    fill400s();
    fill300s();
    fillMisc();
    fillExtra();
    fillCreditCount();


    updateReqsCategory('Spec');
    updateReqsCategory('Maj');
    updateReqsCategory('Min');


    // Update 300s
    if (filledTextboxes300.spec === 3) {
        updateCategory($('#spec_300')[0].getElementsByClassName('code')[0], 'fulfilled');
        updateCategory($('#min_misc')[0].getElementsByClassName('code')[0], 'fulfilled');
        updateCategory($('#maj_300')[0].getElementsByClassName('code')[0], 'fulfilled');
        categoriesCompleted.spec += 1;
        categoriesCompleted.maj += 1;
    } else {
        updateCategory($('#spec_300')[0].getElementsByClassName('code')[0], 'not fulfilled');
        updateCategory($('#maj_300')[0].getElementsByClassName('code')[0], 'not fulfilled');
        updateCategory($('#min_misc')[0].getElementsByClassName('code')[0], 'not fulfilled');
    }


    // Update 400s
    if (filledTextboxes400.spec === 3) {
        updateCategory($('#spec_400')[0].getElementsByClassName('code')[0], 'fulfilled');
        updateCategory($('#min_misc')[0].getElementsByClassName('code')[0], 'fulfilled');
        categoriesCompleted.spec += 1;
    } else {
        updateCategory($('#spec_400')[0].getElementsByClassName('code')[0], 'not fulfilled');
        updateCategory($('#min_misc')[0].getElementsByClassName('code')[0], 'not fulfilled');
    }

    if (filledTextboxes400.maj === 1) {
        updateCategory($('#maj_400')[0].getElementsByClassName('code')[0], 'fulfilled');
        categoriesCompleted.maj += 1;
    } else {    
        updateCategory($('#maj_400')[0].getElementsByClassName('code')[0], 'not fulfilled'); 
    }

    // Update Extra
    if (filledTextboxesExtra.spec === 4) {
        updateCategory($('#spec_extra')[0].getElementsByClassName('code')[0], 'fulfilled');
        categoriesCompleted.spec += 1;
    } 
    if (filledTextboxesExtra.maj === 3) {
        updateCategory($('#maj_extra')[0].getElementsByClassName('code')[0], 'fulfilled');
        categoriesCompleted.maj += 1;
    } 
    if ((filledTextboxes300.min + filledTextboxes400.min + filledTextboxes200) === 3) {
        updateCategory($('#min_misc')[0].getElementsByClassName('code')[0], 'fulfilled');
        categoriesCompleted.min += 1;
    } 
    if (filledTextboxesExtra.spec < 4) {
        updateCategory($('#spec_extra')[0].getElementsByClassName('code')[0], 'not fulfilled');
    } 
    if (filledTextboxesExtra.maj < 3) {
        updateCategory($('#maj_extra')[0].getElementsByClassName('code')[0], 'not fulfilled');
    } 
    if ((filledTextboxes300.min + filledTextboxes400.min + filledTextboxes200) < 3) {
        updateCategory($('#min_misc')[0].getElementsByClassName('code')[0], 'not fulfilled');
    }

    checkPostCompleted();
}

/**
 * Resets all values to initial starting values
**/
function resetValues() {
    'use strict';

    index300 = {'spec': 0, 'maj': 0, 'min': 0};
    index400 = {'spec': 0, 'maj': 0, 'min': 0};
    categoriesCompleted = {'spec': 0, 'maj': 0, 'min': 0};
    index200 = 0;

    creditCount300and400.maj = 0;
    creditCount300and400.spec = 0;
    creditCount300and400.min = 0;

    filledTextboxes300 = {'spec': 0, 'maj': 0, 'min': 0};
    filledTextboxes400 = {'spec': 0, 'maj': 0, 'min': 0};
    filledTextboxesExtra = {'spec': 0, 'maj': 0, 'min': 0};
    filledTextboxes200 = 0;
}


/**
 * Updates categories for required courses in each POSt
 * @param {string} post The POSt that you are updating categories for.
**/
function updateReqsCategory(post) {
    'use strict';

    var array = 'completed' + post;
    for (var property in window[array]) {
        if (window[array].hasOwnProperty(property)) {
            var category = $('#' + post.toLowerCase() + '_' + property.toLowerCase())[0].getElementsByClassName('code')[0];
            if (window[array][property] === 1) { // if the category is completed
                activateCourse(property);
                updateCategory(category, 'fulfilled');
                categoriesCompleted[post.toLowerCase()] += 1;
            } else { // if the category is not completed
                deactivateCourse(property);
                updateCategory(category, 'not fulfilled');
            }
        }
    }
}


/**
 * Records a course as clicked. 
 * @param {string} courseCode The course code
**/
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
**/
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
**/
function updateCategory(category, status) {
    'use strict';

    if (status === 'fulfilled') {
        category.style.backgroundColor = "#3CB371";
    } else if (status === 'not fulfilled') {
        category.style.backgroundColor = '#ebe8e4';
    }
}


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

    var m;

    if (k < 3) {
        m = fill300and400Textboxes(3, 2, '300', '400', k, spec300s, maj300s, min300s);
    }

    // add extra 200 level courses for min
    if (m < 3) {
        addExtraMinCourses(m, min300s);
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
        if ((index300.spec === active300s.length) || (i === 4)) {
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

/**
 * Updates Credit Count for each POSt.
 **/
function fillCreditCount() {
    'use strict';

    specCount = creditCountSpec  + creditCount300and400.spec;
    majCount = creditCountMaj + creditCount300and400.maj;
    minCount = creditCountMin + creditCount300and400.min;


    fillSpecCreditCount(specCount);
    fillMajCreditCount(majCount);
    fillMinCreditCount(minCount);
}

/**
 * Autofills the credit count for specialist
 * @param {number} specCount The credit count for specialist
**/
function fillSpecCreditCount(specCount) {
    'use strict';

    if (specCount >= 12) {
        $('#spec_creds').html('(12.0/12.0)');
    } else {
        $('#spec_creds').html('(' + (specCount).toFixed(1) + '/12.0)');
    }
}

/**
 * Autofills the credit count for major
 * @param {number} majCount The credit count for major
**/
function fillMajCreditCount(majCount) {
    'use strict';

    if (majCount >= 8) {
        $('#maj_creds').html('(8.0/8.0)')
    } else {
        $('#maj_creds').html('(' + (majCount).toFixed(1) + '/8.0)');
    }
}


/**
 * Autofills the credit count for minor.
 * @param {number} minCount The credit count for minor
**/
function fillMinCreditCount(minCount) {
    'use strict';

    if (minCount >= 4) {
        $('#min_creds').html('(4.0/4.0)')
    } else {
        $('#min_creds').html('(' + (minCount).toFixed(1) + '/4.0)');
    }
}


/**
 * Autofills extra 200-level courses for last minor constraint.
 * @param {number} index The textbox number we are at
 * @param {HTMLElement} min300s Array of textbox elements to fill
**/
function addExtraMinCourses(index, min300s) {
    'use strict';

    for (var m = 0; m < 3; m++) {
        if (index === 3) {
            break;      
        } else if (getCookie(additionMin200s[m]) === 'active') {
            min300s[index].value = additionMin200s[m];
            min300s[index].readOnly = true;
            creditCount300and400.min += 0.5;
            filledTextboxes200 += 1;
            index += 1;
        }
        
    }
}


/**
 * Checks whether a POSt is completed and updates credit count colour if it is.
**/
function checkPostCompleted() {
    'use strict';

    if (categoriesCompleted.spec === 17) {
        $('#spec_creds').css('color', 'green');
    } else {
        $('#spec_creds').css('color', 'red');
    }
    
    if (categoriesCompleted.maj === 13) {
        $('#maj_creds').css('color', 'green');
    } else {
        $('#maj_creds').css('color', 'red');
    } 

    if (categoriesCompleted.min === 6) {
        $('#min_creds').css('color', 'green');
    } else {
        $('#min_creds').css('color', 'red');
    }
}


/**
 * Updates the Nav Bar on the Check My Post! page
**/
function updateNavPost() {
    'use strict';

    var nav_post = $('#nav-links')[0].getElementsByTagName('li')[3].getElementsByTagName('a')[0];

    if (getCookie('specialist') === 'active') {
        nav_post.innerHTML = 'Check My POSt! (' + (specCount).toFixed(1) + '/12.0)';
        setCookie('activecount', (specCount).toFixed(1));
    } else if (getCookie('major') === 'active') {
        nav_post.innerHTML = 'Check My POSt! (' + (majCount).toFixed(1) + '/8.0)';
        setCookie('activecount', (majCount).toFixed(1));
    } else if (getCookie('minor') === 'active') {
        nav_post.innerHTML = 'Check My POSt! (' + (minCount).toFixed(1) + '/4.0)';
        setCookie('activecount', (minCount).toFixed(1));
    } 
}


/**
 * Updates the Nav Bar on the Graph page.
**/
function updateNavGraph() {
    'use strict';

    var nav_graph = $('#nav-links')[0].getElementsByTagName('li')[3].getElementsByTagName('a')[0];

    if (getCookie('specialist') === 'active') {
        nav_graph.innerHTML = 'Check My POSt! (' + getCookie('activecount') + '/12.0)';
    } else if (getCookie('major') === 'active') {
        nav_graph.innerHTML = 'Check My POSt! (' + getCookie('activecount') + '/8.0)';
    } else if (getCookie('minor') === 'active') {
        nav_graph.innerHTML = 'Check My POSt! (' + getCookie('activecount') + '/4.0)';
    } 
}
