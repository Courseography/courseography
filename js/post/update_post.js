// Objects to store how many courses in each category have been completed
var completed_spec = {'CSC108': 0, 'CSC148': 0, 'CSC165': 0, 'Calc1': 0, 'CSC207': 0, 'CSC209': 0, 
                      'CSC236': 0, 'CSC258': 0, 'CSC263': 0, 'Sta1': 0, 'Lin1': 0, 'CSC369': 0, 'CSC373': 0};

var completed_maj = {'CSC108': 0, 'CSC148': 0, 'CSC165': 0, 'Calc1': 0, 'CSC207': 0, 'CSC236': 0, 
                     'CSC258': 0, 'CSC263': 0, 'Sta1': 0};

var completed_min = {'CSC108': 0, 'CSC148': 0, 'CSC165': 0, 'CSC236': 0, 'CSC207': 0}; 

var level300 = {'CSC300': 0, 'CSC301': 0, 'CSC302': 0, 'CSC309': 0, 'CSC310': 0, 
                'CSC318': 0, 'CSC320': 0, 'CSC321': 0, 'CSC324': 0, 'CSC336': 0,
                'CSC343': 0, 'CSC358': 0, 'CSC372': 0, 'CSC384': 0, 'ECE385': 0,
                'ECE489': 0, 'BCB410': 0, 'BCB420': 0, 'BCB430': 0};

var level400 = {'CSC401': 0, 'CSC404': 0, 'CSC411': 0, 'CSC412': 0, 'CSC418': 0,
                'CSC420': 0, 'CSC428': 0, 'CSC436': 0, 'CSC438': 0, 'CSC443': 0, 
                'CSC446': 0, 'CSC448': 0, 'CSC454': 0, 'CSC456': 0, 'CSC458': 0,
                'CSC463': 0, 'CSC465': 0, 'CSC469': 0, 'CSC486': 0, 'CSC488': 0,
                'CSC490': 0, 'CSC491': 0, 'CSC494': 0, 'CSC495': 0, 'BCB410': 0, 
                'BCB420': 0, 'BCB430': 0, 'CSC410': 0};

var additional_min_200s = ['CSC209', 'CSC258', 'CSC263'];

activeInq = [];
active400s = [];
active300s = [];
var index300 = {'Spec': 0, 'Maj': 0, 'Min': 0};
var index400 = {'Spec': 0, 'Maj': 0, 'Min': 0};
var categories_completed = {'Spec': 0, 'Maj': 0, 'Min': 0};
var index200 = 0;

/**
 * Updates POSts when button is clicked.
**/
$('#update').click(function (e) {
    'use-strict'

    updateAllCategories();
    updateNavPost();
});


/**
 * Sets cookie when clicking on a course in Check My POSt
 * TODO: Check prereqs properly
**/
/*
$('.full_name').click(function (e) {
    'use-strict';

    var index = 0;
    if (this.className.indexOf('CSC') > -1) {
        index = this.className.indexOf('CSC'); 
    } else if (this.className.indexOf('Calc') > -1) {
        index = this.className.indexOf('Calc'); 
    } else if (this.className.indexOf('Lin') > -1) {
        index = this.className.indexOf('Lin'); 
    } else if (this.className.indexOf('Sta') > -1) {
        index = this.className.indexOf('Sta'); 
    }
    

    var courseCode = this.className.substring(index, this.className.length);
    if (getCookie(courseCode) === 'inactive' || getCookie(courseCode) === 'takeable') {
            setCookie(courseCode, 'active');
    } if (getCookie(courseCode) === 'active' || getCookie(courseCode) === 'overriden') {
            setCookie(courseCode, 'inactive');
    }
    updateAllCategories();
});
*/


/**
 * Updates all categories to see if they are fulfilled or not.
**/
// TODO: Add CSC240 and CSC265
function updateAllCategories() {
    'use-strict'

    index300 = {'Spec': 0, 'Maj': 0, 'Min': 0};
    index400 = {'Spec': 0, 'Maj': 0, 'Min': 0};
    categories_completed = {'Spec': 0, 'Maj': 0, 'Min': 0};
    index200 = 0;

    creditCount300and400['Maj'] = 0;
    creditCount300and400['Spec'] = 0;
    creditCount300and400['Min'] = 0;


    updateCompletedMinCourses();
    updateCompletedMajCourses();
    updateCompletedSpecCourses();
    update300s();
    update400s();
    fill400s();
    fill300s();
    fillMisc();
    fillExtra();
    // updateCreditCount();
    fillCreditCount();

    // Update Specialist 
    for (var property in completed_spec) {
        if (completed_spec.hasOwnProperty(property)) {
            var category = $('#spec_' + property.toLowerCase())[0].getElementsByClassName('code')[0];
            if (completed_spec[property] === 1) { // if the category is completed
                activateCourse(property);
                updateCategory(category, 'fulfilled');
                categories_completed['Spec'] += 1;
            } else { // if the category is not completed
                deactivateCourse(property);
                updateCategory(category, 'not fulfilled');
            }
        }
    }

    // Update Major
    for (var property in completed_maj) {
        if (completed_maj.hasOwnProperty(property)) {
            var category = $('#maj_' + property.toLowerCase())[0].getElementsByClassName('code')[0];
            if (completed_maj[property] === 1) { // if the category is completed
                activateCourse(property);
                updateCategory(category, 'fulfilled');
                categories_completed['Maj'] += 1;
            } else { // if the category is not completed
                deactivateCourse(property);
                updateCategory(category, 'not fulfilled');
            }
        }
    }


    // Update Minor
    for (var property in completed_min) {
        if (completed_min.hasOwnProperty(property)) {
            var category = $('#min_' + property.toLowerCase())[0].getElementsByClassName('code')[0];
            if (completed_min[property] === 1) { // if the category is completed
                activateCourse(property);
                updateCategory(category, 'fulfilled');
                categories_completed['Min'] += 1;
            } else { // if the category is not completed
                deactivateCourse(property);
                updateCategory(category, 'not fulfilled');
            }
        }
    }


    // Update 300s
    var i = 0; 
    var spec300s = $('.lvl300spec');
    for (var m = 0; m < 3; m++) {
        if (spec300s[m].value != '') {
            i += 1;
        }
    }

    if (i === 3) {
        updateCategory($('#spec_300')[0].getElementsByClassName('code')[0], 'fulfilled');
        updateCategory($('#min_misc')[0].getElementsByClassName('code')[0], 'fulfilled');
        updateCategory($('#maj_300')[0].getElementsByClassName('code')[0], 'fulfilled');
        categories_completed['Spec'] += 1;
        categories_completed['Maj'] += 1;
    } else {
        updateCategory($('#spec_300')[0].getElementsByClassName('code')[0], 'not fulfilled');
        updateCategory($('#maj_300')[0].getElementsByClassName('code')[0], 'not fulfilled');
        updateCategory($('#min_misc')[0].getElementsByClassName('code')[0], 'not fulfilled');
    }


    // Update 400s

    i = 0; 
    var spec400s = $('.lvl400spec');
    for (var l = 0; l < 1; l++) {
        if (spec400s[l].value != '') {
            i += 1;
        }
    }

    if (i === 1) {
        updateCategory($('#spec_400')[0].getElementsByClassName('code')[0], 'fulfilled');
        updateCategory($('#maj_400')[0].getElementsByClassName('code')[0], 'fulfilled');
        updateCategory($('#min_misc')[0].getElementsByClassName('code')[0], 'fulfilled');
        categories_completed['Spec'] += 1;
        categories_completed['Maj'] += 1;
    } else {
        updateCategory($('#spec_400')[0].getElementsByClassName('code')[0], 'not fulfilled');
        updateCategory($('#maj_400')[0].getElementsByClassName('code')[0], 'not fulfilled');
        updateCategory($('#min_misc')[0].getElementsByClassName('code')[0], 'not fulfilled');
    }

    // Update Extra

    countMaj = 0; 
    countSpec = 0;
    countMin = 0;
    var specExtra = $('#spec_extra')[0].getElementsByTagName('input');
    var majExtra = $('#maj_extra')[0].getElementsByTagName('input');
    var minExtra = $('#min_misc')[0].getElementsByTagName('input');
    for (var l = 0; l < 4; l++) {
        if (specExtra[l].value != '') {
            countSpec += 1;
        } if (l < 3) {
            if (majExtra[l].value != '') {
                countMaj += 1;
            } if (minExtra[l].value != '') {
                countMin += 1;
            }
        }
    }


    if (countSpec === 4) {
        updateCategory($('#spec_extra')[0].getElementsByClassName('code')[0], 'fulfilled');
        categories_completed['Spec'] += 1;
    } if (countMaj === 3) {
        updateCategory($('#maj_extra')[0].getElementsByClassName('code')[0], 'fulfilled');
        categories_completed['Maj'] += 1;
    } if (countMin === 3) {
        updateCategory($('#min_misc')[0].getElementsByClassName('code')[0], 'fulfilled');
        categories_completed['Min'] += 1;
    } if (countSpec < 4) {
        updateCategory($('#spec_extra')[0].getElementsByClassName('code')[0], 'not fulfilled');
    } if (countMaj < 3) {
        updateCategory($('#maj_extra')[0].getElementsByClassName('code')[0], 'not fulfilled');
    } if (countMin < 3) {
        updateCategory($('#min_misc')[0].getElementsByClassName('code')[0], 'not fulfilled');
    }

    checkPostCompleted();

}


/**
 * Records a course as clicked. 
 * @param {string} courseCode The course code
**/
function activateCourse(courseCode) {
    'use-strict'
    
    var elements = document.getElementsByClassName(courseCode);
    for (i = 0; i < elements.length; i++) {
        elements[i].style.backgroundColor = '#99ff99';
    }
}


/**
 * Records a course as not clicked. 
 * @param {string} courseCode The course code
**/
function deactivateCourse(courseCode) {
    'use-strict'
    
    var elements = document.getElementsByClassName(courseCode);
    for (i = 0; i < elements.length; i++) {
            elements[i].style.backgroundColor = '#BABABA';
    }
}


/**
 * Records a category as fulfilled or not fulfilled
 * @param {Element} category Element of category
 * @param {string} status Whether it is 'fulfilled' or 'not fulfilled'
**/
function updateCategory(category, status) {
    'use-strict'

    if (status === 'fulfilled') {
        category.style.backgroundColor = "#3CB371";
    } else if (status === 'not fulfilled') {
        category.style.backgroundColor = '#ebe8e4';
    }
}


/**
 * Autofills textboxes for 300 level courses. 
**/
function fill300s() {
    'use-strict'

    var i = 0; 

    var spec300s = $('.lvl300spec');
    var maj300s = $('.lvl300maj');
    var min300s = $('.lvl300min');

    
    // clear textboxes
    for (k = 0; k < 3; k++) {
        spec300s[k].value = '';
        spec300s[k].readOnly = false;
        if (k < 2) {
            maj300s[k].value = '';
            maj300s[k].readOnly = false;
        }
        min300s[k].value = '';
        min300s[k].readOnly = false;
    }
    

    // fill courses that have been selected
    for (var m = 0; m < 3; m++) {
        if (index300['Spec'] === active300s.length) {
            break;
        }
        spec300s[i].value = active300s[index300['Spec']];
        spec300s[i].readOnly = true;
        index300['Spec'] += 1;
        creditCount300and400['Spec'] += 0.5;
        if (i < 2) {
            maj300s[i].value = active300s[index300['Maj']];
            maj300s[i].readOnly = true;
            index300['Maj'] += 1;
            creditCount300and400['Maj'] += 0.5;
        }
        min300s[i].value = active300s[index300['Min']];
        min300s[i].readOnly = true;
        index300['Min'] += 1;
        creditCount300and400['Min'] += 0.5;
        i += 1; 
    }

    if (i < 3) {
        for (var m = 0; m < 3; m++) {
            if ((index400['Spec'] === active400s.length) || (i === 3)) {
                break;
            }
            spec300s[i].value = active400s[index400['Spec']];
            spec300s[i].readOnly = true;
            index400['Spec'] += 1;
            creditCount300and400['Spec'] += 0.5;
            if (i < 2) {
                maj300s[i].value = active400s[index400['Maj']];
                maj300s[i].readOnly = true;
                index400['Maj'] += 1;
                creditCount300and400['Maj'] += 0.5;
            }
            min300s[i].value = active400s[index400['Min']]; 
            min300s[i].readOnly = true;
            index400['Min'] += 1;
            creditCount300and400['Min'] += 0.5;
            i += 1;
        }
    }

    // add extra 200 level courses for min
    if (i < 3) {
        addExtraMinCourses(i, min300s);
    }
}  


/**
 * Autofills textboxes for 400 level courses. 
**/
function fill400s() {
    'use-strict'

    var i = 0; 
    var spec400s = $('.lvl400spec');
    var maj400s = $('.lvl400maj');
    var min400s = $('.lvl400min');

    
    // clear textboxes
    for (k = 0; k < 3; k++) {
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
    for (var m = 0; m < active400s.length; m++) {
        if ((index400['Spec'] == active400s.length) || (i === 3)) {
            break;
        }
        spec400s[i].value = active400s[index400['Spec']];
        spec400s[i].readOnly = true;
        index400['Spec'] += 1;
        creditCount300and400['Spec'] += 0.5;
        if (i < 1) {
            maj400s[i].value = active400s[index400['Maj']];
            maj400s[i].readOnly = true;
            index400['Maj'] += 1;
            creditCount300and400['Maj'] += 0.5;
        }
        min400s[i].value = active400s[index400['Min']]; 
        min400s[i].readOnly = true;
        index400['Min'] += 1;
        creditCount300and400['Min'] += 0.5;
        i += 1;
    }

}
 

function fillExtra() {
    'use-strict'

    var i = 0;
    var spec_extra = $('#spec_extra')[0].getElementsByTagName('input');
    var maj_extra = $('#maj_extra')[0].getElementsByTagName('input');

    for (var k = 0; k < 4; k++) {

        // clear text boxes
        if (spec_extra[k].value.indexOf('MAT') === -1 && spec_extra[k].value.indexOf('STA') === -1) {
            spec_extra[k].value = '';
            spec_extra[k].readOnly = false;
        } if (k < 3) {
            if (maj_extra[k].value.indexOf('MAT') === -1 && maj_extra[k].value.indexOf('STA') === -1) {
                maj_extra[k].value = '';
                maj_extra[k].readOnly = false;
            }
        } 

        // add credit count for MAT and STA courses
        if (spec_extra[k].value.indexOf('MAT') > -1 || spec_extra[k].value.indexOf('STA') > -1) {
            creditCount300and400['Spec'] += 0.5;
        } if (k < 3) {
            if (maj_extra[k].value.indexOf('MAT') > -1 || maj_extra[k].value.indexOf('STA') > -1) {
                creditCount300and400['Maj'] += 0.5;
            }
        }
    }

    // fill courses that have been selected
    for (m = 0; m < active300s.length; m++) {
        if ((index300['Spec'] === active300s.length) || (i === 4)) {
            break;
        }
        if ((i < 2) && (maj_extra[i].value === '')) {
            maj_extra[i].value = active300s[index300['Maj']];
            maj_extra[i].readOnly = true;
            index300['Maj'] += 1;
            creditCount300and400['Maj'] += 0.5;
        }
        if (spec_extra[i].value === '') {
            spec_extra[i].value = active300s[index300['Spec']];
            spec_extra[i].readOnly = true;
            index300['Spec'] += 1;
            creditCount300and400['Spec'] += 0.5;
        } 
        i += 1;
    }

    if (i < 4) {
        for (m = 0; m < active400s.length; m++) {
            if (((index400['Spec'] === active400s.length) && (index400['Maj'] === active400s.length)) || (i === 4)) {
                break;
            }
            if ((i < 3) && (maj_extra[i].value === '')) {
                maj_extra[i].value = active400s[index400['Maj']];
                maj_extra[i].readOnly = true;
                index400['Maj'] += 1;
                creditCount300and400['Maj'] += 0.5;
            }
            if ((spec_extra[i].value === '') && (index400['Spec'] < active400s.length)){
                spec_extra[i].value = active400s[index400['Spec']];
                spec_extra[i].readOnly = true;
                index400['Spec'] += 1;
                creditCount300and400['Spec'] += 0.5;
            }
            i += 1;
        }
    }

}


/**
 * Autofills textboxes and updates category for Inquiry courses
**/
function fillMisc() {
    'use-strict'

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
        categories_completed['Spec'] += 1;
    } if (maj_inq[0].value != '') {    
        updateCategory($('#maj_misc')[0].getElementsByClassName('code')[0], 'fulfilled'); 
        categories_completed['Maj'] += 1;
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
    'use-strict'

    specCount = creditCountSpec  + creditCount300and400['Spec'];
    majCount = creditCountMaj + creditCount300and400['Maj'];
    minCount = creditCountMin + (creditCount300and400['Min'] * 0.5);


    fillSpecCreditCount(specCount);
    fillMajCreditCount(majCount);
    fillMinCreditCount(minCount);
}

/**
 *
**/
function fillSpecCreditCount(specCount) {
    if (specCount >= 12) {
        $('#spec_creds').html('(12.0/12.0)');
    } else {
        $('#spec_creds').html('(' + (specCount).toFixed(1) + '/12.0)');
    }
}

/**
 *
**/
function fillMajCreditCount(majCount) {
    if (majCount >= 8) {
        $('#maj_creds').html('(8.0/8.0)')
    } else {
        $('#maj_creds').html('(' + (majCount).toFixed(1) + '/8.0)');
    }
}

/**
 *
**/
function fillMinCreditCount(minCount) {
    if (minCount >= 4) {
        $('#min_creds').html('(4.0/4.0)')
    } else {
        $('#min_creds').html('(' + (minCount).toFixed(1) + '/4.0)');
    }
}

/**
 *
**/
function addExtraMinCourses(index, min300s) {
    for (var m = 0; m < 3; m++) {
        if(index === 3) {
            break;      
        } if (getCookie(additional_min_200s[m]) === 'active') {
            min300s[index].value = additional_min_200s[m];
            min300s[index].readOnly = true;
            creditCount300and400['Min'] += 0.5;
            index += 1;
        }
        
    }
}

/**
 *
**/
function checkPostCompleted() {
    if (categories_completed['Spec'] === 17) {
        $('#spec_creds').css('color', 'green');
    } else {
        $('#spec_creds').css('color', 'red');
    }
    
    if (categories_completed['Maj'] === 13) {
        $('#maj_creds').css('color', 'green');
    } else {
        $('#maj_creds').css('color', 'red');
    } 

    if (categories_completed['Min'] === 6) {
        $('#min_creds').css('color', 'green');
    } else {
        $('#min_creds').css('color', 'red');
    }
}

/**
 *
**/
function updateNavPost() {
    var nav_post = $('#nav-links')[0].getElementsByTagName('li')[3].getElementsByTagName('a')[0];

    if (getCookie('specialist') === 'active') {
        nav_post.innerHTML = 'Check My POSt! (' + (specCount).toFixed(1) + '/12.0)';
        setCookie('activecount', (specCount).toFixed(1));
    } if (getCookie('major') === 'active') {
        nav_post.innerHTML = 'Check My POSt! (' + (majCount).toFixed(1) + '/8.0)';
        setCookie('activecount', (majCount).toFixed(1));
    } if (getCookie('minor') === 'active') {
        nav_post.innerHTML = 'Check My POSt! (' + (minCount).toFixed(1) + '/4.0)';
        setCookie('activecount', (minCount).toFixed(1));
    } 
}

/**
 *
**/
function updateNavGraph() {
    var nav_graph = $('#nav-links')[0].getElementsByTagName('li')[3].getElementsByTagName('a')[0];

    if (getCookie('specialist') === 'active') {
        nav_graph.innerHTML = 'Check My POSt! (' + getCookie('activecount') + '/12.0)';
    } if (getCookie('major') === 'active') {
        nav_graph.innerHTML = 'Check My POSt! (' + getCookie('activecount') + '/8.0)';
    } if (getCookie('minor') === 'active') {
        nav_graph.innerHTML = 'Check My POSt! (' + getCookie('activecount') + '/4.0)';
    } 
}
