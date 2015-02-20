// Objects to store how many courses in each category have been completed
var completed_spec = {'CSC108': 0, 'CSC148': 0, 'CSC165': 0, 'Calc1': 0, 'CSC207': 0, 'CSC209': 0, 
                      'CSC236': 0, 'CSC258': 0, 'CSC263': 0, 'Sta1': 0, 'Lin1': 0, 'CSC369': 0, 'CSC373': 0};

var completed_maj = {'CSC108': 0, 'CSC148': 0, 'CSC165': 0, 'Calc1': 0, 'CSC207': 0, 'CSC236': 0, 
                     'CSC258': 0, 'CSC263': 0, 'Sta1': 0};

var completed_min = {'CSC108': 0, 'CSC148': 0, 'CSC165': 0, 'CSC236': 0}; 

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

activeInq = [];
active400s = [];
active300s = [];
var index400 = 0;
creditCountSpec = 0;
creditCountMaj = 0;
creditCountMin = 0;
creditCount300 = 0;
creditCount400 = 0;

/**
 * Updates POSts when button is clicked.
**/
$('#update').click(function (e) {
    'use-strict';

    updateAllCategories();
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
    'use-strict';

    updateCompletedMinCourses();
    updateCompletedMajCourses();
    updateCompletedSpecCourses();
    update300s();
    update400s();
    fill300s();
    fill400s();
    fillMisc();
    updateCreditCount();

    // Update Specialist 
    for (var property in completed_spec) {
        if (completed_spec.hasOwnProperty(property)) {
            var category = $('#spec_' + property.toLowerCase())[0].getElementsByClassName('code')[0];
            if (completed_spec[property] === 1) { // if the category is completed
                activateCourse(property);
                updateCategory(category, 'fulfilled');
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
        updateCategory($('#maj_300')[0].getElementsByClassName('code')[0], 'fulfilled');
        updateCategory($('#min_misc')[0].getElementsByClassName('code')[0], 'fulfilled');

    } else {
        updateCategory($('#spec_300')[0].getElementsByClassName('code')[0], 'not fulfilled');
        updateCategory($('#maj_300')[0].getElementsByClassName('code')[0], 'not fulfilled');
        updateCategory($('#min_misc')[0].getElementsByClassName('code')[0], 'not fulfilled');
        
    }

    // Update 400s

    i = 0; 
    var spec400s = $('.lvl400spec');
    for (var l = 0; l < 3; l++) {
        if (spec400s[l].value != '') {
            i += 1;
        }
    }

    if (i === 3) {
        updateCategory($('#spec_400')[0].getElementsByClassName('code')[0], 'fulfilled');
        updateCategory($('#maj_400')[0].getElementsByClassName('code')[0], 'fulfilled');
        updateCategory($('#min_misc')[0].getElementsByClassName('code')[0], 'fulfilled');
    } else {
        updateCategory($('#spec_400')[0].getElementsByClassName('code')[0], 'not fulfilled');
        updateCategory($('#maj_400')[0].getElementsByClassName('code')[0], 'not fulfilled');
        updateCategory($('#min_misc')[0].getElementsByClassName('code')[0], 'not fulfilled');
    }


}


/**
 * Records a course as clicked. 
 * @param {string} courseCode The course code
**/
function activateCourse(courseCode) {
    'use-strict';
    
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
    'use-strict';
    
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
    'use-strict';

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
    'use-strict';

    var i = 0; 
    var index300 = 0;
    var spec300s = $('.lvl300spec');
    var maj300s = $('.lvl300maj');
    var min300s = $('.lvl300min');

    
    // clear textboxes
    for (k = 0; k < 3; k++) {
        var course = spec300s[k].value;
        if (getCookie(course) === 'inactive' || getCookie(course) === 'takeable') {
            spec300s[k].value = '';
            maj300s[k].value = '';
            min300s[k].value = '';
        }
    }
    

    // fill courses that have been selected
    for (var m = 0; m < 3; m++) {
        spec300s[i].value = active300s[index300];
        maj300s[i].value = active300s[index300];
        min300s[i].value = active300s[index300];
        i += 1; 
        index300 += 1;
        if (index300 === active300s.length) {
            break;
        }
    }

    if (i < 3) {
        for (var m = 0; m < 3; m++) {
            spec300s[i].value = active400s[index400];
            maj300s[i].value = active400s[index400];
            min300s[i].value = active400s[index400]; 
            i += 1;
            index400 += 1;
            if ((index400 === active400s.length) || (i === 3)) {
                break;
            }
        }
    }
}  


/**
 * Autofills textboxes for 400 level courses. 
**/
function fill400s() {
    'use-strict';

    var i = 0; 
    var spec400s = $('.lvl400spec');
    var maj400s = $('.lvl400maj');
    var min400s = $('.lvl400min');

    
    // clear textboxes
    for (k = 0; k < 3; k++) {
        spec400s[k].value = '';
        maj400s[k].value = '';
        min400s[k].value = '';
    }
    

    // fill courses that have been selected
    for (var m = 0; m < active400s.length; m++) {
        spec400s[i].value = active400s[i];
        maj400s[i].value = active400s[i];
        min400s[i].value = active400s[i]; 
        i += 1;
        if (i === 3) {
            break;
        }
    }
}
 


/**
 * Autofills textboxes and updates category for Inquiry courses
**/
function fillMisc() {
    'use-strict';

    var i = 0;

    var spec_inq = $('#spec_misc')[0].getElementsByTagName('input');
    var maj_inq = $('#maj_misc')[0].getElementsByTagName('input');

    // clear textboxes
    for (k = 0; k < 4; k++) {
        spec_inq[k].value = '';
        maj_inq[k].value = '';
    }
    
    for (m = 0; m < activeInq.length; m++) {
        spec_inq[m].value = activeInq[m];
        maj_inq[m].value = activeInq[m];
        i += 1;
        if (i == 4) {
            break;
        }
    }

    if (activeInq.length >= 4) {
        updateCategory($('#spec_misc')[0].getElementsByClassName('code')[0], 'fulfilled'); 
        updateCategory($('#maj_misc')[0].getElementsByClassName('code')[0], 'fulfilled'); 
    } else {
        updateCategory($('#spec_misc')[0].getElementsByClassName('code')[0], 'not fulfilled');
        updateCategory($('#maj_misc')[0].getElementsByClassName('code')[0], 'not fulfilled');
    }

}


