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


/**
 * Updates POSts when button is clicked.
**/
$('#update').click(function (e) {
    'use-strict';

    updateAllCategories();
});

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
    fillInquiries();

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
    var spec300s = $('.300lvlspec');
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
    var spec400s = $('.400lvlspec');
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
 * Updates number of completed courses in Specialist.
**/
function updateCompletedSpecCourses () {
    'use-strict';

    for (var courseCode in completed_spec) {
        if (completed_spec.hasOwnProperty(courseCode)) {
            if (getCookie(courseCode) === 'active' || getCookie(courseCode) === 'overridden') {
                if (completed_spec[courseCode] < 1) {
                    completed_spec[courseCode] += 1;
                } 
            } else if ((getCookie(courseCode) === 'inactive' || getCookie(courseCode) === 'takeable')
                       && (completed_spec[courseCode] > 0)) {
                    completed_spec[courseCode] -= 1;
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
                } 
            } else if ((getCookie(courseCode) === 'inactive' || getCookie(courseCode) === 'takeable')
                        && (completed_maj[courseCode] > 0)) {
                completed_maj[courseCode] -= 1;
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
                } 
            } else if ((getCookie(courseCode) === 'inactive' || getCookie(courseCode) === 'takeable')
                           && (completed_min[courseCode] > 0)) {
                completed_min[courseCode] -= 1;
            }
        }       
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
 * Updates number of 300 level category completed courses.
 **/
function update300s() {
    'use-strict';

    for (var courseCode in level300) {
        if (level300.hasOwnProperty(courseCode)) {
            if (getCookie(courseCode) === 'active' || getCookie(courseCode) === 'overridden') {
                if (level300[courseCode] < 1) {
                    level300[courseCode] += 1;
                } if ((CSCinq.indexOf(courseCode) > -1) && (activeInq.indexOf(courseCode) === -1)) { // check if Inquiry Course
                    activeInq.push(courseCode);
                }
            } else if ((getCookie(courseCode) === 'inactive' || getCookie(courseCode) === 'takeable') 
                       && (level300[courseCode] > 0)) {
                level300[courseCode] -= 1;
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
                } if ((CSCinq.indexOf(courseCode) > -1) && (activeInq.indexOf(courseCode) === -1)) { // check if Inquiry Course
                    activeInq.push(courseCode);
                }
            } else if ((getCookie(courseCode) === 'inactive' || getCookie(courseCode) === 'takeable') 
                       && (level400[courseCode] > 0)) {
                level400[courseCode] -= 1;
                var index = activeInq.indexOf(courseCode);
                if (index > -1) {
                    activeInq.splice(index, 1);
                }
            }
        }       
    }
}



/**
 * Autofills textboxes for 300 level courses. 
**/
function fill300s() {
    'use-strict';

    var i = 0; 
    var spec300s = $('.300lvlspec');
    var maj300s = $('.300lvlmaj');
    var min300s = $('.300lvlmin');

    
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
    for (course in level300) {
        if (level300.hasOwnProperty(course)) {
            if (level300[course] === 1) {
                spec300s[i].value = course;
                maj300s[i].value = course;
                min300s[i].value = course; 
                i += 1;
            } if (i === 3) {
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
    var spec400s = $('.400lvlspec');
    var maj400s = $('.400lvlmaj');
    var min400s = $('.400lvlmin');

    
    // clear textboxes
    for (k = 0; k < 3; k++) {
        spec400s[k].value = '';
        maj400s[k].value = '';
        min400s[k].value = '';
    }
    

    // fill courses that have been selected
    for (course in level400) {
        if (level400.hasOwnProperty(course)) {
            if (level400[course] === 1) {
                spec400s[i].value = course;
                maj400s[i].value = course;
                min400s[i].value = course; 
                i += 1;
            } if (i === 3) {
                break;
            }
        }
    }
} 


function fillInquiries() {
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


