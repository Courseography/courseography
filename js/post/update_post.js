// Objects to store how many courses in each category have been completed
var completed_spec = {'CSC108': 0, 'CSC148': 0, 'CSC165': 0, 'Calc1': 0, 'CSC207': 0, 'CSC209': 0, 
                      'CSC236': 0, 'CSC258': 0, 'CSC263': 0, 'Sta1': 0, 'Lin1': 0, 'CSC369': 0, 'CSC373': 0};

var completed_maj = {'CSC108': 0, 'CSC148': 0, 'CSC165': 0, 'Calc1': 0, 'CSC207': 0, 'CSC236': 0, 
                     'CSC258': 0, 'CSC263': 0, 'Sta1': 0};

var completed_min = {'CSC108': 0, 'CSC148': 0, 'CSC165': 0, 'CSC236': 0}; 

var level300 = {'CSC300': 0, 'CSC301': 0, 'CSC302': 0, 'CSC309': 0, 'CSC310': 0, 
                'CSC318': 0, 'CSC320': 0, 'CSC321': 0, 'CSC324': 0, 'CSC336': 0,
                'CSC343': 0, 'CSC358': 0, 'CSC372': 0, 'CSC384': 0, 'ECE385': 0,
                'BCB410': 0, 'BCB420': 0, 'BCB430': 0};

var level400 = {'CSC401': 0, 'CSC404': 0, 'CSC411': 0, 'CSC412': 0, 'CSC418': 0,
                'CSC420': 0, 'CSC428': 0, 'CSC436': 0, 'CSC438': 0, 'CSC443': 0, 
                'CSC446': 0, 'CSC448': 0, 'CSC454': 0, 'CSC456': 0, 'CSC458': 0,
                'CSC463': 0, 'CSC465': 0, 'CSC469': 0, 'CSC486': 0, 'CSC488': 0,
                'CSC490': 0, 'CSC491': 0, 'CSC494': 0, 'CSC495': 0, 'ECE489': 0,
                'BCB410': 0, 'BCB420': 0, 'BCB430': 0};


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
                } else if ((getCookie(courseCode) === 'inactive' || getCookie(courseCode) === 'takeable')
                      && (completed_maj[courseCode] > 0)) {
                    completed_maj[courseCode] -= 1;
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
                } else if ((getCookie(courseCode) === 'inactive' || getCookie(courseCode) === 'takeable')
                           && (completed_min[courseCode] > 0)) {
                    completed_min[courseCode] -= 1;
                }
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


function update300s() {
    'use-strict';

    for (var courseCode in level300) {
        if (level300.hasOwnProperty(courseCode)) {
            if (getCookie(courseCode) === 'active' || getCookie(courseCode) === 'overridden') {
                if (level300[courseCode] < 1) {
                    level300[courseCode] += 1;
                } if (getCookie(courseCode) === 'inactive' || getCookie(courseCode) === 'takeable') {
                    level300[courseCode] -= 1;
                }
            }       
        }
    }
}

function update400s() {
    'use-strict';

    for (var courseCode in level400) {
        if (level400.hasOwnProperty(courseCode)) {
            if (getCookie(courseCode) === 'active' || getCookie(courseCode) === 'overridden') {
                if (level400[courseCode] < 1) {
                    level400[courseCode] += 1;
                } if (getCookie(courseCode) === 'inactive' || getCookie(courseCode) === 'takeable') {
                    level400[courseCode] -= 1;
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

