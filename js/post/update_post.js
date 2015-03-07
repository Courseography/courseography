var activeCourses = [];
var specialist = {'index300': 0, 'index400': 0, 'categoriesCompleted': 0, 'filledTextboxes300': 0, 'filledTextboxes400': 0, 
                  'filledTextboxesExtra': 0, 'specCount': 0, 'reqs': ['CSC108', 'CSC148', 'CSC165', 'CSC207', 'CSC209', 'CSC236',
                  'CSC258', 'CSC263', 'Sta1', 'Lin1', 'Calc1', 'CSC369', 'CSC373'], 'texboxes300': 3, 'textboxes400': 3, 'textboxesExtra': 4};
var major = {'index300': 0, 'index400': 0, 'categoriesCompleted': 0, 'filledTextboxes300': 0, 'filledTextboxes400': 0, 
             'filledTextboxesExtra': 0, 'majCount': 0, 'reqs': ['CSC108', 'CSC148', 'CSC165', 'CSC207', 'CSC236',
             'CSC258', 'CSC263', 'Sta1', 'Calc1'], 'texboxes300': 2, 'textboxes400': 1, 'textboxesExtra': 4};
var minor = {'index300': 0, 'index400': 0, 'index200': 0, 'categoriesCompleted': 0, 'filledTextboxes300': 0, 'filledTextboxes400': 0, 
             'filledTextboxesExtra': 0, 'filledTextboxes200': 0, 'minCount': 0, 'reqs': ['CSC108', 'CSC148', 'CSC165', 'CSC207', 'CSC236'],
             'textboxesExtra': 3};


/**
 * Updates POSts when button is clicked.
**/
$('#update').click(function (e) {
    'use strict';

    updateAllCategories();
    //updateNavPost();
});


/**
 * Updates all categories to see if they are fulfilled or not.
**/
function updateAllCategories() {
    'use strict';

    resetValues();

    updateActiveCourses();

    fill400s();
    fill300s();
    //fillMisc();
    fillExtra();
    //fillCreditCount();

    updateReqsCategory(specialist, 'spec');
    updateReqsCategory(major, 'maj');
    updateReqsCategory(minor, 'min');

    update300Categories();
    console.log('300 done!')
    update400Categories();
     console.log('400 done!')
    updateExtraCategories();
     console.log('300 done!')

    checkPostCompleted();
}

/**
 * Resets all values to initial starting values
**/
function resetValues() {
    'use strict';

    activeCourses = [];
    specialist = {'index300': 0, 'index400': 0, 'categoriesCompleted': 0, 'filledTextboxes300': 0, 'filledTextboxes400': 0, 
                  'filledTextboxesExtra': 0, 'specCount': 0, 'reqs': ['CSC108', 'CSC148', 'CSC165', 'CSC207', 'CSC209', 'CSC236',
                  'CSC258', 'CSC263', 'Sta1', 'Lin1', 'Calc1', 'CSC369', 'CSC373'], 'textboxes300': 3, 'textboxes400': 3, 'textboxesExtra': 4};
    major = {'index300': 0, 'index400': 0, 'categoriesCompleted': 0, 'filledTextboxes300': 0, 'filledTextboxes400': 0, 
             'filledTextboxesExtra': 0, 'majCount': 0, 'reqs': ['CSC108', 'CSC148', 'CSC165', 'CSC207', 'CSC236',
             'CSC258', 'CSC263', 'Sta1', 'Calc1'], 'textboxes300': 2, 'textboxes400': 1, 'textboxesExtra': 3};
    minor = {'index300': 0, 'index400': 0, 'index200': 0, 'categoriesCompleted': 0, 
             'filledTextboxesExtra': 0, 'filledTextboxes200': 0, 'minCount': 0, 'reqs': ['CSC108', 'CSC148', 'CSC165', 'CSC207', 'CSC236'],
             'textboxesExtra': 3};
}


/**
 * Updates categories for required courses in each POSt
 * @param {string} post The POSt that you are updating categories for.
**/
function updateReqsCategory(post, name) {
    'use strict';

    for (var i = 0; i < post.reqs.length; i++) {
        var category = $('#' + name + '_' + post.reqs[i].toLowerCase())[0].getElementsByClassName('code')[0];
        if (activeCourses.indexOf(post.reqs[i]) != -1) {
            activateCourse(post.reqs[i]);
            updateCategory(category, 'fulfilled');
            post.categoriesCompleted += 1;
        } else { // if the category is not completed
            deactivateCourse(post.reqs[i]);
            updateCategory(category, 'not fulfilled');
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
 * Updates Credit Count for each POSt.
 **/
 /*
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
        nav_post.innerHTML = 'Check My POSt! (' + specCount.toFixed(1) + '/12.0)';
        setCookie('activecount', specCount.toFixed(1));
    } else if (getCookie('major') === 'active') {
        nav_post.innerHTML = 'Check My POSt! (' + majCount.toFixed(1) + '/8.0)';
        setCookie('activecount', majCount.toFixed(1));
    } else if (getCookie('minor') === 'active') {
        nav_post.innerHTML = 'Check My POSt! (' + minCount.toFixed(1) + '/4.0)';
        setCookie('activecount', minCount.toFixed(1));
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
