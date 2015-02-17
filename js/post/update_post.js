// Objects to store how many courses in each category have been completed
var completed_spec = {'CSC108': 0, 'CSC148': 0, 'CSC165': 0, 'Calc1': 0, 'CSC207': 0, 'CSC209': 0, 
                      'CSC236': 0, 'CSC258': 0, 'CSC263': 0, 'Sta1': 0, 'Lin1:': 0, 'CSC369': 0, 'CSC373': 0};

var completed_maj = {'CSC108': 0, 'CSC148': 0, 'CSC165': 0, 'Calc1': 0, 'CSC207': 0, 'CSC236': 0, 
                     'CSC258': 0, 'CSC263': 0, 'Sta1': 0};

var completed_min = {'CSC108': 0, 'CSC148': 0, 'CSC165': 0, 'CSC236': 0}; 

/**
    Checks if any courses under the course code clicked are changed.
**/
$('.code').click (function (e) {
   'use-strict';
    
    // update 300 level and 400 level text boxes - in progress
    /*
    if ($(this).parent().attr('id') === 'spec_300' || $(this).parent().attr('id') === 'spec_400') {
        update300sAnd400s(this);
    }
    */
    
    e.preventDefault();
    var children = $(this).parent().children('.more-info').children();

    for (i = 0; i < children.length; i++) {
        if (children[i].className.indexOf('CSC') > 0) {
            var index = children[i].className.indexOf('CSC');
        } if (children[i].className.indexOf('Lin') > 0) {
            var index = children[i].className.indexOf('Lin');
        } if (children[i].className.indexOf('Calc') > 0) {
            var index = children[i].className.indexOf('Calc');
        } if (children[i].className.indexOf('Sta1') > 0) {
            var index = children[i].className.indexOf('Sta1');
        }
        
        var courseCode = children[i].className.substring(index, children[i].className.length);
        if (getCookie(courseCode) === 'active' || getCookie(courseCode) === 'overridden') {
            activateCourse(courseCode);
            if (completed_min[courseCode] < 1 && completed_maj[courseCode]  < 1 && 
                completed_spec[courseCode] < 1) {
                    completed_spec[courseCode] += 1;
                    completed_maj[courseCode] += 1;
                    completed_min[courseCode] += 1;
            }
        } else {
            deactivateCourse(courseCode);
            completed_spec[courseCode] -= 1;
            completed_maj[courseCode] -= 1;
            completed_min[courseCode] -= 1;
        }
    }
});

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
 * Updates all categories to see if they are fulfilled or not.
**/
function updateAllCategories() {

    // Update Specialist
    for (var property in completed_spec) {
        if (completed_spec.hasOwnProperty(property)) {
            var category = $('#spec_' + property.toLowerCase())[0].getElementsByClassName('code')[0];
            if (completed_spec[property] === 1) { // if the category is completed
                updateCategory(category, 'fulfilled');
            } else { // if the category is not completed
                updateCategory(category, 'not fulfilled');
            }
        }
    }

    // Update Major
    for (var property in completed_maj) {
        if (completed_maj.hasOwnProperty(property)) {
            var category = $('#maj_' + property.toLowerCase())[0].getElementsByClassName('code')[0];
            if (completed_maj[property] === 1) { // if the category is completed
                updateCategory(category, 'fulfilled');
            } else { // if the category is not completed
                updateCategory(category, 'not fulfilled');
            }
        }
    }

    // Update Minor
    for (var property in completed_min) {
        if (completed_min.hasOwnProperty(property)) {
            var category = $('#min_' + property.toLowerCase())[0].getElementsByClassName('code')[0];
            if (completed_min[property] === 1) { // if the category is completed
                updateCategory($('#min_' + property.toLowerCase())[0], 'fulfilled');
            } else { // if the category is not completed
                updateCategory($('#min_' + property.toLowerCase())[0], 'not fulfilled');
            }
        }
    }
}


/**
 * Records a category as fulfilled or not fulfilled
 * @param {Element} category Element of category
 * @param {string} status Whether it is 'fulfilled' or 'not fulfilled'
**/
function updateCategory(category, status) {
    if (status === 'fulfilled') {
        category.style.backgroundColor = "#3CB371";
    } else if (status === 'not fulfilled') {
        category.style.backgroundColor = '#ebe8e4';
    }
}

/**
 * Updates textbox in 300 or 400 level category
 * @param {object} obj Object that was clicked
**/
function update300sAnd400s(obj) {
    var textboxes = $(obj).parent().children('more-info').children();
    var i = 0;
    while (textboxes[i].value) {
        i = i + 1;
    }
    //textboxes[i].value = ;
    
}   