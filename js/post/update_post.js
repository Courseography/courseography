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

    var active = 0;
    var inactive = 0;
    
    e.preventDefault();
    var children = $(this).parent().children('.more-info').children();
    for (i = 0; i < children.length; i++) {
        
        if (children[i].className.indexOf("CSC") != 0) {
           var index = children[i].className.indexOf("CSC");
        } else if (children[i].className.indexOf("Lin") != 0) {
           var index = children[i].className.indexOf("Lin");
        } else if (children[i].className.indexOf("Mat") != 0) {
           var index = children[i].className.indexOf("Mat");
        } 
        
        var courseCode = children[i].className.substring(index, children[i].className.length);
        if (getCookie(courseCode) === 'active' || getCookie(courseCode) === 'overridden') {
            activateCourse(courseCode);
            active += 1;
        } else {
            deactivateCourse(courseCode);
            inactive += 1;
        }
    }
    
    // Changes category to show it is satisfied - in progress
    /*
    var elem = children[0].parentNode.parentNode;
    if (active === children.length) {
        updateCategory(elem, 'fulfilled');
    } else if (inactive > 0) {
        updateCategory(elem, 'not fulfilled');
    }*/
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