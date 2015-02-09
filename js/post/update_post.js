/**
    Checks if any courses under the course code clicked are changed.
**/
$('.code').click (function (e) {
   'use-strict';

    e.preventDefault();
    var children = $(this).parent().children('.more-info').children();
    for (i = 0; i < children.length; i++) {
        var index = children[i].className.indexOf("CSC");
        var courseCode = children[i].className.substring(index, children[i].className.length);
        if (getCookie(courseCode) === 'active') {
            activateCourse(courseCode);
        } else {
            deactivateCourse(courseCode);
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