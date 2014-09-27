/* global $ */
// All Global Variables for the Grid
//TODO: Are all of these used?
var trapScroll;             // Disables ability to scroll parent div.
var courses;                //
var courseCache = [];       // All Courses that have been previously requested.
var selectedCourses = [];   // All selected Courses.
var selectedSections = [];  // All selected sections.
var courseObjects = [];     // All selected course JSON files.


/**
 * Sets up the page.
 */
$(document).ready(function () {
    'use strict';

    $('#dialog').fadeOut()
                .css('visibility', 'visible');

    generateGrid();
    var tdObjects = $('td');
    tdObjects.each(function () {
            $(this).data('conflicts', []);
        });

    restoreFromCookies();
    renderClearAllButton();
    enableSearch();
    courses = getVeryLargeCourseArray();
    trapScroll();
    setTdHover();
});


/**
 * Renders the button that allows the user to deselect
 * all selected courses.
 */
function renderClearAllButton() {
    'use strict';

    var clearAllItem = document.getElementById('clear-all');
    $(clearAllItem).click(function () {
        if (confirm('Clear all selected courses?')) {
            $.each(courseObjects.slice(0), function (i, course) {
                removeCourseFromList(course.name);
            });
        }
    });
}


/**
 * Adapted from http://codepen.io/LelandKwong/pen/edAmn.
 */
(function($) {
    trapScroll = function () {
        var trapElement;
        var scrollableDist;
        var trapClassName = 'trapScroll-enabled';
        var trapSelector = '#course-select';

        var trapWheel = function(e){
            if (!$('body').hasClass(trapClassName)) {
                return;
            } else {
                var curScrollPos = trapElement.scrollTop();
                var wheelEvent = e.originalEvent;
                var dY = wheelEvent.deltaY;

                // only trap events once we've scrolled to the end
                // or beginning
                if ((dY>0 && curScrollPos >= scrollableDist) ||
                    (dY<0 && curScrollPos <= 0)) {
                    return false;
                }
            }
        };

        $(document)
            .on('wheel', trapWheel)
            .on('mouseleave', trapSelector, function() {
                $('body').removeClass(trapClassName);
            })
            .on('mouseenter', trapSelector, function() {
                trapElement = $(this);
                var containerHeight = trapElement.outerHeight();
                var contentHeight = trapElement[0].scrollHeight; // height of scrollable content
                scrollableDist = contentHeight - containerHeight;

                if (contentHeight > containerHeight) {
                    $('body').addClass(trapClassName);
                }
            });
    };
})($);
