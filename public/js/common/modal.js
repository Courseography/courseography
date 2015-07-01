/**
 * Creates and returns the main modal content div.
 * @param {string} courseCode The course code.
 * @returns {jQuery} The main modal content div.
 */
function createModalDiv(courseCode) {
    'use strict';

    var contentDiv = $('<div></div>');
    var names = formatCourseName(courseCode);
    var courseVideoUrls = getCourseVideoUrls(names);

    contentDiv.attr('id', 'modal-content-container');

    addCourseDescriptionToModal(contentDiv, courseCode);
    addTimeslotToModal(contentDiv, courseCode);
    displayCourseVideo(courseVideoUrls, contentDiv);

    return contentDiv;
}


/**
 * Adds the timeslot to the modal.
 * @param contentDiv
 * @param courseCode
 */
function addTimeslotToModal(contentDiv, courseCode) {
    var timetable = setupTimeslot(courseCode);
    contentDiv.append(timetable);
}

/**
 * Adds the course description to the modal.
 * @param contentDiv
 * @param courseCode
 */
function addCourseDescriptionToModal(contentDiv, courseCode) {
    var courseDescription = fetchCourseDescription(courseCode);
    var p = $('<p></p>').html(courseDescription);
    contentDiv.append(p);
}


/**
 * Returns a formatted course timeslot.
 * @param {string} courseCode The course code.
 * @returns {jQuery}
 */
function setupTimeslot(courseCode) {
    'use strict';

    var courseName;
    var timeslot = $('<div></div>');
    var title = $('<h3></h3>');

    timeslot.append(title);
    timeslot.append($('#timetableMain').children('tbody')
                                       .children('tr')
                                       .first()
                                       .clone());

    $('.searchClass').each(function () {
        courseName = $(this).children('td').first().html();
        if (courseName.indexOf(courseCode) > -1) {
            timeslot.append($(this).clone());
        }
    });

    timeslot.attr('style', 'width: 920px; margin: 0 auto;');

    timeslot.children('.searchClass').children('td').first().remove();
    timeslot.children('tr').children('td').first().remove();
    return timeslot;
}


/**
 * Opens a modal.
 * @param {String} title The title of the modal.
 * @param {jQuery} modalDiv The div that is opened as a modal.
 */
function openModal(title, modalDiv) {
    'use strict';

    var context = $('#courseography-header').attr('context');

    if ($('.modal').length === 0) {
        modalDiv.attr('title', title)
                .addClass('modal').dialog({
                    autoOpen: true,
                    modal: true,
                    width: 750,
                    height: 400,
                    closeText: 'X',
                    open: function(event, ui) {
                        $('.ui-widget-overlay').bind('click', function () {
                            modalDiv.dialog('close');
                        }); },
                    close: function () {
                        $(this).remove();
                        if (context === 'graph') {
                            lightUpGraph();
                        }
                    }});

        if (context === 'graph') {
            dimGraph();
        }
    }
}


/**
 * Lights up the graph.
 */
function lightUpGraph() {
    'use strict';

    $.each(nodes, function (index, elem) {
        window[elem].updateSVG();
    });
    $('body').css('background', 'rgb(255,255,255)');
}


/**
 * Dims the graph.
 */
function dimGraph() {
    'use strict';

    $('.node, .hybrid').attr('data-active', 'unlit');
    $('body').css('background', 'rgb(40,40,40)');

    $('.tooltip-group').remove();
}
