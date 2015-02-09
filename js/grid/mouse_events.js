/* Hover functions */
/**
 * Sets the selected section table cell's hover.
 */
function setTdHover() {
    'use strict';

    $('td').mouseover(function () {
        var courseName = $(this).html();
        if (courseName !== '') {
            var course = getCourseObject(courseName, courseObjects);
            if (course !== undefined) {
                $.each(course.getSectionTimes(), function (i, time) {
                    $(time).addClass('hover-time');
                });

                var section = course.selected[$(this).attr('type')];
                renderDisplayCourseInformation(course);
                renderDisplaySectionInformation(section);
            }
        }

    }).mouseout(function () {
        var course = getCourseObject($(this).html(), courseObjects);
        if (course !== undefined) {
            $.each(course.getSectionTimes(), function (i, time) {
                $(time).removeClass('hover-time');
            });

            renderClearCourseInformation();
        }
    });
}


/* Conflicts */
/**
 * Displays the conflicts in the table cell time.
 * @param {string} time
 * @param {string[]} conflicts
 */
function renderConflicts(time, conflicts) {
    'use strict';

    $(time).data('conflicts', conflicts)
           .attr('title', conflicts.map(function (section) {
                              return section.courseName;
                          })
            )
           .attr('in-conflict', String(conflicts.length > 0))
           .attr('status', conflicts.length > 0 ? 'conflict' : 'occupied')
           .attr('satisfied', getCourseObject($(time).html(),
                                              courseObjects).satisfied);

}


/**
 * Returns whether any cell in the table is in conflict.
 * @returns {boolean} Whether any cell in the table is in conflict.
 */
function getInConflict() {
    'use strict';

    return $('td[in-conflict*=true]').length > 0;
}


/**
 * Alerts the user of conflicting selected sections.
 */
function alertUserOfConflict() {
    'use strict';

    var dialogSelector = $('#dialog');
    getInConflict() ? dialogSelector.fadeIn(750) :
                      dialogSelector.fadeOut(750);
}


/* Functions to manipulate grid DOM */

/**
 * Clears a cell in the grid.
 * @param {string} time The time's cell ID.
 */
function renderClearTime(time) {
    'use strict';

    $(time).html('')
           .attr('clicked', 'false')
           .attr('satisfied', 'true')
           .attr('type', '')
           .attr('hover', 'off')
           .attr('status', 'clear');
}


/**
 * Clears a cell's hover.
 * @param {string} time The time's cell ID.
 */
function renderClearHover(time) {
    'use strict';

    if ($(time).attr('clicked') !== 'true') {
            $(time).html('');
        }
    $(time).attr('hover', 'off');
}


/**
 * Renders a cell on hover.
 * @param {string} time The time's cell ID.
 * @param {Section} section The time's occupying Section.
 */
function renderAddHover(time, section) {
    'use strict';

    var n;

    n = time.indexOf('H');

    if (n != -1) {
        extendRow(time.slice(2,n));
    } 

    if ($(time).attr('clicked') !== 'true') {
        $(time).html(section.courseName)
               .attr('hover', 'good');
    } else if ($(time).html() === section.courseName &&
               $(time).attr('type') === section.type) {
        $(time).attr('hover', 'remove');
    } else {
        $(time).attr('hover', 'conflict');
    }
}
/**
 * Extends a given row to display half hour sections.
 * @param {string} time The full hour time of the row.
 */
function extendRow(time) {
    'use strict';

    var weekPrefixArray = ['M', 'T', 'W', 'R', 'F'];
    var pcells = [];
    var ccells = [];
    var cellID;

    for (var k = 0; k < 5; k++) {
        cellID = '#' + weekPrefixArray[k] + time + 'F';
        pcells[pcells.length] = cellID;
        cellID = '#' + weekPrefixArray[k] + time + 'S';
        pcells[pcells.length] = cellID;
        cellID = '#' + weekPrefixArray[k] + time +'H' + 'F';
        ccells[ccells.length] = cellID;
        cellID = '#' + weekPrefixArray[k] + time +'H'+ 'S';
        ccells[ccells.length] = cellID;
    }

    for (var i = 0; i < 10; i++) {
        $(pcells[i]).attr('rowspan', '1');
        $(ccells[i]).attr('display', 'table-cell');
    }

}
