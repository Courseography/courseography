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
    
    var m;
    
    m= time.indexOf('E');
    
    if (m != -1) {
        time = time.slice(0,m) + time.charAt(m+1);
    }

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

    var n;
    var m;

    n = time.indexOf('H');
    m = time.indexOf('E');
        
    if (n != -1 && $(time).attr('clicked') !== 'true') {
        compressCell(time.slice(2,n), time.charAt(1), time.charAt(n+1));
    }
    
    if (m != -1) {
        compressCell(time.slice(2,m), time.charAt(1), time.charAt(m+1));
        time = time.slice(0,m) + time.charAt(m+1);
    }
    
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
    var m;

    n = time.indexOf('H');
    m = time.indexOf('E');

    if (n != -1) {
        extendCell(time.slice(2,n), time.charAt(1), time.charAt(n+1));
        ptime = time.slice(0,n) + time.charAt(n+1);
        
        if ($(time).attr('clicked') !== 'true') {
            $(time).html(section.courseName)
                   .attr('hover', 'good');
        } else if ($(time).html() === section.courseName &&
                   $(time).attr('type') === section.type) {
            $(time).attr('hover', 'remove');
        } else if ($(ptime).attr('clicked') === 'true') {
            $(time).attr('hover', 'conflict');
        } else {
            $(time).attr('hover', 'conflict');
        }
        
    } else if (m != -1) {
        extendCell(time.slice(2,m), time.charAt(1), time.charAt(m+1));
        ptime = time.slice(0,m) + time.charAt(m+1);
        
        if ($(ptime).attr('clicked') !== 'true') {
            $(ptime).html(section.courseName)
                   .attr('hover', 'good');
        } else if ($(ptime).html() === section.courseName &&
                   $(ptime).attr('type') === section.type) {
            $(ptime).attr('hover', 'remove');
        } else {
            $(ptime).attr('hover', 'conflict');
        }
        
    } else {
        
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
}

/**
 * Extends a given cell to display half hour sections.
 * @param {string} time The full hour time of the row.
 * @param {string} week The week of the timetable cell.
 * @param {string} term The term of the timetable cell.
 */
function extendCell(time, week, term) {
    'use strict';

    var pcell;
    var ccell;

    pcell = '#' + week + time + term;
    ccell = '#' + week + time + term;

    $(pcell).attr('rowspan', '1');
    $(ccell).attr('display', 'table-cell');

}

/**
 * Compress a given cell to hide half hour sections.
 * @param {string} time The full hour time of the row.
 * @param {string} week The week of the timetable cell.
 * @param {string} term The term of the timetable cell.
 */
function compressCell(time, week, term) {
    'use strict';

    var pcell;
    var ccell;

    pcell = '#' + week + time + term;
    ccell = '#' + week + time + term;

    $(pcell).attr('rowspan', '2');
    $(ccell).attr('display', 'none');

}
