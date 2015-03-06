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

    var n;
    var ctime;

    n = time.charAt(time.length-1);

    if (n === 'E') {
        ctime = time.slice(0);
        time = time.slice(0, time.length-1);
        if ($(time).attr('clicked') !== 'true') {
            compressCell(parseInt(ctime.slice(2)), ctime.charAt(1), ctime.charAt(ctime.length-2));
        }
    }

    if ($(time).attr('clicked') !== 'true') {
        $(time).removeAttr('style');
    }
        
    if (n === 'H' && $(time).attr('clicked') !== 'true') {
        compressCell(parseInt(time.slice(2)), time.charAt(1), time.charAt(time.length-2));
    }
    
    if ($(time).attr('clicked') !== 'true') {
        $(time).html('');
    }

    if ($(time).attr('rowspan') === '1') {
        $(time).css('font-size', '0');
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
    var htime;
    var ptime;

    n = time.charAt(time.length-1);


    if (n === 'H') {
        extendCell(parseInt(time.slice(2)), time.charAt(1), time.charAt(time.length-2));
        ptime = previousCell(time);

        if ($(ptime).attr('clicked') === 'true') {
            $(time).attr('hover', 'conflict');
        }
    }

    if (n === 'E') {
        extendCell(parseInt(time.slice(2)), time.charAt(1), time.charAt(time.length-2));
        time = time.slice(0, time.length-1);
    } 
    
    if ($(time).attr('clicked') !== 'true') {
        if ($(time).attr('rowspan') !== '1') {
            $(time).html(section.courseName.substring(0,6) + ' (' + section.type + ')')
                    .attr('hover', 'good');
        } else {
            $(time).html(section.courseName.substring(0,6) + ' (' + section.type + ')')
                    .attr('hover', 'good')
                    .css('font-size', '0');
        }
    } else if ($(time).html() === section.courseName &&
            $(time).attr('type') === section.type) {
        $(time).attr('hover', 'remove');
    } else {
        $(time).attr('hover', 'conflict');
    }

    if ($(time).attr('rowspan') !== '2'&& n !== 'H' && n !== 'E') {
        $(time).attr('hover', 'conflict');
    }

    ptime = previousCell(time);
    if ($(ptime).html() !== '') {
        $(time).css('border-top-style', 'hidden');
    }
        
}

/**
 * Extends a given row to display half hour sections.
 * @param {Int} time The full hour time of the row.
 * @param {string} term The term of the timetable row.
 */
function extendRow(timeInt, term) {
    'use strict';

    var weekPrefixArray = ['M', 'T', 'W', 'R', 'F'];
    var pcells = [];
    var ccells = [];
    var time = timeInt.toString();

    for (var k = 0; k < 5; k++) {
        pcells[pcells.length] = '#' + weekPrefixArray[k] + time + term;
        ccells[ccells.length] = '#' + weekPrefixArray[k] + time + term + 'H';
    }

    for (var i = 0; i < 5; i++) {
        $(ccells[i]).toggle();
        $(pcells[i]).attr('rowspan', '1');
    }

}

function previousCell(time) {

    var n;
    var ptime;

    n = time.charAt(time.length-1);

    if (n === 'H') {
        ptime = time.slice(0, time.length-1);
        return ptime;

    } else if (n === 'E') {
        ptime = time.slice(0, 2) + String(parseInt(time.slice(2))-1) + time.charAt(time.length-2) + 'H'

        if ($(ptime).css('display') == 'none') {
            ptime = ptime.slice(0, ptime.length-1);
        }
        return ptime;

    } else {
        ptime = time.slice(0, 2) + String(parseInt(time.slice(2))-1) + time.charAt(time.length-1) + 'H'
        
        if ($(ptime).css('display') == 'none') {
            ptime = ptime.slice(0, ptime.length-1);
        }
        return ptime;
    }
}

/**
 * Extends a given cell to display half hour section.
 * @param {Int} time The full hour time of the row.
 * @param {string} day The day of the time.
 * @param {string} term The term of the timetable row.
 */
function extendCell(timeInt, day, term) {
    'use strict';

    var pcell = '#' + day + timeInt + term;
    var ccell = '#' + day + timeInt + term + 'H';

    $(ccell).css('display', 'table-cell');
    $(pcell).attr('rowspan', '1');

}

/**
 * Compress a given cell to hide half hour section.
 * @param {Int} timeInt The full hour time of the row.
 * @param {string} week The week of the timetable cell.
 * @param {string} day The day of the time.
 * @param {string} term The term of the timetable cell.
 */
function compressCell(timeInt, day, term) {
    'use strict';

    var pcell = '#' + day + timeInt + term;
    var ccell = '#' + day + timeInt + term + 'H';

    $(pcell).attr('rowspan', '2');
    $(ccell).css('display', 'none');
}

/**
 * Compress a given row to hide half hour sections.
 * @param {Int} timeInt The full hour time of the row.
 * @param {string} week The week of the timetable cell.
 * @param {string} term The term of the timetable cell.
 */
function compressRow(timeInt, term) {
    'use strict';

    var weekPrefixArray = ['M', 'T', 'W', 'R', 'F'];
    var pcells = [];
    var ccells = [];
    var time = timeInt.toString();

    for (var k = 0; k < 5; k++) {
        pcells[pcells.length] = '#' + weekPrefixArray[k] + time + term;
        ccells[ccells.length] = '#' + weekPrefixArray[k] + time +'H' + term;
    }

    for (var i = 0; i < 5; i++) {
        $(pcells[i]).attr('rowspan', '2');
        $(ccells[i]).css('display', 'none');
    }

}

