/**
 * Generates the duplex timetable grid.
 */
function generateGrid() {
    'use strict';

    var rowDiv = $('<div></div>').addClass('row');
    var timetableContainerDivFall = createTimeTableContainer();
    var timetableContainerDivSpring = createTimeTableContainer();
    var timetableTableFall = createTimeTable('F');
    var fallThead = $('<thead></thead>');
    var timetableTableSpring = createTimeTable('S');
    var springThead = $('<thead></thead>');

    timetableTableFall.append(fallThead);
    timetableTableSpring.append(springThead);

    appendHeaders(fallThead, springThead);
    appendTableRows(timetableTableFall, timetableTableSpring);

    timetableContainerDivFall.append(timetableTableFall);
    timetableContainerDivSpring.append(timetableTableSpring);

    rowDiv.append(timetableContainerDivFall)
          .append(timetableContainerDivSpring)
          .insertBefore($('#info'));
}


/**
 * Creates a timetable.
 * @param {string} idSuffix The suffix of the ID of the timetable.
 * @returns {jQuery}
 */
function createTimeTable(idSuffix) {
    'use strict';

    return $('<table></table>')
        .addClass('timetable table')
        .attr('id', 'timetable-' + idSuffix);
}


/**
 * Creates and returns a timetable container HTML div element.
 * @returns {jQuery}
 */
function createTimeTableContainer() {
    'use strict';

    return $('<div></div>')
        .addClass('col-md-6 col-xs-12 timetable-container');
}


/**
 * Appends table headers to the timetables.
 * @param {jQuery} fallThead The fall table header.
 * @param {jQuery} springThead The spring table header.
 */
function appendHeaders(fallThead, springThead) {
    'use strict';

    var days = ['Mon', 'Tue', 'Wed', 'Thu', 'Fri'];

    fallThead.append($('<th></th>')
        .addClass('timetable-dummy-cell'));

    fallThead.append($('<th></th>')
        .addClass('term-name')
        .html('Fall'));

    for (var j = 0; j < 5; j++) {
        fallThead.append($('<th></th>').html(days[j]));
        springThead.append($('<th></th>').html(days[j]));
    }

    springThead.append($('<th></th>')
        .addClass('term-name')
        .html('Spring'));

    springThead.append($('<th></th>')
        .addClass('timetable-dummy-cell'));
}


/**
 * Appends all table rows to the timetables.
 * @param {jQuery} timetableTableFall The fall timetable.
 * @param {jQuery} timetableTableSpring The spring timetable.
 */
function appendTableRows(timetableTableFall, timetableTableSpring) {
    'use strict';

    for (var i = 8; i < 22; i++) {
        var trFall = $('<tr></tr>');
        var trSpring = $('<tr></tr>');
        appendTableData(trFall, trSpring, i);
        timetableTableSpring.append(trSpring);
        timetableTableFall.append(trFall);
        trFall = $('<tr></tr>');
        trSpring = $('<tr></tr>');
        appendTableData(trFall, trSpring, i + 0.5);
        timetableTableSpring.append(trSpring);
        timetableTableFall.append(trFall);
    }
}


/**
 * Appends table cells to table rows.
 * @param {jQuery} trFall The fall table row.
 * @param {jQuery} trSpring The spring table row.
 * @param {int} time The table row's time.
 */
function appendTableData(trFall, trSpring, time) {
    'use strict';

    var weekPrefixArray = ['M', 'T', 'W', 'R', 'F'];

    if (time % 1 === 0) {
        var adjustedTime = (time === 12 ? 12 : time % 12) + ':00';

        trFall.append($('<td></td>')
            .addClass('timetable-dummy-cell'));

        trFall.append($('<td></td>')
           .addClass('timetable-time')
           .attr('rowspan', '2')
           .html(adjustedTime));

        for (var k = 0; k < 5; k++) {
            trFall.append($('<td></td>')
                .attr('id', weekPrefixArray[k] + time + '-0F')
                .attr('in-conflict', 'false')
                .attr('satisfied', 'true')
                .attr('rowspan', '2')
                .addClass('timetable-cell'));
            trSpring.append($('<td></td>')
                .attr('id', weekPrefixArray[k] + time + '-0S')
                .attr('in-conflict', 'false')
                .attr('satisfied', 'true')
                .attr('rowspan', '2')
                .addClass('timetable-cell'));
        }

        trSpring.append($('<td></td>')
            .addClass('timetable-time')
            .attr('rowspan', '2')
            .html(adjustedTime));

        trSpring.append($('<td></td>')
            .addClass('timetable-dummy-cell'));

    } else {
        var adjustedTime = '';

        trFall.append($('<td></td>')
            .addClass('timetable-dummy-cell'));

        for (var k = 0; k < 5; k++) {
            trFall.append($('<td></td>')
                .attr('id', weekPrefixArray[k] + time.toString().replace('.', '-') + 'F' + 'H')
                .attr('in-conflict', 'false')
                .attr('satisfied', 'true')
                .attr('rowspan', '1')
                .addClass('timetable-cell')
                .addClass('timetable-half-cell'));
            trSpring.append($('<td></td>')
                .attr('id', weekPrefixArray[k] + time.toString().replace('.', '-') + 'S' + 'H')
                .attr('in-conflict', 'false')
                .attr('satisfied', 'true')
                .attr('rowspan', '1')
                .addClass('timetable-cell')
                .addClass('timetable-half-cell'));
        }

        trSpring.append($('<td></td>')
            .addClass('timetable-dummy-cell'));
    }

}
