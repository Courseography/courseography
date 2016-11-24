requirejs([
    'grid/generate_grid',
    'grid/timetable_util',
    'common/utilities/trap_scroll',
    'es6!common/export/export'
    ],
    function(
        generate_grid,
        timetable_util,
        trap_scroll,
        exp
    ) {

    $(document).ready(function () {
        'use strict';

        generate_grid.generateGrid();
        var tdObjects = $('td');
        tdObjects.each(function () {
            $(this).data('conflicts', []);
        });

        timetable_util.restoreFromCookies();
        timetable_util.renderClearAllButton();
        timetable_util.enableSearch();
        timetable_util.getVeryLargeCourseArray();
        trap_scroll.trap_scroll();
        setTdHover();
    });
});