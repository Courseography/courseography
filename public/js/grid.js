requirejs([
    'grid/generate_grid',
    'grid/timetable_util',
    'common/utilities/trap_scroll',
    'es6!common/export/export',
    'es6!grid/react_util',
    'es6!grid/react_generate_grid'
    ],
    function(
        generate_grid,
        timetable_util,
        trap_scroll,
        exp,
        react_util,
        react_generate_grid
    ) {

    $(document).ready(function () {
        'use strict';
        /* This commented code generates the grid using plain Javascript,
          whereas react_generate_grid does the same thing using React) */
        // generate_grid.generateGrid();
        // var tdObjects = $('td');
        // tdObjects.each(function () {
        //     $(this).data('conflicts', []);
        // });

        timetable_util.restoreFromLocalStorage();
        timetable_util.renderClearAllButton();
        timetable_util.enableSearch();
        timetable_util.getVeryLargeCourseArray();
        trap_scroll.trap_scroll();
        setTdHover();
        react_util.initGrid();

        react_generate_grid.drawTimetable();
    });
});
