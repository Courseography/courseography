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
        timetable_util.restoreFromLocalStorage();
        timetable_util.enableSearch();
        timetable_util.getVeryLargeCourseArray();
        trap_scroll.trap_scroll();
        setTdHover();
        react_util.initGrid();

        react_generate_grid.drawTimetable();
    });
});
