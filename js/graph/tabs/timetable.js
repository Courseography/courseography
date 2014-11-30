/**
 * Creates the timetable.
 */
function createTimetable() {
    'use strict';

    $.ajax(timetable).done(function (response) {
            $('#timetableContainer').html(response);
    });
}


/**
 * Initializes timetable search.
 */
function createTimetableSearch() {
    'use strict';

    $('#filter').keyup(function () {
        var filter = $(this).val();
        $('.searchClass').each(function () {
            if ($(this).text().search(new RegExp(filter, "i")) < 0) {
                $(this).fadeOut();
            } else {
                $(this).show();
            }
        });
    });
}