// Fetch timetable information
function createTimetable() {
    $.ajax(timetable).done(function (response) {
            $('#timetableContainer').html(response);
    });
}


// Search function for timetable
function createTimetableSearch() {
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