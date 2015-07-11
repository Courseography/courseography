requirejs(['grid/generate_grid'], function(g) {
    //This function is called when scripts/helper/util.js is loaded.
    //If util.js calls define(), then this function is not fired until
    //util's dependencies have loaded, and the util argument will hold
    //the module value for "helper/util".
    $(document).ready(function () {
        'use strict';

        $('#dialog').fadeOut()
                    .css('visibility', 'visible');

        g.generateGrid();
        var tdObjects = $('td');
        tdObjects.each(function () {
            $(this).data('conflicts', []);
        });

        restoreFromCookies();
        renderClearAllButton();
        enableSearch();
        getVeryLargeCourseArray();
        trapScroll();
        setTdHover();
    });
});