$(document).ready(function () {
    'use strict';

    var urlSpecifiedGraph = getURLParameter('dept');

    // HACK: Temporary workaround for giving the statistics department a link to our graph.
    // Should be replaced with a more general solution.
    var active;
    if (urlSpecifiedGraph === 'sta') {
        active = '2';
    } else if (urlSpecifiedGraph !== null) {
        active = '1';
    } else {
        active = getCookie('active-graph');
    }

    if (active !== '') {
        loadGraph(active);
    } else {
        loadGraph('1');
    }

    $('#fcecount').hide();
});
