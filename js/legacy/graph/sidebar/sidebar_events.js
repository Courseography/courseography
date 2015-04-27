'use strict';

import $ from 'jquery';

import { getCookie, setCookie } from '../../common/cookie_handler';
import { getRemote } from '../setup';
import { buildGraph } from '../parse_graph';
import { setMouseCallbacks } from '../mouse_events';
import { initializeGraphSettings } from '../utilities/util';
import { fillFCECount } from './sidebar_divs';
import { updateActiveFocus, clearFocus } from './focuses';

/**
 * The click function when we click the reset button.
**/
$('#reset').click(function () {
    'use strict';

    reset();
    enableReset();
})

/**
 * The click function when a focus is clicked.
**/
$('.focus').click(function(e) {
    'use strict';

    var id = $(this).attr('id');

    if ($('#' + id + '-details').text() !== '') {
        clearFocus();
        $('ellipse.spotlight').remove();
        setMouseCallbacks();
        $('#' + id + '-details').animate({height: '2px'}, 'fast');
        $('#' + id + '-details').html('');
    } else {
        $('.details').css('height', '2px');
        updateActiveFocus(id);
        $('#' + id + '-details').animate({height: '128px'}, 'fast');
        $('#' + id + '-details').html(global[id + 'Description']);
    }
});


/**
 * Dynamically creates buttons for each graph in the sidebar.
 * @param {JSON[]} An array of JSON objects representing graphs in the database
**/
export function createGraphButtons(graphs) {
    'use strict';

    for (var i = 0; i < graphs.length; i++) {
        var graphTitle = graphs[i].title;
        var graphButton = '<div id = "graph-' + graphTitle +'" class = "graph-button">';
        $('#graphs').append(graphButton);
        $('#graph-' + graphTitle).html(graphTitle);
        $('#graph-' + graphTitle).data('id', graphs[i].gId);
    }
}


/**
 * Click function to load graph, based on button clicked in sidebar.
**/
$(document).on('click', '.graph-button', function() {
    'use strict';

    var id = $(this).data('id');
    loadGraph(id);
});


/**
 * Loads a Graph
 * @param{string} id ID of graph in database
**/
export function loadGraph(id) {
    'use strict';

    setCookie('active-graph', id);

    // Remove current graph
    $('#graph').empty();

    getRemote('static/res/graphs/' + id + '.svg');

    // Only create this if CSC graph loaded
    if (getCookie('active-graph') === '1') {
        global.FCEPrerequisiteCourses = [csc318, csc454];
    }

    buildGraph();

    // Set mouse callbacks
    setMouseCallbacks();

    // Initialize interface
    initializeGraphSettings();

    fillFCECount();

    // Uncomment to enable the feedback form (must also be displayed in html)
    // activateFeedbackForm();
    // Uncomment to enable graph dragging
    // enableGraphDragging();
};


/**
 * Grabs all the graphs currently parsed into the database
**/
export function getGraphsInDatabase() {
    'use strict';

    $.ajax({
        url: 'graphs',
        dataType: 'json',
        async: false,
        success: function (data) {
            createGraphButtons(data);
        },
        error: function () {
            throw 'No graphs in database';
        }
    });
}

/**
 * Determines whether Reset button should be greyed out or not
 * (If there are active FCEs or not)
**/
export function enableReset() {
    'use strict';

    if (FCEs > 0) {
        $('#reset').attr('disabled', false);
    } else {
        $('#reset').attr('disabled', true);
    }

    fillFCECount();

}
