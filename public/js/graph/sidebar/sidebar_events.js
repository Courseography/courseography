/**
 * The click function when we click the reset button.
 */
$('#reset').click(function () {
    'use strict';

    reset();
    enableReset();
});

/**
 * The click function when a focus is clicked.
 */
$('.focus').click(function(e) {
    'use strict';

    var id = $(this).attr('id');
    var focusDetails = $('#' + id + '-details');

    if (focusDetails.text() !== '') {
        clearFocus();
        $('ellipse.spotlight').remove();
        setMouseCallbacks();
        focusDetails.animate({height: '2px'}, 'fast');
        focusDetails.html('');
    } else {
        $('.details').css('height', '2px');
        updateActiveFocus(id);
        focusDetails.animate({height: '128px'}, 'fast');
        focusDetails.html(window[id + 'Description']);
    }
});


/**
 * Dynamically creates buttons for each graph in the sidebar.
 */
function createGraphButtons() {
    'use strict';

    for (var i = 0; i < graphs.length; i++) {
        var graphId = graphs[i].id;
        var graphTitle = graphs[i].title;
        var graphButton = '<div id = "graph-' + graphId +'" class = "graph-button">';
        $('#graphs').append(graphButton);
        $('#graph-' + graphId).html(graphTitle);
        $('#graph-' + graphId).data('id', graphs[i].id);
    }
}


/**
 * Click function to load graph, based on button clicked in sidebar.
 */
$(document).on('click', '.graph-button', function() {
    'use strict';

    var id = $(this).data('id');
    loadGraph(id);
    changeFocusEnable(id);
});


/**
 * Loads a Graph
 * @param{string} id ID of graph in database
 */
function loadGraph(id) {
    'use strict';

    setCookie('active-graph', id);

    // Remove current graph
    $('#graph').empty();

    getRemote('static/res/graphs/gen/' + id + '.svg');

    // Only create this if CSC graph loaded
    if (getCookie('active-graph') === '1') {
        FCEPrerequisiteCourses = [csc318, csc454];
    }

    buildGraph(id);

    // Set mouse callbacks
    setMouseCallbacks();

    // Initialize interface
    initializeGraphSettings();

    fillFCECount();

    // Uncomment to enable the feedback form (must also be displayed in html)
    // activateFeedbackForm();
    // Uncomment to enable graph dragging
    // enableGraphDragging();
}


/**
 * Grabs all the graphs currently parsed into the database
 */
function getGraphsInDatabase() {
    'use strict';

    $.ajax({
        url: 'graphs',
        dataType: 'json',
        async: false,
        success: function (data) {
            graphs = data;
        },
        error: function () {
            throw 'No graphs in database';
        }
    });
}

/**
 * Determines whether Reset button should be greyed out or not
 * (If there are active FCEs or not)
 */
function enableReset() {
    'use strict';

    fillFCECount();
}

/**
 * Enables the Focuses nav in the sidebar if the CS graph is selected.
 * @param:{string} id ID of the graph we just selected
**/
function changeFocusEnable(id) {
    var currentGraph = graphs[id - 1].title;
    if (currentGraph.indexOf("Computer Science") >= 0)
    {
        $("#focuses-nav").removeClass('disabled');
    }
    else
    {
        $("#focuses-nav").addClass('disabled');
    }
}
