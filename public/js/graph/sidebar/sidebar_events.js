var filenames = {'CSC': 'CSC/csc_graph.svg', 'STA': 'STA/sta_graph.svg'};

/**
 * The click function when a focus is clicked.
**/
$('.focus').click(function(e){
    'use strict';

    var id = $(this).attr('id');

    if ($('#' + id + '-details').css('height') === '180px') {
        clearFocus();
        $('ellipse.spotlight').remove();
        setMouseCallbacks();
        $('#' + id + '-details').animate({height: '2px'}, 'fast');
        $('#' + id + '-details').html("");
    } else {
        $('.details').css('height', '2px');
        updateActiveFocus(id);
        $('#' + id + '-details').animate({height: '180px'}, 'fast');
        $('#' + id + '-details').html(window[id + 'Description']);
    } 
});


/**
 * Dynamically creates buttons for each graph in the sidebar.
 * @param {JSON[]} An array of JSON objects representing graphs in the database
**/
function createGraphButtons(graphs) {
    'use strict';

    for (var i = 0; i < graphs.length; i++) {
        var graphTitle = graphs[i].graph_title.toLowerCase();
        var graphButton = '<div id = "graph-' + graphTitle +'" class = "graph-button">';
        $('#graphs').append(graphButton);
        $('#graph-' + graphTitle).html(graphTitle.toUpperCase());
    }
}


/**
 * Click function to load graph, based on button clicked in sidebar.
**/
$('div').on('click', 'div.graph-button', function() {
    'use strict';

    var id = $(this).attr('id');
    loadGraph(id);
});


/**
 * Loads a Graph
 * @param{string} id ID name of graph button clicked
**/
function loadGraph(id) {
    'use-strict';
    
    var graph = id.substring(6, id.length);

    // Remove current graph
    $('#graph').empty();

    getRemote(graph.toUpperCase() + '/' + graph + '_graph.svg');

    FCEPrerequisiteCourses = [csc318, csc454];

    buildGraph();

    // Set mouse callbacks
    setMouseCallbacks();

    // Initialize interface
    initializeGraphSettings();
    
    // Update credit count in nav bar
    updateNavGraph();

    fillFCECount();

    // Uncomment to enable the feedback form (must also be displayed in html)
    // activateFeedbackForm();
    // Uncomment to enable graph dragging
    // enableGraphDragging();
};


/**
 * Grabs all the graphs currently parsed into the database
**/
function getGraphsInDatabase() {
    'use strict';

    var graphs;
    $.ajax({
        url: 'graphs',
        dataType: 'json',
        async: false,
        success: function (data) {
            var graphs = data;
            createGraphButtons(graphs);
        },
        error: function () {
            throw 'No graphs in database';
        }
    });
}
