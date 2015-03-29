
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
        var graphTitle = graphs[i].title.toLowerCase();
        var graphButton = '<div id = "graph-' + graphTitle +'" class = "graph-button">';
        $('#graphs').append(graphButton);
        $('#graph-' + graphTitle).html(graphTitle.toUpperCase());
        $('#graph-' + graphTitle).data('id', graphs[i].gId);

    }
}


/**
 * Click function to load graph, based on button clicked in sidebar.
**/
$('div').on('click', 'div.graph-button', function() {
    'use strict';

    var name = $(this).attr('id');
    var id = $(this).data('id');
    loadGraph(name, id);
});


/**
 * Loads a Graph
 * @param{string} name ID name of graph button clicked
 * @param{string} id ID of graph in database
**/
function loadGraph(name, id) {
    'use-strict';
    
    var graph = name.substring(6, name.length);

    // Remove current graph
    $('#graph').empty();

    getRemote(graph.toUpperCase() + '/' + id + '.svg');

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

    $.ajax({
        url: 'graphs',
        dataType: 'json',
        async: false,
        success: function (data) {
            var graphs = data;
            console.log(graphs);
            createGraphButtons(graphs);
        },
        error: function () {
            throw 'No graphs in database';
        }
    });
}
