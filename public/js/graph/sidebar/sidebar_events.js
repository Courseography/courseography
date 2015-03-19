var filenames = {'CSC': 'CSC/csc_graph.svg', 'STA': 'STA/sta_graph.svg'};
var gIDs = {'CSC': 1, 'STA': 2};

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
**/
function createGraphButtons() {
    for (graph in filenames) {
        if (filenames.hasOwnProperty(graph)) {
            var graphButton = '<div id = "graph-' + graph.toLowerCase() +'" class = "graph-button">';
            $('#graphs').append(graphButton);
            $('#graph-' + graph.toLowerCase()).html(graph);
        }
    }
}


/**
 * Click function to load graph, based on button clicked in sidebar.
**/
$('div').on('click', 'div.graph-button', function() {
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
