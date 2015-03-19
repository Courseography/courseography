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
 * Click function to load CSC graph
**/
$('#graph-csc').click(function(e) {
    'use-strict';

    // Remove current graph
    $('#graph').empty();

    getRemote('CSC/csc_graph.svg');

    buildGraph();

    FCEPrerequisiteCourses = [csc318, csc454];

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
});


/**
 * Click function to load Statistics Graph
 * NOTE: A dummy graph is loaded at the moment
**/
$('#graph-sta').click(function(e) {
    'use strict';

    // Remove current graph
    $('#graph').empty();

    getRemote('STA/dummy_graph.svg');

    buildGraph();

    //setMouseCallbacks();

    //initializeGraphSettings();

});