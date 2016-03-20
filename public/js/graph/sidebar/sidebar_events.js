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
 * Enables the Focuses nav in the sidebar if the CS graph is selected.
 * @param:{string} id ID of the graph we just selected
**/
function changeFocusEnable(id) {
    var currentGraph = graphs[id - 1].title;
    if (currentGraph.indexOf("Computer Science") >= 0) {
        $("#focuses-nav").removeClass('disabled');
    }
    else {
        $("#focuses-nav").addClass('disabled');
    }
}
