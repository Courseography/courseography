requirejs([
    'es6!graph/react_node'],
    function (
        reactNode
    ) {

    $(document).ready(function () {
        'use strict';
        var graphComponent = reactNode.renderReactGraph();

        // Set sidebar onclick. Eventually move this into its own React component.
        $('.graph-button').click(function () {
            var id = $(this).data('id');
            graphComponent.getGraph(id);
            changeFocusEnable(id);
        });

        // Set focus button onclicks
        $('.focus').click(function(e) {
            var id = $(this).attr('id');
            var focusDetails = $('#' + id + '-details');

            if (graphComponent.state.highlightedNodes == window[id + 'FocusList']) {
                graphComponent.setState({highlightedNodes: []});
                focusDetails.animate({height: '2px'}, 'fast');
            } else {
                $('.details').css('height', '2px');
                focusDetails.animate({height: '128px'}, 'fast');
                focusDetails.html(window[id + 'Description']);
                graphComponent.setState({highlightedNodes: window[id + 'FocusList']});
            }
        });

        // Enable Reset button
        $('#reset').click(function () {
            graphComponent.reset();
        })
    });
});