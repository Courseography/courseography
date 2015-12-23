requirejs([
    'es6!graph/react_node'],
    function (
        reactNode
    ) {

    $(document).ready(function () {
        var graphComponent = reactNode.renderReactGraph();

        // Set sidebar onclick. Eventually move this into its own React component.
        $('.graph-button').click(function () {
            var id = $(this).data('id');
            graphComponent.getGraph(id);
            changeFocusEnable(id);
        });
    });
});