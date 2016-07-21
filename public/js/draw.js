requirejs([
    'es6!graph/react_graph',
    'es6!draw/draw'
    ],
    function (
        reactGraph,
        draw
    ) {

    $(document).ready(function () {
        'use strict';
        var graphComponent = reactGraph.renderReactGraph('react-graph', true, true);

        svgDoc = $('#react-graph svg')[0];
        // svgDoc.addEventListener('mousedown', draw.makeNodePath, false);
        svgDoc.addEventListener('mousemove', draw.moveNodeElbow, false);
        svgDoc.addEventListener('mouseup', draw.unclickAll, false);
    })

    }
)