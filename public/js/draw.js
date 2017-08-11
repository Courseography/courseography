requirejs([
    'es6!graph/react_graph',
    'es6!draw/draw',
    'es6!draw/sidebar'
    ],
    function (
        reactGraph,
        draw,
        sidebar
    ) {



$(document).ready(function () {
    var graphComponent = reactGraph.renderReactGraph('react-graph', true, true);
    ReactDOM.render(React.createElement(sidebar.Sidebar),
        document.getElementById('side-panel-wrap'));
});

//     $(document).ready(function () {
//         'use strict';
//         var graphComponent = reactGraph.renderReactGraph('react-graph', true, true);
//
//         svgDoc = $('#react-graph svg')[0];
//         svgDoc.addEventListener('mousemove', draw.moveNodeElbow, false);
//         svgDoc.addEventListener('mouseup', draw.unclickAll, false);
//     })
//
//     }
// )

});
