import React from "react";
import ReactDOM from "react-dom";
import Graph from '../graph/Graph';


document.addEventListener('DOMContentLoaded', () => {
    return ReactDOM.render(
        <Graph start_blank={true} edit={true} initialDrawMode="draw-node" />,
        document.getElementById('react-graph')
    );
})
