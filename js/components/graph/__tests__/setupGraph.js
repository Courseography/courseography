import React from 'react';
import Graph from "../Graph";
import { render, wait } from "react-testing-library";

/**
 * @param {function} done - provided by Jest to indicate the completion of an async operation
 * @returns {Graph}
 */
export default async function setupGraph() {
    const graphProps = {
        edit: false,
        initialDrawMode: "draw-node",
        initialOnDraw: false,
        start_blank: false
    };

    const graph = render(<Graph {...graphProps} />);
    await wait(() => graph.queryByText("AAA100") !== null);
    return graph;
}
