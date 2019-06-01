import React from "react";
import Graph from "../Graph";
import { render, wait } from "react-testing-library";

export default class TestGraph {
  constructor(graph) {
    if (typeof graph === "undefined") {
      throw new Error(
        "Cannot call constructor directly. Please call `await TestGraph.build()`"
      );
    }
    this.graph = graph;
  }
  static async build() {
    const graphProps = {
      edit: false,
      initialDrawMode: "draw-node",
      initialOnDraw: false,
      start_blank: false
    };

    const graph = render(<Graph {...graphProps} />);
    await wait(() => graph.queryByText("AAA100") !== null);
    return new TestGraph(graph);
  }

  getNodeByText(text) {
    return this.graph.getByText(text).parentNode;
  }
}
