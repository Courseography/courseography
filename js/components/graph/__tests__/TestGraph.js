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
    this.rtlGraph = graph;
  }

  /**
   * For async construction of the TestGraph
   * @return {TestGraph}
   */
  static async build() {
    const graphProps = {
      edit: false,
      initialDrawMode: "draw-node",
      initialOnDraw: false,
      start_blank: false
    };

    const rtlGraph = render(<Graph {...graphProps} />);
    await wait(() => rtlGraph.queryByText("AAA100") !== null);
    return new TestGraph(rtlGraph);
  }

  /**
   * @param {string} text
   * @returns {DOM Element}
   */
  getNodeByText(text) {
    return this.rtlGraph.getByText(text).parentNode;
  }

  /**
   * @param {string} testId - value of the "data-testId" attribute
   * @returns {DOM Element}
   */
  getByTestId(testId) {
    return this.rtlGraph.getByTestId(testId);
  }
}
