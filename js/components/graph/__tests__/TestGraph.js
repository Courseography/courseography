import React from "react";
import Graph from "../Graph";
import { render, wait } from "@testing-library/react";

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
      start_blank: false,
      currFocus: null,
      graphName: "Computer Science"
    };

    const rtlGraph = render(<Graph {...graphProps} />);
    await wait(() => rtlGraph.queryByText("AAA100") !== null);
    return new TestGraph(rtlGraph);
  }

  /**
   * @param {string} text
   * @return {DOM Element} -  for example, SVGElement
   */
  getNodeByText(text) {
    return this.rtlGraph.getByText(text).parentNode;
  }

  /**
   * @param {string} testId - value of the "data-testid" attribute
   * @returns {DOM Element}
   */
  getByTestId(testId) {
    return this.rtlGraph.getByTestId(testId);
  }

  /**
   * @param {regex or string} text
   * @returns {boolean} - whether the exact text exists on the webpage
   */
  textExists(text) {
    return this.rtlGraph.queryByText(text) !== null;
  }
}
