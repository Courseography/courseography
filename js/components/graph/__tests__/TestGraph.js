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

  getNodeByText(text) {
    return this.rtlGraph.getByText(text).parentNode;
  }

  /**
   * 
   */
  getByTestId(testId) {
    return this.rtlGraph.getByTestId(testId);
  }

  /**
   * The only unique identifier for a path is the d attributes.
   *
   * @param {string} name - name of one of the edges, one of the keys in `edgeLocation`
   *
   * @returns {DOM Path Element} - <path> HTML DOM element that corresponds to the provided `name`
   * @throws {Error} if the provided name could not be found in `graph`
   */
  getPath(name) {
    const edgeLocation = {
      "h101-201": "M626.794148,68.11810799999999 626.794148,80.89120799999999 ",
      "201-and":
        "M659.307448,103.86880799999999 765.952148,103.86880799999999 765.952148,131.888008 ",
      "102-and": "M515,70 515,144 748,144 ",
      "and-303": "M765.952148,155 765.952148,171 ",
      "102-or": "M515,70 540,90 ",
      "201-or": "M592,100 570,100 ",
      "or-202": "M550,100 550,173 "
    };

    if (!(name in edgeLocation)) {
      throw new Error(
        `Provided Path name "${name}" does not exist in the edgeLocation mapping! There could be a typo in the name. If the edge is new in the test data, please add it to edgeLocation in getPath() in Edge.test.js!`
      );
    }

    // HTMLCollection doesn't have a forEach
    const paths = this.rtlGraph.container.getElementsByTagName("path");
    for (let i = 0; i < paths.length; i += 1) {
      if (paths[i].getAttribute("d") === edgeLocation[name]) {
        return paths[i];
      }
    }
    throw new Error(
      `Couldn't find the edge ${name} in graph. The test data may have been updated and you should update edgeLocation in getPath() in Edge.test.js!`
    );
  }
}
