import React from "react";
import { shallow } from "enzyme";
import TestGraph from "./TestGraph";

import Edge from "../Edge";

describe("Edge", () => {
  it("should match shallow snapshot", () => {
    const edgeProps = {
      className: "path",
      edgeID: "p1",
      points: [[497.651848, 69.09890799999998], [497.651848, 130.885308]],
      source: "csc165240",
      target: "csc236240",
      updateEdgeStatus: null,
      svg: {}
    };
    const component = shallow(<Edge {...edgeProps} />);
    expect(component).toMatchSnapshot();
  });

  describe("Clicking course nodes", () => {
    it("unselected source and destination => 'inactive' Edge", async () => {
      const graph = await TestGraph.build();
      const path_h101_201 = graph.getPath("h101-201");
      expect(path_h101_201.classList.contains("inactive")).toBe(true);
    });
  });
});
