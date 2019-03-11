import React from "react";
import { shallow } from "enzyme";

import Bool from "../Bool";

describe("Bool", () => {
  it("should match shallow snapshot", () => {
    const boolProps = {
      JSON: {
        fill: "",
        graph: 1,
        height: 14.7368002,
        id: "bool1",
        pos: [1020.015148, 243.51310800000002],
        stroke: "",
        text: [
          {
            align: "begin",
            fill: "",
            graph: 1,
            pos: [1012.903868, 246.655709],
            rID: "text512",
            text: "and"
          }
        ],
        type_: "BoolNode",
        width: 19.7600002
      },
      childs: ["csc369", "csc358", "csc458", "csc385", "csc367"],
      className: "bool",
      inEdges: ["p39", "p40"],
      logicalType: "and",
      outEdges: ["p41", "p42", "p43", "p59", "p82"],
      parents: ["csc209", "csc258"],
      svg: {}
    };
    const wrapper = shallow(<Bool {...boolProps} />);
    expect(wrapper).toMatchSnapshot();
  });

  it("should not do anything when you hover or click on it", () => { });
  it("should become selected when its pre-req parent is satisfied", () => { });
});
