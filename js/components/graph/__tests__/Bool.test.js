import React from "react";
import { shallow } from "enzyme";

import Bool from "../Bool";

describe("Bool", () => {
  it("Bool", () => {
    const BoolProps = {
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
      inEdges: ["p39, p40"],
      logicalType: "and",
      outEdges: ["p41", "p42", "p43", "p59", "p82"],
      parents: ["csc209, csc258"],
      svg: null
    };
    const component = shallow(<Bool {...BoolProps} />);
    expect(component).toMatchSnapshot();
  });
});
