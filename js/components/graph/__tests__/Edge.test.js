import React from "react";
import { shallow } from "enzyme";

import Edge from "../Edge";

describe("Edge", () => {
  it("Edge", () => {
    const EdgeProps = {
      className: "path",
      edgeID: "p1",
      points: [[497.651848, 69.09890799999998], [497.651848, 130.885308]],
      source: "csc165240",
      target: "csc236240",
      updateEdgeStatus: null,
      svg: null
    };
    const component = shallow(<Edge {...EdgeProps} />);
    expect(component).toMatchSnapshot();
  });
});
