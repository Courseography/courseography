import React from "react";
import { shallow } from "enzyme";

import NodeGroup from "../NodeGroup";

describe("NodeGroup", () => {
  it("should match shallow snapshot", () => {
    const props = {
      edgesJSON: [],
      highlightedNodes: [],
      hybridsJSON: [],
      nodeClick: jest.fn(),
      nodeMouseDown: jest.fn(),
      nodeMouseEnter: jest.fn(),
      nodeMouseLeave: jest.fn(),
      nodesJSON: [],
      onDraw: false,
      svg: null 
    };
    const component = shallow(<NodeGroup {...props} />);
    expect(component).toMatchSnapshot();
  });
});
