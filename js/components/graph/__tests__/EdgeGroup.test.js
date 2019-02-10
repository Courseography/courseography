import React from "react";
import { shallow } from "enzyme";

import EdgeGroup from "../EdgeGroup";

describe("EdgeGroup", () => {
  it("should match shallow snapshot", () => {
    const edgeGroupProps = {
      edgesJSON: [],
      svg: {}
    };
    const component = shallow(<EdgeGroup {...edgeGroupProps} />);
    expect(component).toMatchSnapshot();
  });
});
