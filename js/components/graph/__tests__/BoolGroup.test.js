import React from "react";
import { shallow } from "enzyme";

import BoolGroup from "../BoolGroup";

describe("BoolGroup", () => {
  it("BoolGroup should match shallow snapshot", () => {
    const boolGroupProps = {
      boolsJSON: [],
      egesJSON: [],
      svg: null
    };
    const component = shallow(<BoolGroup {...boolGroupProps} />);
    expect(component).toMatchSnapshot();
  });
});
