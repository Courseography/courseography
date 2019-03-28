import React from "react";
import { shallow } from "enzyme";

import InfoBox from "../InfoBox";

describe("InfoBox", () => {
  it("should match shallow snapshot", () => {
    const infoBoxProps = {
      onClick: jest.fn(),
      onMouseDown: jest.fn(),
      onMouseLeave: jest.fn()
    };
    const component = shallow(<InfoBox {...infoBoxProps} />);
    expect(component).toMatchSnapshot();
  });
});
