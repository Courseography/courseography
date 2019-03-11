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

  it("Hovering over a course node should make a info box appear", () => { });
  it("Pressing on the info box should create a new pop up", () => { });
});
