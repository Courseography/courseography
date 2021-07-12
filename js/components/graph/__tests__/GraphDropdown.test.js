
import React from "react";
import { shallow } from "enzyme";
import GraphDropdown from "../GraphDropdown";

describe("GraphDropdown", () => {
  it("should match shallow snapshot", () => {
    const graphDropdownProps = {
			onMouseMove: jest.fn(),
			onMouseLeave: jest.fn(),
			showGraphDropdown:false,
			graphs: [],
			updateGraph: jest.fn()
    };
    const component = shallow(<GraphDropdown {...graphDropdownProps} />);
    expect(component).toMatchSnapshot();
  });
	/*
	TODO: Implement tests for "appearing when hovering over the graph tab", "disappearing a second after the cursor isn't hovered on the dropdown",
	and "disappearing a second after the cursor isn't hovered on the graph tab"
	*/
});