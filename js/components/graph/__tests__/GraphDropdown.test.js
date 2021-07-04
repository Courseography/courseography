
import React from "react";
import { shallow } from "enzyme";
import GraphDropdown from "../GraphDropdown";
import TestGraph from "./TestGraph";
import { fireEvent } from "@testing-library/react";
import TestContainer from "./TestContainer";

describe("GraphDropdown", () => {
  it("should match shallow snapshot", () => {
    const graphDropdownProps = {
			onMouseEnter: jest.fn(),
			onMouseLeave: jest.fn(),
			showGraphDropdown:false,
			graphs: [],
			updateGraph: jest.fn()
    };
    const component = shallow(<GraphDropdown {...graphDropdownProps} />);
    expect(component).toMatchSnapshot();
  });

	it("should appear when hovering over the graph tab", async () => {
		// Don't think I can do any of the tests around hovering over graph tab, since
		// graph tab isn't represented in react?
		const container = await TestContainer.build();
		const graphNav = container.getByText("Graph");
		const graphDropdown  = container.getByTestId("test-graph-dropdown");
		expect(graphDropdown.classList.contains("graph-dropdown-display")).toBe(true);
		fireEvent.mouseOver(graphNav);
		expect(graphDropdown.classList.contains("graph-dropdown-display")).toBe(true);
	});

	it("should disappear a second after the cursor isn't hovered on the dropdown", async () => {
		const graph = await TestGraph.build();
		const graphDropdown = graph.getByTestId("test-graph-dropdown");
		fireEvent.mouseOver(graphDropdown);
		expect(graphDropdown.classList.contains("graph-dropdown-display")).toBe(true);
		fireEvent.mouseOut(graphDropdown);
    setTimeout(() => {
      expect(graphDropdown.classList.contains("graph-dropdown-hidden")).toBe(true);
    }, 1000);
	});
	it("should disappear a second after the cursor isn't hovered on the graph tab", async () => {
		const graph = await TestGraph.build();
		const graphDropdown = graph.getByTestId("test-graph-dropdown");
		const graphNav = graph.getNodeByText("Graph");
		fireEvent.mouseOver(graphNav);
		expect(graphDropdown.classList.contains("graph-dropdown-display")).toBe(true);
		fireEvent.mouseOut(graphNav);
    setTimeout(() => {
      expect(graphDropdown.classList.contains("graph-dropdown-hidden")).toBe(true);
    }, 1000);

	});
});