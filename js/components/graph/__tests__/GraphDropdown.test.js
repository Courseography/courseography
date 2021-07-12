import TestContainer from "./TestContainer";
import { fireEvent} from "@testing-library/react";

describe("GraphDropdown", () => {
  // it("should match shallow snapshot", () => {
  //   const graphDropdownProps = {
	// 		onMouseMove: jest.fn(),
	// 		onMouseLeave: jest.fn(),
	// 		showGraphDropdown:false,
	// 		graphs: [],
	// 		updateGraph: jest.fn()
  //   };
  //   const component = shallow(<GraphDropdown {...graphDropdownProps} />);
  //   expect(component).toMatchSnapshot();
  // });
	it("should appear when hovering over the graph tab and be hidden before", async () => {
		const mouseEnter = new MouseEvent("mouseenter", {
			bubbles: false,
			cancelable: false
		});

		const container = await TestContainer.build();
		const graphNav = container.getByText("Graph");
		const graphDropdown  = container.getByTestId("test-graph-dropdown");
		expect(graphDropdown.classList.contains("graph-dropdown-hidden")).toBe(true);
		fireEvent(graphNav, mouseEnter);
		expect(graphDropdown.classList.contains("graph-dropdown-display")).toBe(true);
	});

	it("should disappear a second after the cursor isn't hovered on the dropdown", async () => {
		const mouseEnter = new MouseEvent("mouseenter", {
			bubbles: false,
			cancelable: false
		});

		const container = await TestContainer.build();
		const graphNav = container.getByText("Graph");
		const graphDropdown  = container.getByTestId("test-graph-dropdown");
		expect(graphDropdown.classList.contains("graph-dropdown-hidden")).toBe(true);
		fireEvent(graphNav, mouseEnter);
		expect(graphDropdown.classList.contains("graph-dropdown-display")).toBe(true);

		fireEvent.mouseLeave(graphDropdown);
		expect(graphDropdown.classList.contains("graph-dropdown-hidden")).toBe(true);
	});
	// it("should disappear a second after the cursor isn't hovered on the graph tab", async () => {
	// 	const graph = await TestGraph.build();
	// 	const graphDropdown = graph.getByTestId("test-graph-dropdown");
	// 	const graphNav = graph.getNodeByText("Graph");
	// 	fireEvent.mouseOver(graphNav);
	// 	expect(graphDropdown.classList.contains("graph-dropdown-display")).toBe(true);
	// 	fireEvent.mouseOut(graphNav);
  //   setTimeout(() => {
  //     expect(graphDropdown.classList.contains("graph-dropdown-hidden")).toBe(true);
  //   }, 1000);

	// });
	/*
	TODO: Implement tests for "appearing when hovering over the graph tab", "disappearing a second after the cursor isn't hovered on the dropdown",
	and "disappearing a second after the cursor isn't hovered on the graph tab"
	*/
});