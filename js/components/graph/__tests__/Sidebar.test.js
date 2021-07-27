import { fireEvent} from "@testing-library/react";
import TestSidebar from "./TestSidebar";
import TestContainer from "./TestContainer";

describe("Sidebar", () => {
  it("should match shallow snapshot", async () => {
    const sidebar = await TestSidebar.build();
    expect(sidebar).toMatchSnapshot();
  });

  it("Pressing on the sidebar button should open it", async () => {
    const sidebar = await TestSidebar.build();
    expect(sidebar.getByTestId("test-sidebar").classList.contains("opened")).toBe(false);
    fireEvent.click(sidebar.getByTestId("test-sidebar-button"));
    expect(sidebar.getByTestId("test-sidebar").classList.contains("opened")).toBe(true);
  });

  it("Clicking a graph button should increase the FCE Count", async () => {
    const container = await TestContainer.build();
    expect(container.getByText("FCE Count: 0.0")).toBeDefined();
    fireEvent.click(container.getByTestId("aaa100"));
    expect(container.getByText("FCE Count: 0.5")).toBeDefined();
    fireEvent.click(container.getByTestId("aaa101"));
    expect(container.getByText("FCE Count: 1.0")).toBeDefined();
    fireEvent.click(container.getByTestId("aaa101"));
    expect(container.getByText("FCE Count: 0.5")).toBeDefined();
  });

  it("Clicking the reset button should reset the FCE Count to 0.0", async () => {
        const container = await TestContainer.build();
        expect(container.getByText("FCE Count: 0.0")).toBeDefined();
        fireEvent.click(container.getByTestId("aaa100"));
        expect(container.getByText("FCE Count: 0.5")).toBeDefined();
        fireEvent.click(container.getByTestId("test-reset"));
        expect(container.getByText("FCE Count: 0.0")).toBeDefined();
      });
});
