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

  it("When focuses are clicked, focuses should be active", async() => {
    const sidebar = await TestSidebar.build();
    fireEvent.click(sidebar.getByTestId("test-sidebar-button"));
    fireEvent.click(sidebar.getByTestId("test-focuses-nav"));
    expect(sidebar.getByTestId("test-focuses-nav").classList.contains("active")).toBe(true);
    expect(sidebar.getByTestId("test-focus-buttons").classList.contains("hidden")).toBe(false);
  });

  it("Focuses should be disabled for non computer science graphs", async() => {
    const container = await TestContainer.build();
    fireEvent.click(container.getByTestId("test-sidebar-button"));
    expect(container.getByTestId("test-focuses-nav").classList.contains("disabled")).toBe(false);
    fireEvent.click(container.getByTestId("test-graph-1"));
    expect(container.getByTestId("test-focuses-nav").classList.contains("disabled")).toBe(true);
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
