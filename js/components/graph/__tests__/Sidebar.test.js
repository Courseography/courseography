import { fireEvent, waitForDomChange } from "@testing-library/react";
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
  
  it("Focus buttons should be hidden when graphs are active", async() => {
    const sidebar = await TestSidebar.build();
    fireEvent.click(sidebar.getByTestId("test-sidebar-button"));
    expect(sidebar.getByTestId("test-graphs-nav").classList.contains("active")).toBe(true);
    expect(sidebar.getByTestId("test-graph-buttons").classList.contains("hidden")).toBe(false);
    expect(sidebar.getByTestId("test-focuses-nav").classList.contains("active")).toBe(false);
    expect(sidebar.getByTestId("test-focus-buttons").classList.contains("hidden")).toBe(true);
  });

  it("Graph buttons should be hidden when focuses are active", async() => {
    const sidebar = await TestSidebar.build();
    fireEvent.click(sidebar.getByTestId("test-sidebar-button"));
    fireEvent.click(sidebar.getByTestId("test-focuses-nav"));
    expect(sidebar.getByTestId("test-graphs-nav").classList.contains("active")).toBe(false);
    expect(sidebar.getByTestId("test-graph-buttons").classList.contains("hidden")).toBe(true);
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

  it("Clicking on a graph button should change the graph", async () => {
    const container = await TestContainer.build();
    fireEvent.click(container.getByTestId("test-sidebar-button"));
    expect(container.getByText("AAA100")).toBeDefined();
    fireEvent.click(container.getByTestId("test-graph-1"));
    // Need to wait for props update in Graph component
    await waitForDomChange(() => {
      expect(container.getByText("BBB100")).toBeDefined();
      expect(container.getByText("AAA100")).toBeNull();
    })
  });
});
