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
    expect(sidebar.getByTestId("test-toggle").classList.contains("collapsed")).toBe(true);
    expect(sidebar.getByTestId("test-toggle").classList.contains("expanded")).toBe(false);
    fireEvent.click(sidebar.getByTestId("test-sidebar-button"));
    expect(sidebar.getByTestId("test-toggle").classList.contains("collapsed")).toBe(false);
    expect(sidebar.getByTestId("test-toggle").classList.contains("expanded")).toBe(true);
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

  it("Clicking a graph button adds and removes the course to the Selected Courses", async () => {
    const container = await TestContainer.build();
    fireEvent.click(container.getByTestId("aaa100"));
    const sidebar = await TestSidebar.build();
    expect(sidebar.getByTestId("test AAA100")).toBeDefined();
    fireEvent.click(container.getByTestId("aaa100"));
    expect(sidebar.queryByTestId("test AAA100")).toBeNull();
  });

  it("Clicking graph buttons add and remove the courses to the Selected Courses", async () => {
    const container = await TestContainer.build();
    fireEvent.click(container.getByTestId("aaa100"));
    fireEvent.click(container.getByTestId("aaa303"));
    const sidebar = await TestSidebar.build();
    expect(sidebar.getByTestId("test AAA100")).toBeDefined();
    expect(sidebar.getByTestId("test AAA303")).toBeDefined();
    fireEvent.click(container.getByTestId("aaa100"));
    fireEvent.click(container.getByTestId("aaa303"));
    expect(sidebar.queryByTestId("test AAA100")).toBeNull();
    expect(sidebar.queryByTestId("test AAA303")).toBeNull();
  });

  it("Clicking the reset selections clears the Selected Courses and resets FCE", async () => {
    const container = await TestContainer.build();
    fireEvent.click(container.getByTestId("aaa100"));
    fireEvent.click(container.getByTestId("test-reset"));
    expect(container.getByText("FCE Count: 0.0")).toBeDefined();
    const sidebar = await TestSidebar.build();
    expect(sidebar.queryByTestId("test AAA100")).toBeNull();
  });

});
