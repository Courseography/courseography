import { fireEvent, within, waitFor } from "@testing-library/react"
import TestSidebar from "./TestSidebar"
import TestContainer from "./TestContainer"

describe("Sidebar", () => {
  it("should match shallow snapshot", async () => {
    const sidebar = await TestSidebar.build()
    expect(sidebar).toMatchSnapshot()
  })

  it("Pressing on the sidebar button should open it", async () => {
    const sidebar = await TestSidebar.build()
    expect(sidebar.getByTestId("test-toggle").classList.contains("collapsed")).toBe(
      true
    )
    expect(sidebar.getByTestId("test-toggle").classList.contains("expanded")).toBe(
      false
    )
    fireEvent.click(sidebar.getByTestId("test-sidebar-button"))
    expect(sidebar.getByTestId("test-toggle").classList.contains("collapsed")).toBe(
      false
    )
    expect(sidebar.getByTestId("test-toggle").classList.contains("expanded")).toBe(true)
  })

  it("Clicking a graph button should increase the FCE Count", async () => {
    const container = await TestContainer.build()
    expect(container.getByText("FCE Count: 0.0")).toBeDefined()
    fireEvent.click(container.getByTestId("aaa100"))
    expect(container.getByText("FCE Count: 0.5")).toBeDefined()
    fireEvent.click(container.getByTestId("aaa101"))
    expect(container.getByText("FCE Count: 1.0")).toBeDefined()
    fireEvent.click(container.getByTestId("aaa101"))
    expect(container.getByText("FCE Count: 0.5")).toBeDefined()
  })

  it("Clicking the course code on the sidebar should decrease the FCE count", async () => {
    const container = await TestContainer.build()
    expect(container.getByText("FCE Count: 0.0")).toBeDefined()
    fireEvent.click(container.getByTestId("aaa100"))
    expect(container.getByText("FCE Count: 0.5")).toBeDefined()
    const sidebar = await TestSidebar.build()
    expect(sidebar.getByTestId("test AAA100")).toBeDefined()
    fireEvent.click(sidebar.getByTestId("test AAA100"))
    expect(container.getByText("FCE Count: 0.0")).toBeDefined()
  })

  it("Clicking a graph button adds and removes the course to the Selected Courses", async () => {
    const container = await TestContainer.build()
    fireEvent.click(container.getByTestId("aaa100"))
    const sidebar = await TestSidebar.build()
    expect(sidebar.getByTestId("test AAA100")).toBeDefined()
    fireEvent.click(container.getByTestId("aaa100"))
    expect(sidebar.queryByTestId("test AAA100")).toBeNull()
  })

  it("Clicking graph buttons add and remove the courses to the Selected Courses", async () => {
    const container = await TestContainer.build()
    fireEvent.click(container.getByTestId("aaa100"))
    fireEvent.click(container.getByTestId("aaa303"))
    const sidebar = await TestSidebar.build()
    expect(sidebar.getByTestId("test AAA100")).toBeDefined()
    expect(sidebar.getByTestId("test AAA303")).toBeDefined()
    fireEvent.click(container.getByTestId("aaa100"))
    fireEvent.click(container.getByTestId("aaa303"))
    expect(sidebar.queryByTestId("test AAA100")).toBeNull()
    expect(sidebar.queryByTestId("test AAA303")).toBeNull()
  })

  it("Clicking the `x` button next to a selected course removes that course from the Selected Courses", async () => {
    const container = await TestContainer.build()
    fireEvent.click(container.getByTestId("aaa100"))
    const sidebar = await TestSidebar.build()
    expect(sidebar.getByTestId("test AAA100")).toBeDefined()
    fireEvent.mouseDown(within(sidebar.getByTestId("test AAA100")).getByText("X"))
    expect(sidebar.queryByTestId("test AAA100")).toBeNull()
  })

  it("Clicking the `x` button for a 'composite` selected course removes that course from the Selected Courses", async () => {
    const container = await TestContainer.build()
    fireEvent.click(container.getByTestId("aaa100"))
    const sidebar = await TestSidebar.build()
    expect(sidebar.getByTestId("test AAA100")).toBeDefined()
    fireEvent.mouseDown(within(sidebar.getByTestId("test AAA100")).getByText("X"))
    expect(sidebar.queryByTestId("test AAA100")).toBeNull()
  })

  it("Clicking the `x` buttons next to selected courses removes those courses from the Selected Courses", async () => {
    const container = await TestContainer.build()
    fireEvent.click(container.getByTestId("aaa100"))
    fireEvent.click(container.getByTestId("aaa303"))
    const sidebar = await TestSidebar.build()
    expect(sidebar.getByTestId("test AAA100")).toBeDefined()
    expect(sidebar.getByTestId("test AAA303")).toBeDefined()
    fireEvent.mouseDown(within(sidebar.getByTestId("test AAA100")).getByText("X"))
    expect(sidebar.queryByTestId("test AAA100")).toBeNull()
    fireEvent.mouseDown(within(sidebar.getByTestId("test AAA303")).getByText("X"))
    expect(sidebar.queryByTestId("test AAA303")).toBeNull()
  })

  it("Clicking on a course-link (amongst the Selected Courses) opens the corresponding course info modal ", async () => {
    const container = await TestContainer.build()
    const sidebar = await TestSidebar.build()
    fireEvent.click(container.getByTestId("aaa100")) // activates AAA100 as a "selected course"
    expect(sidebar.getByTestId("test AAA100")).toBeDefined()
    fireEvent.click(within(sidebar.getByTestId("test AAA100")).getByText("AAA100")) // clicking the course text link
    await waitFor(() => {
      expect(container.queryByText(/AAA Thinking/)).toBeDefined()
    })
  })

  it("Clicking the reset selections clears the Selected Courses and resets FCE", async () => {
    const container = await TestContainer.build()
    fireEvent.click(container.getByTestId("aaa100"))
    fireEvent.click(container.getByTestId("test-reset"))
    expect(container.getByText("FCE Count: 0.0")).toBeDefined()
    const sidebar = await TestSidebar.build()
    expect(sidebar.queryByTestId("test AAA100")).toBeNull()
  })

  it("Searching in the Search bar yields the correct result", async () => {
    const sidebar = await TestSidebar.build()
    const input = sidebar.getByTestId("test-search-bar")
    fireEvent.change(input, { target: { value: "AAA100" } })
    expect(input.value).toBe("AAA100")
  })

  it("Search bar dropdown is only rendered to DOM once an input is recognized", async () => {
    const sidebar = await TestSidebar.build()
    expect(sidebar.queryByTestId("test-searchDropdown")).toBeNull()
    const input = sidebar.getByTestId("test-search-bar")
    fireEvent.change(input, { target: { value: "AAA100" } })
    expect(input.value).toBe("AAA100")
    expect(sidebar.queryByTestId("test-searchDropdown")).toBeDefined()
  })

  // it('Clicking an item from the Search bar updates FCE count and adds it to the active courses', async () => {
  //   const sidebar = await TestSidebar.build();
  //   const input = sidebar.getByTestId('test-search-bar');
  //   fireEvent.change(input, { target: { value: 'AAA100' } });
  //   const container = await TestContainer.build();
  //   expect(container.getByText("FCE Count: 0.0")).toBeDefined();
  //   const liArr = sidebar.getAllByLabelText('test-li');
  //   expect(liArr[0].textContent).toBe('AAA100');
  //   fireEvent.click(liArr[0]);
  //   expect(container.getByText("FCE Count: 0.5")).toBeDefined();
  //   expect(sidebar.getByTestId("test aaa100")).toBeDefined();
  //   fireEvent.click(sidebar.getByTestId("test-search-AAA100"));
  //   expect(sidebar.queryByTestId("test aaa100")).toBeNull();
  //   expect(container.getByText("FCE Count: 0.0")).toBeDefined();
  // });
})
