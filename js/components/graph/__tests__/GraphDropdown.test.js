import React from "react"
import TestContainer from "./TestContainer"
import GraphDropdown from "../GraphDropdown"
import { shallow } from "enzyme"
import { fireEvent } from "@testing-library/react"

function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms))
}

describe("GraphDropdown", () => {
  it("should match shallow snapshot", () => {
    const graphDropdownProps = {
      onMouseMove: jest.fn(),
      onMouseLeave: jest.fn(),
      showGraphDropdown: false,
      graphs: [],
      updateGraph: jest.fn(),
    }
    const component = shallow(<GraphDropdown {...graphDropdownProps} />)
    expect(component).toMatchSnapshot()
  })
  it("should appear when hovering over the graph tab and be hidden before", async () => {
    const mouseEnter = new MouseEvent("mouseenter", {
      bubbles: false,
      cancelable: false,
    })

    const container = await TestContainer.build()
    const graphNav = container.getByText("Graph")
    const graphDropdown = container.getByTestId("test-graph-dropdown")
    expect(graphDropdown.classList.contains("hidden")).toBe(true)
    fireEvent(graphNav, mouseEnter)
    expect(graphDropdown.classList.contains("graph-dropdown-display")).toBe(true)
  })

  it("should disappear a second after the cursor isn't hovered on the dropdown", async () => {
    const mouseEnter = new MouseEvent("mouseenter", {
      bubbles: false,
      cancelable: false,
    })
    const container = await TestContainer.build()
    const graphNav = container.getByText("Graph")
    const graphDropdown = container.getByTestId("test-graph-dropdown")
    expect(graphDropdown.classList.contains("hidden")).toBe(true)
    fireEvent(graphNav, mouseEnter)
    expect(graphDropdown.classList.contains("graph-dropdown-display")).toBe(true)

    fireEvent.mouseOut(graphDropdown)
    await sleep(600)
    expect(graphDropdown.classList.contains("hidden")).toBe(true)
  }, 5000)

  it("should disappear a second after the cursor isn't hovered on the graph tab", async () => {
    const mouseEnter = new MouseEvent("mouseenter", {
      bubbles: false,
      cancelable: false,
    })
    const mouseLeave = new MouseEvent("mouseleave", {
      bubbles: false,
      cancelable: false,
    })

    const container = await TestContainer.build()
    const graphNav = container.getByText("Graph")
    const graphDropdown = container.getByTestId("test-graph-dropdown")
    expect(graphDropdown.classList.contains("hidden")).toBe(true)
    fireEvent(graphNav, mouseEnter)
    expect(graphDropdown.classList.contains("graph-dropdown-display")).toBe(true)

    fireEvent(graphNav, mouseLeave)
    await sleep(600)
    expect(graphDropdown.classList.contains("hidden")).toBe(true)
  }, 5000)
})
