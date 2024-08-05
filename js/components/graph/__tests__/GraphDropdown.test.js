import React from "react"
import TestContainer from "./TestContainer"
import GraphDropdown from "../GraphDropdown"
import { waitFor, render, screen, act } from "@testing-library/react"
import userEvent from "@testing-library/user-event"

describe("GraphDropdown", () => {
  it("should match snapshot", async () => {
    const graphDropdownProps = {
      onMouseMove: jest.fn(),
      onMouseLeave: jest.fn(),
      showGraphDropdown: false,
      graphs: [],
      updateGraph: jest.fn(),
    }
    await act(async () => render(<GraphDropdown {...graphDropdownProps} />))
    const graphDropdownElement = screen.getByTestId("test-graph-dropdown")
    expect(graphDropdownElement).toMatchSnapshot()
  })

  it("should appear when hovering over the graph tab and be hidden before", async () => {
    const user = userEvent.setup()

    const container = await TestContainer.build()
    const graphNav = container.getByText("Graph")
    const graphDropdown = container.getByTestId("test-graph-dropdown")
    expect(graphDropdown.classList.contains("hidden")).toBe(true)
    await user.hover(graphNav)
    expect(graphDropdown.classList.contains("graph-dropdown-display")).toBe(true)
  })

  it("should disappear a second after the cursor isn't hovered on the dropdown", async () => {
    const user = userEvent.setup()
    const container = await TestContainer.build()
    const graphNav = container.getByText("Graph")
    const graphDropdown = container.getByTestId("test-graph-dropdown")
    expect(graphDropdown.classList.contains("hidden")).toBe(true)
    await user.hover(graphNav)
    expect(graphDropdown.classList.contains("graph-dropdown-display")).toBe(true)

    await user.unhover(graphDropdown)
    await waitFor(() => {
      expect(graphDropdown.classList.contains("hidden")).toBe(true)
    })
  }, 5000)

  it("should disappear a second after the cursor isn't hovered on the graph tab", async () => {
    const user = userEvent.setup()

    const container = await TestContainer.build()
    const graphNav = container.getByText("Graph")
    const graphDropdown = container.getByTestId("test-graph-dropdown")
    expect(graphDropdown.classList.contains("hidden")).toBe(true)
    await user.hover(graphNav)
    expect(graphDropdown.classList.contains("graph-dropdown-display")).toBe(true)

    await user.unhover(graphNav)
    await waitFor(() => {
      expect(graphDropdown.classList.contains("hidden")).toBe(true)
    })
  }, 5000)
})
