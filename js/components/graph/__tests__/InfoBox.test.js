import React from "react"
import InfoBox from "../InfoBox"
import TestGraph from "./TestGraph"
import { waitFor, act, screen, render } from "@testing-library/react"

import { userEvent } from "@testing-library/user-event"

describe("InfoBox", () => {
  it("should match snapshot", async () => {
    const infoBoxProps = {
      onClick: jest.fn(),
      onMouseDown: jest.fn(),
      onMouseLeave: jest.fn(),
      nodeId: "",
      showInfoBox: false,
      xPos: 0,
      yPos: 0,
    }
    await act(async () => render(<InfoBox {...infoBoxProps} />))
    const infoBoxElement = screen.getByText("Info").closest("g")
    expect(infoBoxElement).toMatchSnapshot()
  })

  it("should appear when hovering over a course", async () => {
    const user = userEvent.setup()
    const graph = await TestGraph.build()
    const aaa100 = graph.getByTestId("aaa100")

    const infoBox = graph.getNodeByText("Info")
    expect(infoBox.classList.contains("tooltip-group-hidden")).toBe(true)
    await user.hover(aaa100)
    expect(infoBox.classList.contains("tooltip-group-display")).toBe(true)
  })

  it("should remain visible when hovering over the InfoBox itself", async () => {
    const user = userEvent.setup()
    const graph = await TestGraph.build()
    const aaa100 = graph.getByTestId("aaa100")

    await user.hover(aaa100)
    const infoBox = graph.getNodeByText("Info")
    expect(infoBox.classList.contains("tooltip-group-display")).toBe(true)

    await user.hover(infoBox)

    await waitFor(() => {
      expect(infoBox.classList.contains("tooltip-group-display")).toBe(true)
    })
  })

  it("should disappear a second after the the cursor isn't hovered over the course", async () => {
    const user = userEvent.setup()
    const graph = await TestGraph.build()
    const aaa100 = graph.getByTestId("aaa100")

    await user.hover(aaa100)
    const infoBox = graph.getNodeByText("Info")
    expect(infoBox.classList.contains("tooltip-group-display")).toBe(true)

    await user.unhover(aaa100)

    await waitFor(() => {
      expect(infoBox.classList.contains("tooltip-group-hidden")).toBe(true)
    })
  }, 5000)

  it("Pressing on the info box should create a new pop up", async () => {
    const user = userEvent.setup()
    const graph = await TestGraph.build()
    const aaa100 = graph.getByTestId("aaa100")
    await user.hover(aaa100)
    const infoBox = graph.getNodeByText("Info")
    await user.click(infoBox)

    await waitFor(() => {
      expect(graph.textExists(/AAA Thinking/)).toBe(true)
    })
  })
})
