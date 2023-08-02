import React from "react"
import renderer from "react-test-renderer"
import InfoBox from "../InfoBox"
import TestGraph from "./TestGraph"
import { fireEvent, waitFor } from "@testing-library/react"

describe("InfoBox", () => {
  it("should match snapshot", () => {
    const infoBoxProps = {
      onClick: jest.fn(),
      onMouseDown: jest.fn(),
      onMouseLeave: jest.fn(),
      nodeId: "",
      showInfoBox: false,
      xPos: 0,
      yPos: 0,
    }
    const tree = renderer.create(<InfoBox {...infoBoxProps} />).toJSON()
    expect(tree).toMatchSnapshot()
  })

  it("should appear when hovering over a course", async () => {
    const graph = await TestGraph.build()
    const aaa100 = graph.getByTestId("aaa100")

    const infoBox = graph.getNodeByText("Info")
    expect(infoBox.classList.contains("tooltip-group-hidden")).toBe(true)
    fireEvent.mouseOver(aaa100)
    expect(infoBox.classList.contains("tooltip-group-display")).toBe(true)
  })

  it("should disappear a second after the the cursor isn't hovered over the course", async () => {
    const graph = await TestGraph.build()
    const aaa100 = graph.getByTestId("aaa100")

    fireEvent.mouseOver(aaa100)
    const infoBox = graph.getNodeByText("Info")
    expect(infoBox.classList.contains("tooltip-group-display")).toBe(true)

    fireEvent.mouseOut(aaa100)

    await waitFor(() => {
      expect(infoBox.classList.contains("tooltip-group-hidden")).toBe(true)
    })
  }, 5000)

  it("Pressing on the info box should create a new pop up", async () => {
    const graph = await TestGraph.build()
    const aaa100 = graph.getByTestId("aaa100")
    fireEvent.mouseOver(aaa100)
    const infoBox = graph.getNodeByText("Info")
    fireEvent.click(infoBox)

    await waitFor(() => {
      expect(graph.textExists(/AAA Thinking/)).toBe(true)
    })
  })
})
