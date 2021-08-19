import React from "react"
import { shallow } from "enzyme"
import InfoBox from "../InfoBox"
import TestGraph from "./TestGraph"
import { fireEvent } from "@testing-library/react"

describe("InfoBox", () => {
  it("should match shallow snapshot", () => {
    const infoBoxProps = {
      onClick: jest.fn(),
      onMouseDown: jest.fn(),
      onMouseLeave: jest.fn(),
      nodeId: "",
      showInfoBox: false,
      xPos: 0,
      yPos: 0,
    }
    const component = shallow(<InfoBox {...infoBoxProps} />)
    expect(component).toMatchSnapshot()
  })

  it("should appear when hovering over a course", async () => {
    const graph = await TestGraph.build()
    const aaa100 = graph.getByTestId("aaa100")

    const infoBox = graph.getNodeByText("Info")
    expect(infoBox.classList.contains("tooltip-group-hidden")).toBe(true)
    fireEvent.mouseOver(aaa100)
    expect(infoBox.classList.contains("tooltip-group-display")).toBe(true)
  })

  it("should disappear a second after the the cursor isn't hovered over the course", async done => {
    const graph = await TestGraph.build()
    const aaa100 = graph.getByTestId("aaa100")

    fireEvent.mouseOver(aaa100)
    const infoBox = graph.getNodeByText("Info")
    expect(infoBox.classList.contains("tooltip-group-display")).toBe(true)

    fireEvent.mouseOut(aaa100)

    setTimeout(() => {
      expect(infoBox.classList.contains("tooltip-group-hidden")).toBe(true)
      done()
    }, 1000)
  })
  it("Pressing on the info box should create a new pop up", async done => {
    const graph = await TestGraph.build()
    const aaa100 = graph.getByTestId("aaa100")
    fireEvent.mouseOver(aaa100)
    const infoBox = graph.getNodeByText("Info")
    fireEvent.click(infoBox)

    // wait for fake fetch to finish
    setTimeout(() => {
      // expect description in the modal box to appear
      expect(graph.textExists(/AAA Thinking/)).toBe(true)
      done()
    }, 1000)
  })
})
