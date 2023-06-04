import TestGraph from "./TestGraph"
import { fireEvent } from "@testing-library/react"
import { ZOOM_INCREMENT, KEYBOARD_PANNING_INCREMENT } from "../Graph"
import { shallow } from "enzyme"
import { Graph } from "../Graph"

describe("Graph Navigation", () => {
  it("Should pan right when the right arrow key is pressed", async () => {
    await TestGraph.build()
    const svg = document.querySelector("svg")
    const initialX = parseInt(svg.getAttribute("viewBox").split(" ")[0])
    fireEvent.keyDown(document.body, { key: "ArrowRight", code: "ArrowRight" })
    const newX = parseInt(svg.getAttribute("viewBox").split(" ")[0])
    const expected = initialX - KEYBOARD_PANNING_INCREMENT
    expect(newX).toBe(expected)
  })

  it("Should pan left when the left arrow key is pressed", async () => {
    await TestGraph.build()
    const svg = document.querySelector("svg")
    const initialX = parseInt(svg.getAttribute("viewBox").split(" ")[0])
    fireEvent.keyDown(document.body, { key: "ArrowLeft", code: "ArrowLeft" })
    const newX = parseInt(svg.getAttribute("viewBox").split(" ")[0])
    const expected = initialX + KEYBOARD_PANNING_INCREMENT
    expect(newX).toBe(expected)
  })

  it("Should pan down when the down arrow key is pressed", async () => {
    await TestGraph.build()
    const svg = document.querySelector("svg")
    const initialY = parseInt(svg.getAttribute("viewBox").split(" ")[1])
    fireEvent.keyDown(document.body, { key: "ArrowDown", code: "ArrowDown" })
    const newY = parseInt(svg.getAttribute("viewBox").split(" ")[1])
    const expected = initialY - KEYBOARD_PANNING_INCREMENT
    expect(newY).toBe(expected)
  })

  it("Should pan up when the up arrow key is pressed", async () => {
    await TestGraph.build()
    const svg = document.querySelector("svg")
    const initialY = parseInt(svg.getAttribute("viewBox").split(" ")[1])
    fireEvent.keyDown(document.body, { key: "ArrowUp", code: "ArrowUp" })
    const newY = parseInt(svg.getAttribute("viewBox").split(" ")[1])
    const expected = initialY + KEYBOARD_PANNING_INCREMENT
    expect(newY).toBe(expected)
  })

  it("Should zoom in when the user presses the + key", async () => {
    await TestGraph.build()
    const svg = document.querySelector("svg")
    const initialDims = svg
      .getAttribute("viewBox")
      .split(" ")
      .splice(2)
      .map(dim => parseFloat(dim))
    fireEvent.keyDown(document.body, { key: "+" })
    const newDims = svg
      .getAttribute("viewBox")
      .split(" ")
      .splice(2)
      .map(dim => parseFloat(dim))
    const expectedDims = initialDims.map(dim => dim * (1 - ZOOM_INCREMENT))

    expect(newDims[0]).toBe(expectedDims[0])
    expect(newDims[1]).toBe(expectedDims[1])
  })

  it("Should zoom out when the user presses the - key", async () => {
    await TestGraph.build()
    const svg = document.querySelector("svg")
    const initialDims = svg
      .getAttribute("viewBox")
      .split(" ")
      .splice(2)
      .map(dim => parseFloat(dim))
    fireEvent.keyDown(document.body, { key: "-" })
    const newDims = svg
      .getAttribute("viewBox")
      .split(" ")
      .splice(2)
      .map(dim => parseFloat(dim))
    const expectedDims = initialDims.map(dim => dim * (1 + ZOOM_INCREMENT))

    expect(newDims[0]).toBe(expectedDims[0])
    expect(newDims[1]).toBe(expectedDims[1])
  })

  it("Should pan when the user clicks and drags", async () => {
    await TestGraph.build()
    const svg = document.querySelector("svg")
    const initialX = parseInt(svg.getAttribute("viewBox").split(" ")[0])
    const initialY = parseInt(svg.getAttribute("viewBox").split(" ")[1])
    fireEvent.mouseDown(svg, { clientX: 250, clientY: 10 })
    fireEvent.mouseMove(svg, { clientX: 100, clientY: 20 })
    fireEvent.mouseUp(svg, { clientX: 100, clientY: 20 })
    const newX = parseInt(svg.getAttribute("viewBox").split(" ")[0])
    const newY = parseInt(svg.getAttribute("viewBox").split(" ")[1])
    const expectedX = initialX + 150
    const expectedY = initialY - 10

    expect(newX).toBe(expectedX)
    expect(newY).toBe(expectedY)
  })

  it("Should zoom in when the mouse wheel is scrolled down", async () => {
    const graph = await TestGraph.build()
    const svg = document.querySelector("svg")
    const initialDims = svg
      .getAttribute("viewBox")
      .split(" ")
      .splice(2)
      .map(dim => parseFloat(dim))
    const reactGraph = graph.getByTestId("react-graph")
    fireEvent.wheel(reactGraph, { deltaY: -1 })
    const newDims = svg
      .getAttribute("viewBox")
      .split(" ")
      .splice(2)
      .map(dim => parseFloat(dim))
    const expectedDims = initialDims.map(dim => dim * (1 - ZOOM_INCREMENT))
    expect(newDims[0]).toBe(expectedDims[0])
    expect(newDims[1]).toBe(expectedDims[1])
  })

  it("Should zoom out when the mouse wheel is scrolled up", async () => {
    const graph = await TestGraph.build()
    const svg = document.querySelector("svg")
    const initialDims = svg
      .getAttribute("viewBox")
      .split(" ")
      .splice(2)
      .map(dim => parseFloat(dim))
    const reactGraph = graph.getByTestId("react-graph")
    fireEvent.wheel(reactGraph, { deltaY: 1 })
    const newDims = svg
      .getAttribute("viewBox")
      .split(" ")
      .splice(2)
      .map(dim => parseFloat(dim))
    const expectedDims = initialDims.map(dim => dim * (1 + ZOOM_INCREMENT))
    expect(newDims[0]).toBe(expectedDims[0])
    expect(newDims[1]).toBe(expectedDims[1])
  })
})

describe("Graph rendering RegionGroup", () => {
  it("should match shallow snapshot", () => {
    const regionsJSON = [
      {
        graph: 1,
        points: [
          [17.386348, 281.07376883],
          [17.386348, 14.41568908],
          [316.385978, 14.41576883],
          [316.386348, 281.07376883],
        ],
        isRegion: true,
        stroke: "",
        fill: "#6276b9",
        id_: "p83",
        source: "",
        target: "",
      },
    ]
    const labelsJSON = [
      {
        graph: 1,
        rId: "tspan4346-9",
        text: "Systems",
        pos: [1088.1677413999998, 201.36453113000002],
        fill: "#000000",
        align: "begin",
      },
    ]

    const wrapper = new Graph({})
    expect(wrapper.renderRegionsLabels(regionsJSON, labelsJSON)).toMatchSnapshot()
  })
})

describe("EdgeGroup", () => {
  it("should match shallow snapshot", () => {
    const params = {
      edgesJSON: {},
      edgesStatus: {},
    }

    const graph = new Graph({})
    const component = shallow(
      graph.renderEdgeGroup(params.edgesJSON, params.edgesStatus)
    )

    expect(component).toMatchSnapshot()
  })
})

describe("BoolGroup", () => {
  it("BoolGroup should match shallow snapshot", () => {
    const params = {
      boolsJSON: {},
      boolsStatus: { bool1: "inactive", bool2: "inactive" },
      connections: {},
    }

    const graph = new Graph({})
    const component = shallow(
      graph.renderBoolGroup(params.boolsJSON, params.boolsStatus, params.connections)
    )

    expect(component).toMatchSnapshot()
  })
})

describe("NodeGroup", () => {
  it("should match shallow snapshot", () => {
    const params = {
      nodeClick: jest.fn(),
      nodeMouseEnter: jest.fn(),
      nodeMouseLeave: jest.fn(),
      nodeMouseDown: jest.fn(),
      onKeyDown: jest.fn(),
      onWheel: jest.fn(),
      nodesStatus: {},
      nodesJSON: {},
      hybridsJSON: {},
      highlightedNodes: [],
      edgesJSON: [],
      connections: {},
      nodeDropshadowFilter: "",
    }

    const graph = new Graph({})
    const component = shallow(
      graph.renderNodeGroup(
        params.nodeClick,
        params.nodeMouseEnter,
        params.nodeMouseLeave,
        params.nodeMouseDown,
        params.onKeyDown,
        params.onWheel,
        params.nodesStatus,
        params.nodesJSON,
        params.hybridsJSON,
        params.highlightedNodes,
        params.edgesJSON,
        params.connections,
        params.nodeDropshadowFilter
      )
    )

    expect(component).toMatchSnapshot()
  })
})
