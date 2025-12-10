import TestGraph from "./TestGraph"
import { fireEvent, screen, within } from "@testing-library/react"
import { ZOOM_INCREMENT, KEYBOARD_PANNING_INCREMENT } from "../Graph"
import { Graph } from "../Graph"
import userEvent from "@testing-library/user-event"

describe("Graph Navigation", () => {
  it("Should pan right when the right arrow key is pressed", async () => {
    const user = userEvent.setup()
    await TestGraph.build()
    const svg = document.querySelector("#main-graph")
    const initialX = parseInt(svg.getAttribute("viewBox").split(" ")[0])
    await user.keyboard("{ArrowRight}")
    const newX = parseInt(svg.getAttribute("viewBox").split(" ")[0])
    const expected = initialX - KEYBOARD_PANNING_INCREMENT
    expect(newX).toBe(expected)
  })

  it("Should pan left when the left arrow key is pressed", async () => {
    const user = userEvent.setup()
    await TestGraph.build()
    const svg = document.querySelector("#main-graph")
    const initialX = parseInt(svg.getAttribute("viewBox").split(" ")[0])
    await user.keyboard("{ArrowLeft}")
    const newX = parseInt(svg.getAttribute("viewBox").split(" ")[0])
    const expected = initialX + KEYBOARD_PANNING_INCREMENT
    expect(newX).toBe(expected)
  })

  it("Should pan down when the down arrow key is pressed", async () => {
    const user = userEvent.setup()
    await TestGraph.build()
    const svg = document.querySelector("#main-graph")
    const initialY = parseInt(svg.getAttribute("viewBox").split(" ")[1])
    await user.keyboard("{ArrowDown}")
    const newY = parseInt(svg.getAttribute("viewBox").split(" ")[1])
    const expected = initialY - KEYBOARD_PANNING_INCREMENT
    expect(newY).toBe(expected)
  })

  it("Should pan up when the up arrow key is pressed", async () => {
    const user = userEvent.setup()
    await TestGraph.build()
    const svg = document.querySelector("#main-graph")
    const initialY = parseInt(svg.getAttribute("viewBox").split(" ")[1])
    await user.keyboard("{ArrowUp}")
    const newY = parseInt(svg.getAttribute("viewBox").split(" ")[1])
    const expected = initialY + KEYBOARD_PANNING_INCREMENT
    expect(newY).toBe(expected)
  })

  it("Should zoom in when the user presses the + key", async () => {
    const user = userEvent.setup()
    await TestGraph.build()
    const svg = document.querySelector("#main-graph")
    const initialDims = svg
      .getAttribute("viewBox")
      .split(" ")
      .splice(2)
      .map(dim => parseFloat(dim))
    await user.keyboard("{+}")
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
    const user = userEvent.setup()
    await TestGraph.build()
    const svg = document.querySelector("#main-graph")
    const initialDims = svg
      .getAttribute("viewBox")
      .split(" ")
      .splice(2)
      .map(dim => parseFloat(dim))
    await user.keyboard("{-}")
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
    const user = userEvent.setup()
    await TestGraph.build()
    const svg = document.querySelector("#main-graph")
    const initialX = parseInt(svg.getAttribute("viewBox").split(" ")[0])
    const initialY = parseInt(svg.getAttribute("viewBox").split(" ")[1])

    await user.pointer([
      { keys: "[MouseLeft>]", target: svg, coords: { x: 250, y: 10 } }, // Mouse down
      { coords: { x: 100, y: 20 } }, // Mouse move
      { keys: "[/MouseLeft]" }, // Mouse up
    ])

    const newX = parseInt(svg.getAttribute("viewBox").split(" ")[0])
    const newY = parseInt(svg.getAttribute("viewBox").split(" ")[1])
    const expectedX = initialX + 150
    const expectedY = initialY - 10

    expect(newX).toBe(expectedX)
    expect(newY).toBe(expectedY)
  })

  it("Should pan when the user touches and drags", async () => {
    await TestGraph.build()
    const svg = document.querySelector("#main-graph")
    const initialX = parseInt(svg.getAttribute("viewBox").split(" ")[0])
    const initialY = parseInt(svg.getAttribute("viewBox").split(" ")[1])

    fireEvent.touchStart(svg, { touches: [{ clientX: 250, clientY: 10 }] })
    fireEvent.touchMove(svg, { touches: [{ clientX: 100, clientY: 20 }] })
    fireEvent.touchEnd(svg)

    const newX = parseInt(svg.getAttribute("viewBox").split(" ")[0])
    const newY = parseInt(svg.getAttribute("viewBox").split(" ")[1])
    const expectedX = initialX + 150
    const expectedY = initialY - 10

    expect(newX).toBe(expectedX)
    expect(newY).toBe(expectedY)
  })

  it("Should zoom in when the mouse wheel is scrolled down", async () => {
    const graph = await TestGraph.build()
    const svg = document.querySelector("#main-graph")
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
    const svg = document.querySelector("#main-graph")
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

    const graph = new Graph({})
    expect(graph.renderRegionsLabels(regionsJSON, labelsJSON)).toMatchSnapshot()
  })
})

describe("EdgeGroup", () => {
  it("should match shallow snapshot", () => {
    const params = {
      edgesJSON: {},
      edgesStatus: {},
    }

    const graph = new Graph({})
    const edgeGroup = graph.renderEdgeGroup(params.edgesJSON, params.edgesStatus)
    expect(edgeGroup).toMatchSnapshot()
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
    const boolGroup = graph.renderBoolGroup(
      params.boolsJSON,
      params.boolsStatus,
      params.connections
    )
    expect(boolGroup).toMatchSnapshot()
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
    const nodeGroup = graph.renderNodeGroup(
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
    expect(nodeGroup).toMatchSnapshot()
  })
})

describe("Dependency Highlighting", () => {
  it("Hovering over course node highlights it and its dependencies", async () => {
    await TestGraph.build()
    const user = userEvent.setup()
    const aaa303 = screen.getByText("AAA303")
    const aaa202 = screen.getByText("AAA201")
    await user.hover(aaa303)
    const aaa303Spotlight = aaa303.closest("svg").querySelector(".spotlight") // aaa303 ellipse
    expect(aaa303Spotlight).not.toBeNull()
    const aaa202Spotlight = aaa202.closest("svg").querySelector(".spotlight") // aaa202 ellipse
    expect(aaa202Spotlight).not.toBeNull()
  })

  it("Hovering over course result in Sidebar search highlights the corresponding node and its dependencies", async () => {
    await TestGraph.build()
    const user = userEvent.setup()
    const sidebar = screen.getByTestId("test-searchDropdown")
    const sidebarExpandButton = screen.getByTestId("test-sidebar-button")
    await user.click(sidebarExpandButton)
    const sidebarInput = screen.getByTestId("test-search-bar")
    await user.tripleClick(sidebarInput)
    await user.keyboard("AAA")
    const aaa100SearchResult = within(sidebar).getByText("AAA100")
    const aaa100Node = screen.getByTestId("aaa100")
    expect(aaa100Node.querySelector(".spotlight")).toBeNull()
    await user.hover(aaa100SearchResult)
    expect(aaa100Node.querySelector(".spotlight")).not.toBeNull()
    await user.unhover(aaa100SearchResult)
    expect(aaa100Node.querySelector(".spotlight")).toBeNull()
  })

  it("Hovering over selected course in Sidebar highlights the corresponding node and its dependencies", async () => {
    await TestGraph.build()
    const user = userEvent.setup()
    const sidebar = screen.getByTestId("test-searchDropdown")
    const sidebarExpandButton = screen.getByTestId("test-sidebar-button")
    await user.click(sidebarExpandButton)
    const sidebarInput = screen.getByTestId("test-search-bar")
    await user.tripleClick(sidebarInput)
    await user.keyboard("AAA303")
    const aaa100SearchResult = within(sidebar).getByText("AAA303")
    await user.click(aaa100SearchResult)
    const aaa100SelectedCourse = screen.getByTestId("test AAA303") // in the `selected` section of the sidebar
    await user.unhover(aaa100SearchResult)
    const aaa100Node = screen.getByTestId("aaa303")
    expect(aaa100Node.querySelector(".spotlight")).toBeNull()
    await user.hover(aaa100SelectedCourse)
    expect(aaa100Node.querySelector(".spotlight")).not.toBeNull()
    await user.unhover(aaa100SelectedCourse)
    expect(aaa100Node.querySelector(".spotlight")).toBeNull()
  })
})
