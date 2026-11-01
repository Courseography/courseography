import React from "react"
import { render } from "@testing-library/react"
import GraphCanvas from "../GraphCanvas"

/**
 * Creates a #react-graph container (optionally inside a #generateRoot) with the given
 * client dimensions, since jsdom does not compute layout.
 */
function createContainer(clientWidth, clientHeight, inGenerate) {
  const reactGraph = document.createElement("div")
  reactGraph.id = "react-graph"
  Object.defineProperty(reactGraph, "clientWidth", { value: clientWidth })
  Object.defineProperty(reactGraph, "clientHeight", { value: clientHeight })

  if (inGenerate) {
    const generateRoot = document.createElement("div")
    generateRoot.id = "generateRoot"
    generateRoot.appendChild(reactGraph)
    document.body.appendChild(generateRoot)
  } else {
    document.body.appendChild(reactGraph)
  }
  return reactGraph
}

const canvasProps = {
  width: 800,
  height: 600,
  zoomFactor: 1,
  horizontalPanFactor: 10,
  verticalPanFactor: 5,
}

describe("GraphCanvas", () => {
  afterEach(() => {
    document.body.innerHTML = ""
  })

  it("Should size the viewBox to the graph dimensions", () => {
    const container = createContainer(1000, 500, false)
    render(<GraphCanvas {...canvasProps} />, { container })

    const svg = document.querySelector("#main-graph")
    expect(svg.getAttribute("viewBox")).toBe("12 6 800 600")
  })

  it("Should expand the viewBox to fill the container on the Generate page", () => {
    const container = createContainer(1000, 500, true)
    render(<GraphCanvas {...canvasProps} />, { container })

    const svg = document.querySelector("#main-graph")
    expect(svg.getAttribute("viewBox")).toBe("-88 6 1000 600")
  })

  it("Should not scale panning when there is no #react-graph container", () => {
    render(<GraphCanvas {...canvasProps} />)

    const svg = document.querySelector("#main-graph")
    expect(svg.getAttribute("viewBox")).toBe("10 5 800 600")
  })

  it("Should render its children inside the svg", () => {
    const container = createContainer(1000, 500, false)
    render(
      <GraphCanvas {...canvasProps}>
        <g id="test-child" />
      </GraphCanvas>,
      { container }
    )

    expect(document.querySelector("#main-graph > #test-child")).not.toBeNull()
  })
})
