import React from "react"

/** Element id of the drop shadow filter applied to (non-hybrid) course nodes */
export const NODE_DROPSHADOW_FILTER_ID = "dropshadow"

/**
 * Namespace declarations with no effect on inline SVG in the browser, kept so the
 * markup is a valid standalone SVG document if serialized (e.g. for export).
 * Spread as an object because React's SVG prop types don't declare colon-named attributes.
 */
const EXTRA_NAMESPACE_ATTRS = {
  "xmlns:svg": "http://www.w3.org/2000/svg",
  "xmlns:dc": "http://purl.org/dc/elements/1.1/",
  "xmlns:cc": "http://creativecommons.org/ns#",
  "xmlns:rdf": "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
}

interface GraphCanvasProps {
  width: number
  height: number
  zoomFactor: number
  horizontalPanFactor: number
  verticalPanFactor: number
  onMouseDown?: React.MouseEventHandler<SVGSVGElement>
  onMouseUp?: React.MouseEventHandler<SVGSVGElement>
  onMouseMove?: React.MouseEventHandler<SVGSVGElement>
  onTouchStart?: React.TouchEventHandler<SVGSVGElement>
  children?: React.ReactNode
}

/**
 * The root <svg> element of a graph. Computes the viewBox from the current zoom/pan
 * state and defines the shared filters and markers referenced by nodes and edges.
 */
export default function GraphCanvas({
  width,
  height,
  zoomFactor,
  horizontalPanFactor,
  verticalPanFactor,
  onMouseDown,
  onMouseUp,
  onMouseMove,
  onTouchStart,
  children,
}: GraphCanvasProps) {
  let containerWidth = 0
  let containerHeight = 0
  const reactGraph = document.getElementById("react-graph")
  if (reactGraph !== null) {
    containerWidth = reactGraph.clientWidth
    containerHeight = reactGraph.clientHeight
  }

  let newViewboxWidth = width * zoomFactor
  let newViewboxHeight = height * zoomFactor
  if (document.getElementById("generateRoot") !== null) {
    newViewboxWidth = Math.max(width, containerWidth) * zoomFactor
    newViewboxHeight = Math.max(height, containerHeight) * zoomFactor
  }

  const viewboxContainerRatio =
    containerHeight !== 0 ? newViewboxHeight / containerHeight : 1
  const viewboxX =
    (width - newViewboxWidth) / 2 + horizontalPanFactor * viewboxContainerRatio
  const viewboxY =
    (height - newViewboxHeight) / 2 + verticalPanFactor * viewboxContainerRatio

  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      xmlnsXlink="http://www.w3.org/1999/xlink"
      {...EXTRA_NAMESPACE_ATTRS}
      version="1.1"
      id="main-graph"
      width="100%"
      height="100%"
      viewBox={`${viewboxX} ${viewboxY} ${newViewboxWidth} ${newViewboxHeight}`}
      preserveAspectRatio="xMidYMid"
      onMouseDown={onMouseDown}
      onMouseUp={onMouseUp}
      onMouseMove={onMouseMove}
      onTouchStart={onTouchStart}
    >
      <filter id={NODE_DROPSHADOW_FILTER_ID} height="130%">
        <feGaussianBlur in="SourceAlpha" stdDeviation="3" />
        <feOffset dx="2" dy="2" result="offsetblur" />
        <feComponentTransfer>
          <feFuncA type="linear" slope="0.8" />
        </feComponentTransfer>
        <feMerge>
          <feMergeNode />
          <feMergeNode in="SourceGraphic" />
        </feMerge>
      </filter>
      <defs>
        <marker
          id="arrowHead"
          viewBox="0 0 10 10"
          refX="4"
          refY="5"
          markerUnits="strokeWidth"
          markerWidth="7"
          markerHeight="7"
          orient="auto"
        >
          <polyline points="0,1 10,5 0,9" fill="black" />
        </marker>
      </defs>
      <defs>
        <filter id="blur-filter">
          <feGaussianBlur in="SourceGraphic" stdDeviation="2" />
        </filter>
      </defs>
      {children}
    </svg>
  )
}
