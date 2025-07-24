import React from "react"
import { createRoot } from 'react-dom/client'
import { Graph } from "../graph/Graph"
import { ErrorBoundary } from "react-error-boundary"
import GraphFallback from "../graph/GraphFallback"

document.addEventListener("DOMContentLoaded", () => {
  const container = document.getElementById("react-graph")
  const root = createRoot(container)
  root.render(
    <ErrorBoundary
      FallbackComponent={GraphFallback}
    >
      <Graph start_blank={true} edit={true} initialDrawMode="draw-node" />
    </ErrorBoundary>
)
})
