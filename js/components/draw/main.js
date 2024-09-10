import React from "react"
import { createRoot } from 'react-dom/client'
import { Graph } from "../graph/Graph"

document.addEventListener("DOMContentLoaded", () => {
  const container = document.getElementById("react-graph")
  const root = createRoot(container)
  root.render(<Graph start_blank={true} edit={true} initialDrawMode="draw-node" />)
})
