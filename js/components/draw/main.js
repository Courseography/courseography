import React from "react"
import { Graph } from "../graph/Graph"
import { createRoot } from 'react-dom/client';

document.addEventListener("DOMContentLoaded", () => {
  const container = document.getElementById("react-graph")
  const root = createRoot(container)
  root.render(<Graph start_blank={true} edit={true} initialDrawMode="draw-node" />)
})
