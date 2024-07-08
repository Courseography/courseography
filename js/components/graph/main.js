import React from "react"
import {createRoot} from "react-dom/client"
import Container from "./Container"

// The "main"
document.addEventListener("DOMContentLoaded", () => {
  const container = document.getElementById("container")
  const root = createRoot(container)
  root.render(<Container start_blank={false} edit={false} />)
})
