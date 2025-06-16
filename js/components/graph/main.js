import React from "react"
import { createRoot } from "react-dom/client"
import Container from "./Container"
import { NavBar } from "../common/NavBar.js.jsx"

import {
  AllCommunityModule,
  ModuleRegistry,
  provideGlobalGridOptions,
} from "ag-grid-community"

// Register all community features
ModuleRegistry.registerModules([AllCommunityModule])

// Mark all grids as using legacy themes
provideGlobalGridOptions({ theme: "legacy" })

// The "main"
document.addEventListener("DOMContentLoaded", () => {
  const navbar = document.getElementById("navbar")
  const navbarRoot = createRoot(navbar)
  navbarRoot.render(<NavBar />)

  const container = document.getElementById("container")
  const root = createRoot(container)
  root.render(<Container start_blank={false} edit={false} />)
})
