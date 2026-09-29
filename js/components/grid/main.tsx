import { createRoot } from "react-dom/client"
import Grid from "./grid"

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
  const container = document.getElementById("grid-body")!
  const root = createRoot(container)
  root.render(<Grid />)
})
