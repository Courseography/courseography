import { NavLink, useLocation } from "react-router-dom"

/**
 * NavBar component; includes routing to home/graph page, grid page, generate page
 * and about page, additionally providing the option to export if currently on the
 * graph or grid page.
 */
export function NavBar() {
  const { pathname } = useLocation()

  const page = () => {
    if (pathname.startsWith("/grid")) return "grid"
    if (pathname.startsWith("/generate")) return "generate"
    if (pathname.startsWith("/about")) return "about"
    return "graph" // go to home (graph) page by default
  }

  return (
    <nav className="row header">
      {/* Courseography Logo (also functions as an additional link to graph page) */}
      <div className="nav-left">
        <NavLink to="/graph">
          <img
            id="courseography-header"
            src="/static/res/img/logo.png"
            alt="Courseography"
            data-context={page}
          />
        </NavLink>
      </div>

      {/* Links to graph, grid and generate pages */}
      <div className="nav-middle">
        <ul id="nav-links">
          <li id="nav-graph">
            <NavLink
              to="/graph"
              className={({ isActive }) => (isActive ? "selected-page" : undefined)}
            >
              Graph
            </NavLink>
          </li>
          <li id="nav-grid">
            <NavLink
              to="/grid"
              className={({ isActive }) => (isActive ? "selected-page" : undefined)}
            >
              Grid
            </NavLink>
          </li>
          <li id="nav-generate">
            <NavLink
              to="/generate"
              className={({ isActive }) => (isActive ? "selected-page" : undefined)}
            >
              Generate (beta)
            </NavLink>
          </li>
          <li id="nav-about">
            <NavLink
              to="/about"
              className={({ isActive }) => (isActive ? "selected-page" : undefined)}
            >
              About
            </NavLink>
          </li>
        </ul>
      </div>

      {/* Export button (available for graph/grid pages only)*/}
      <div className="nav-right">
        {(page === "graph" || page === "grid") && (
          <button id="nav-export">
            <img src="/static/res/ico/export.png" alt="Export" />
          </button>
        )}
      </div>
    </nav>
  )
}
