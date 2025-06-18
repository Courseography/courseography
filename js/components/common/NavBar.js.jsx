import React from "react"

/**
 * NavBar component.
 */
export function NavBar({ page }) {
  const isActive = this_page => (page === this_page ? "selected-page" : undefined)

  return (
    <nav className="row header">
      {/* Courseography Logo (also functions as an additional link to graph page) */}
      <div className="nav-left">
        <a href="/graph">
          <img
            id="courseography-header"
            src="/static/res/img/logo.png"
            alt="Courseography"
            data-context={page}
          />
        </a>
      </div>

      {/* Navigation links */}
      <div className="nav-middle">
        <ul id="nav-links">
          <li id="nav-graph" className={isActive("graph")}>
            <a href="/graph">Graph</a>
          </li>
          <li id="nav-grid" className={isActive("grid")}>
            <a href="/grid">Grid</a>
          </li>
          <li id="nav-generate" className={isActive("generate")}>
            <a href="/generate">Generate (beta)</a>
          </li>
          <li id="nav-about" className={isActive("about")}>
            <a href="/about">About</a>
          </li>
        </ul>
      </div>

      {/* Export button (graph/grid only) */}
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
