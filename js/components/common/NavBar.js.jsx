import React from "react"

/**
 * NavBar component.
 */
export function NavBar() {
  const pathname = window.location.pathname

  const page = pathname.startsWith("/grid")
    ? "grid"
    : pathname.startsWith("/generate")
      ? "generate"
      : pathname.startsWith("/about")
        ? "about"
        : "graph" // default

  const isActive = path => (pathname.startsWith(path) ? "selected-page" : undefined)

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
          <li id="nav-graph">
            <a href="/graph" className={isActive("/graph")}>
              Graph
            </a>
          </li>
          <li id="nav-grid">
            <a href="/grid" className={isActive("/grid")}>
              Grid
            </a>
          </li>
          <li id="nav-generate">
            <a href="/generate" className={isActive("/generate")}>
              Generate (beta)
            </a>
          </li>
          <li id="nav-about">
            <a href="/about" className={isActive("/about")}>
              About
            </a>
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
