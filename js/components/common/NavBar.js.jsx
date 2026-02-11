import React from "react"
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome"
import { faDownload } from "@fortawesome/free-solid-svg-icons"
import { Tooltip } from "react-tooltip"
import GraphDropdown from "../graph/GraphDropdown"

/**
 * NavBar component.
 */
export function NavBar({ selected_page, open_modal, graphs = [], updateGraph }) {
  const isActive = page => (page === selected_page ? "selected-page" : undefined)
  const [showGraphDropdown, setShowGraphDropdown] = React.useState(false)
  const [dropdownTimeouts, setDropdownTimeouts] = React.useState([])

  const clearDropdownTimeouts = () => {
    dropdownTimeouts.forEach(timeout => clearTimeout(timeout))
    setDropdownTimeouts([])
  }

  const handleShowGraphDropdown = () => {
    clearDropdownTimeouts()
    setShowGraphDropdown(true)
  }

  const handleHideGraphDropdown = () => {
    const timeout = setTimeout(() => {
      setShowGraphDropdown(false)
    }, 500)
    setDropdownTimeouts(dropdownTimeouts.concat(timeout))
  }

  return (
    <nav className="row header">
      {/* Courseography Logo (also functions as an additional link to graph page) */}
      <div className="nav-left">
        <a href="/graph">
          <img
            id="courseography-header"
            src="/static/res/img/logo.png"
            alt="Courseography"
            data-context={selected_page}
          />
        </a>
      </div>

      {/* Navigation links */}
      <div className="nav-middle">
        <ul id="nav-links">
          <li
            id="nav-graph"
            className={`${isActive("graph")} ${graphs.length > 0 ? "show-dropdown-arrow" : ""}`}
            onMouseEnter={handleShowGraphDropdown}
            onMouseLeave={handleHideGraphDropdown}
          >
            <a href="/graph">Graph</a>
            {selected_page === "graph" && (
              <GraphDropdown
                showGraphDropdown={showGraphDropdown}
                onMouseMove={handleShowGraphDropdown}
                onMouseLeave={handleHideGraphDropdown}
                graphs={graphs}
                updateGraph={updateGraph}
              />
            )}
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
        {(selected_page === "graph" || selected_page === "grid") && (
          <>
            <button
              id="nav-export"
              aria-label="Export"
              data-tooltip-content="Export"
              data-tooltip-id="Export"
              onClick={open_modal}
            >
              <FontAwesomeIcon icon={faDownload} />
            </button>
            <Tooltip
              id="Export"
              className="export-tooltip"
              place="left"
              variant="float"
            />
          </>
        )}
      </div>
    </nav>
  )
}
