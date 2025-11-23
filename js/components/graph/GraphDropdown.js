import React from "react"
import PropTypes from "prop-types"

export default function GraphDropdown({showGraphDropdown, onMouseMove, onMouseLeave, graphs = [], updateGraph}) {
    let className = "hidden"
    let graphTabLeft = 0
    if (graphs.length !== 0 && document.querySelector("#nav-graph")) {
      const navGraph = document.querySelector("#nav-graph")
        if (graphs.length === 0) {
          navGraph.classList.remove("show-dropdown-arrow")
        } else {
          if (!navGraph.classList.contains("show-dropdown-arrow")) {
            navGraph.classList.add("show-dropdown-arrow")
          }
          if (showGraphDropdown) {
            graphTabLeft = navGraph.getBoundingClientRect().left
            className = "graph-dropdown-display"
          }
      }
    }
    return (
      <ul
        className={className}
        onMouseMove={onMouseMove}
        onMouseLeave={onMouseLeave}
        data-testid={"test-graph-dropdown"}
        style={{ left: graphTabLeft, top: "50px", zIndex: 1000
        }}
      >
        {graphs.map((graph, i) => {
          return (
            <li
              key={i}
              className="graph-dropdown-item"
              onClick={() => updateGraph(graph.title)}
              data-testid={"test-graph-" + i}
            >
              {graph.title}
            </li>
          )
        })}
      </ul>
    )
  }

GraphDropdown.defaultProps = {
  graphs: [],
}

GraphDropdown.propTypes = {
  showGraphDropdown: PropTypes.bool,
  onMouseMove: PropTypes.func,
  onMouseLeave: PropTypes.func,
  graphs: PropTypes.array,
  updateGraph: PropTypes.func,
}
