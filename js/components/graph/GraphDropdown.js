import React from "react"
import PropTypes from "prop-types"

export default function GraphDropdown({showGraphDropdown, onMouseEnter, onMouseLeave, graphs = [], updateGraph}) {
  const className = showGraphDropdown && graphs.length > 0 
    ? "graph-dropdown-display" 
    : "hidden"
  
  return (
    <ul
      className={className}
      onMouseEnter={onMouseEnter}
      onMouseLeave={onMouseLeave}
      data-testid="test-graph-dropdown"
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
  onMouseEnter: PropTypes.func,
  onMouseLeave: PropTypes.func,
  graphs: PropTypes.array,
  updateGraph: PropTypes.func,
}
