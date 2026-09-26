import React from "react"

interface Graph {
  title: string
}

interface GraphDropdownProps {
  showGraphDropdown?: boolean
  onMouseEnter?: () => void
  onMouseLeave?: () => void
  graphs?: Graph[]
  updateGraph: (title: string) => void
}

export default function GraphDropdown({
  showGraphDropdown,
  onMouseEnter,
  onMouseLeave,
  graphs = [],
  updateGraph,
}: GraphDropdownProps) {
  const className =
    showGraphDropdown && graphs.length > 0 ? "graph-dropdown-display" : "hidden"

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
