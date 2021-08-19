import React from "react"
import PropTypes from "prop-types"

export default class GraphDropdown extends React.Component {
  render() {
    let className = "hidden"
    let graphTabLeft = 0
    if (this.props.graphs.length !== 0 && document.querySelector("#nav-graph")) {
      let navGraph = document.querySelector("#nav-graph")
      if (this.props.graphs.length === 0) {
        navGraph.classList.remove("show-dropdown-arrow")
      } else {
        if (!navGraph.classList.contains("show-dropdown-arrow")) {
          navGraph.classList.add("show-dropdown-arrow")
        }
        if (this.props.showGraphDropdown) {
          graphTabLeft = navGraph.getBoundingClientRect().left
          className = "graph-dropdown-display"
        }
      }
    }

    return (
      <ul
        className={className}
        onMouseMove={this.props.onMouseMove}
        onMouseLeave={this.props.onMouseLeave}
        data-testid={"test-graph-dropdown"}
        style={{ left: graphTabLeft }}
      >
        {this.props.graphs.map((graph, i) => {
          return (
            <li
              key={i}
              className="graph-dropdown-item"
              onClick={() => this.props.updateGraph(graph.title)}
              data-testid={"test-graph-" + i}
            >
              {graph.title}
            </li>
          )
        })}
      </ul>
    )
  }
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
