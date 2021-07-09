import React from "react";
import PropTypes from "prop-types";

export default class GraphDropdown extends React.Component{
	render() {
		let halfComponentWidth = 328;
		let graphTabLeft = document.querySelector("a[href='/graph").getBoundingClientRect().left;
		var className = this.props.showGraphDropdown ? "graph-dropdown-display": "graph-dropdown-hidden";

    return (
		<ul
			className={className}
			onMouseEnter={this.props.onMouseEnter}
			onMouseLeave={this.props.onMouseLeave}
			data-testid={"test-graph-dropdown"}
			style={{left:graphTabLeft - halfComponentWidth}}
		>
      {this.props.graphs.map((graph, i) => {
      return <li
							key={i}
							className="graph-dropdown-button"
							onClick={() => this.props.updateGraph(graph.title)}
							data-testid={"test-graph-" + i}
							>
        			{graph.title}
      				</li>
    })}
    </ul>
		)
	}
}

GraphDropdown.propTypes = {
	showGraphDropdown: PropTypes.bool,
	onMouseEnter: PropTypes.func,
	onMouseLeave: PropTypes.func,
	graphs: PropTypes.array,
	updateGraph: PropTypes.func
}