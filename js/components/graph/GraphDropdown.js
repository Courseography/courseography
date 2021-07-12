import React from "react";
import PropTypes from "prop-types";

export default class GraphDropdown extends React.Component{
	render() {
		let halfComponentWidth = 328;
		let graphTabLeft = 790;
		if (document.querySelector["a[href='/graph'"]){
			graphTabLeft = document.querySelector("a[href='/graph'").getBoundingClientRect().left;
		}
		var className = this.props.showGraphDropdown ? "graph-dropdown-display": "graph-dropdown-hidden";

		// using onMouseMove rather than mouse enter to handle case where the nav-graph's OnMouseLeave handler is called after GraphDropdown's mouseenter
		// in the case where the user moves their mouse from the nav-graph <a> to GraphDropdown
    return (
		<ul
			className={className}
			onMouseMove={() => {console.log("Mouse is moving");this.props.onMouseMove();}}
			// onMouseLeave={() => {console.log("Mouse is leaving"); this.props.onMouseLeave();}}
			onMouseLeave={() => {console.log("Mouse is leaving graph dropdown");this.props.onMouseLeave()}}
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
	onMouseMove: PropTypes.func,
	onMouseLeave: PropTypes.func,
	graphs: PropTypes.array,
	updateGraph: PropTypes.func
}