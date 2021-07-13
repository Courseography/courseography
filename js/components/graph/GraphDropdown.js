import React from "react";
import PropTypes from "prop-types";

export default class GraphDropdown extends React.Component{

	createDropdownColumn(columnHeader, column){

		return (
			<div>
				{<h4>{columnHeader}</h4>}
					<ul>
					{column.map((graph, i) => {
						return <li
										key={i}
										className="graph-dropdown-item"
										onClick={() => this.props.updateGraph(graph.title)}
										data-testid={"test-graph-" + i}
										>
											{graph.title}
										</li>
					})}

					</ul>
			</div>
		);
	}

	createDropdown(dropdownClassName, onMouseMove, onMouseleave, updateGraph, leftOffset){
		let officialColumn = [];
		let businessColumn = [];
		let sciencesColumn = [];
		let socialStudiesColumn = [];
		let languagesColumn = [];
		let graphToColumnMapping = {
			"Computer Science": officialColumn,
			"(unofficial) Rotman": businessColumn,
			"(unofficial) Statistics": sciencesColumn,
			"(unofficial) Biochemistry": sciencesColumn,
			"(unofficial) Cell & Systems Biology": sciencesColumn,
			"(unofficial) Linguistics": sciencesColumn,
			"(unofficial) History and Philosophy of Science": socialStudiesColumn,
			"(unofficial) History": socialStudiesColumn,
			"(unofficial) Geography": socialStudiesColumn,
			"(unofficial) Aboriginal": socialStudiesColumn,
			"(unofficial) East Asian Studies": socialStudiesColumn,
			"(unofficial) Economics": socialStudiesColumn,
			"(unofficial) Finnish": languagesColumn,
			"(unofficial) Italian": languagesColumn,
			"(unofficial) Spanish": languagesColumn,
			"(unofficial) Portuguese": languagesColumn,
			"(unofficial) German": languagesColumn,
			"(unofficial) Estonian": languagesColumn,
			"(unofficial) Slavic": languagesColumn,
			"(unofficial) English": languagesColumn
		}

		this.props.graphs.forEach((graph) => {
			console.log(graph.title);
			graphToColumnMapping[graph.title].push(graph)
		});
		console.log({officialColumn});
		console.log({businessColumn});
		console.log({sciencesColumn});
		console.log({socialStudiesColumn});
		console.log({languagesColumn});
		return (
		<div className={dropdownClassName} style={{left:leftOffset}}>
			<div className="official-section">
					<h2> Official Graphs </h2>
					{this.createDropdownColumn("Placeholder", officialColumn)}
				</div>
			<div className="unofficial-section">
					<h2> Unofficial Graphs</h2>
					<div className="unofficial-columns">
						{this.createDropdownColumn("Business", businessColumn)}
						{this.createDropdownColumn("Sciences", sciencesColumn)}
						{this.createDropdownColumn("Social Studies", socialStudiesColumn)}
						{this.createDropdownColumn("Languages", languagesColumn)}
					</div>
			</div>

		</div>
		);
	}

	render() {
		let halfComponentWidth = 328;
		let graphTabLeft = 790;
		if (document.querySelector["a[href='/graph'"]){
			graphTabLeft = document.querySelector("a[href='/graph'").getBoundingClientRect().left;
		}
		// var className = this.props.showGraphDropdown ? "graph-dropdown-display": "graph-dropdown-hidden";
		var className = "graph-dropdown-display";

		return this.createDropdown(className, this.props.onMouseMove, this.props.onMouseLeave, this.props.updateGraph, graphTabLeft - halfComponentWidth);
	}
}

GraphDropdown.propTypes = {
	showGraphDropdown: PropTypes.bool,
	onMouseMove: PropTypes.func,
	onMouseLeave: PropTypes.func,
	graphs: PropTypes.array,
	updateGraph: PropTypes.func
}