import React from "react";
import PropTypes from "prop-types";

export default class GraphDropdown extends React.Component{

	generateDropdownItems(column){
		return column.map((graph, i) => {
			let titleStart = graph.title.lastIndexOf(")");
			let graphDisplayTitle = (titleStart === -1) ? graph.title : graph.title.slice(titleStart + 2);
			return <li
							key={i}
							className="graph-dropdown-item"
							onClick={() => this.props.updateGraph(graph.title)}
							data-testid={"test-graph-" + i}
							>
								{graphDisplayTitle}
							</li>
		})
	}

	createDropdown(dropdownClassName, leftOffset){
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
			graphToColumnMapping[graph.title].push(graph)
		});

		return (
		<ul
			className={dropdownClassName}
			style={{left:leftOffset}}
			onMouseMove={this.props.onMouseMove}
			onMouseLeave={this.props.onMouseLeave}
			data-testid={"test-graph-dropdown"}
		>
			<div>
				<h4>Official Graphs</h4>
				{this.generateDropdownItems(officialColumn)}
			</div>
			<div>
				<h4>(Unofficial) Sciences</h4>
				{this.generateDropdownItems(sciencesColumn)}
			</div>
			<div>
				<h4>(Unofficial) Business</h4>
				{this.generateDropdownItems(businessColumn)}
			</div>
			<div>
				<h4>(Unofficial) Social Studies</h4>
				{this.generateDropdownItems(socialStudiesColumn)}
			</div>
			<div>
				<h4>(Unofficial) Languages</h4>
				{this.generateDropdownItems(languagesColumn)}
			</div>
		</ul>

		);
	}

	render() {
		let halfComponentWidth = 412;
		let graphTabLeft = 790;
		if (document.querySelector["a[href='/graph'"]){
			graphTabLeft = document.querySelector("a[href='/graph'").getBoundingClientRect().left;
		}
		var className = this.props.showGraphDropdown ? "graph-dropdown-display": "graph-dropdown-hidden";
		return this.createDropdown(className, graphTabLeft - halfComponentWidth);
	}
}

GraphDropdown.propTypes = {
	showGraphDropdown: PropTypes.bool,
	onMouseMove: PropTypes.func,
	onMouseLeave: PropTypes.func,
	graphs: PropTypes.array,
	updateGraph: PropTypes.func
}