import React from "react";
import ReactDOM from "react-dom";

import Graph from "../graph/Graph";

class Generate extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      courses: null,
      taken: null,
      departments: null,
      maxDepth: null,
      location: null,
      includeRaws: null,
      includeGrades: null
    };

    this.graph = React.createRef();
  }

  generateGraph = (event) => {
    event.preventDefault();

    let newState = {};

    const courses = document.getElementById("courses").value;
    if (courses === "") {
      alert("Cannot generate graph -- no courses entered!");
      return;
    }

    newState["courses"] = courses.split(",");

    const taken = document.getElementById("excluded").value;
    if (taken !== "") {
      newState["taken"] = taken.split(",");
    }

    let departments = document.getElementById("departments").value;
    if (departments !== "") {
      newState["departments"] = departments.split(",");
    }

    const maxDepth = document.getElementById("maxDepth").value;
    if (maxDepth !== "") {
      newState["maxDepth"] = maxDepth;
    }

    const location = Array.from(document.querySelectorAll('#location option:checked')).map(el => el.value);
    if (location.length !== 0) {
      newState["location"] = location
    }

    newState["includeRaws"] = document.getElementById("includeRaws").checked;
    newState["includeGrades"] = document.getElementById("includeGrades").checked;

    this.setState(newState, this.getGraph);
  }

  getGraph = () => {
    // temporary data until we can parse user input
    const data = {};

    for (let key in this.state) {
      if (this.state[key] !== null) {
        data[key] = this.state[key];
      }
    }

    const putData = {
      method: "PUT",
      headers: {
       "Content-Type": "application/json"
      },
      body: JSON.stringify(data) // We send data in JSON format
    };

    fetch("graph-generate", putData)
      .then(res => res.json())
      .then(data => {
        var regionsList = [];
        var nodesList = [];
        var hybridsList = [];
        var boolsList = [];
        var edgesList = [];

        var labelsList = data.texts.filter(function(entry) {
          return entry.rId.startsWith("tspan");
        });

        data.shapes.forEach(function(entry) {
          if (entry.type_ === "Node") {
            nodesList.push(entry);
          } else if (entry.type_ === "Hybrid") {
            hybridsList.push(entry);
          } else if (entry.type_ === "BoolNode") {
            boolsList.push(entry);
          }
        });

        data.paths.forEach(function(entry) {
          if (entry.isRegion) {
            regionsList.push(entry);
          } else {
            edgesList.push(entry);
          }
        });
        this.graph.current.setState({
          labelsJSON: labelsList,
          regionsJSON: regionsList,
          nodesJSON: nodesList,
          hybridsJSON: hybridsList,
          boolsJSON: boolsList,
          edgesJSON: edgesList,
          width: data.width,
          height: data.height,
          zoomFactor: 1,
          horizontalPanFactor: 0,
          verticalPanFactor: 0,
        });
      })
      .catch((err) => {
        console.log("err :>> ", err);;
      }
    )
  }

  render() {
    return (
      <div style={{'display': 'flex', 'flexDirection': 'row'}}>
      <div id="generateDiv" style={{'position': 'initial'}}>
        <h1 id="header-title">Generate Prerequisite Graph</h1>
        <form id="generateForm">
            <div id="main-filter">
              <label htmlFor="courses">Courses:</label>
              <input id="courses" name="courses" type="text"/>
            </div>

          <h2 id="filter-title">Optional filters</h2>

          <ul>
            <li>
              <label htmlFor="excluded">Exclude Courses:</label>
              <input id="excluded" name="excluded" type="text"/>
            </li>

            <li>
              <label htmlFor="departments">Include Departments:</label>
              <input id="departments" name="departments" type="text" placeholder="CSC, MAT, STA"/>
            </li>

            <li>
              <label htmlFor="maxDepth">Depth of prerequisite chain to include:</label>
              <input id="maxDepth" name="maxDepth" type="number" min="0" step="1"/>
            </li>

            <li>
              <label htmlFor="location">Campus:</label>
              <select id="location" name="location" multiple>
                <option value="utsg" selected>St. George</option>
                <option value="utm">Mississauga</option>
                <option value="utsc">Scarborough</option>
              </select>
            </li>

            <li>
              <label htmlFor="includeRaws">Include non-course prerequisites:</label>
              <input id="includeRaws" name="includeRaws" type="checkbox" />
            </li>

            <li>
              <label htmlFor="includeGrades">Include grade-based prerequisites:</label>
              <input id="includeGrades" name="includeGrades" type="checkbox" />
            </li>

          </ul>

          <button id="submit" onClick={this.generateGraph}>
            Submit
          </button>
      </form>
    </div>

    <Graph
        ref={this.graph}
        start_blank={true}
    />
    </div>
    )
  }
}

ReactDOM.render(
  <Generate />,
  document.getElementById("generateRoot")
);
