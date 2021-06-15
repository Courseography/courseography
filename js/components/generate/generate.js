import 'core-js/stable';
import 'regenerator-runtime/runtime';

import React from "react";
import ReactDOM from "react-dom";

import Graph from "../graph/Graph";

class GenerateForm extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      courses: '',
      taken: '',
      departments: 'CSC, MAT, STA',
      maxDepth: 0,
      location: ['utsg'],
      includeRaws: false,
      includeGrades: false
    };

    this.graph = React.createRef();
  }

  handleInputChange = (event) => {
    const target = event.target;
    let value;
    if (target.type === 'checkbox') {
      value = target.checked;
    } else if (target.type === 'select-multiple') {
      value = Array.from(target.selectedOptions, option => option.value);
    } else if (target.type === 'number') {
      value = parseInt(target.value);
    } else {
      value = target.value;
    }
    const name = target.name;

    this.setState({
      [name]: value
    });
  }

  handleSubmit = (event) => {
    event.preventDefault();

    if (!this.state.courses.length) {
      alert("Cannot generate graph -- no courses entered!");
      return;
    }
    let data = {};

    for (let key in this.state) {
      if (['courses', 'taken', 'departments'].includes(key)) {
        data[key] = this.state[key].split(',').map(s => s.trim());
      } else {
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

    fetch("/graph-generate", putData)
      .then(res => res.json())
      .then(data => {
        var regionsList = [];
        var nodesList = [];
        var hybridsList = [];
        var boolsList = [];
        var edgesList = [];

        var labelsList = data.texts.filter(function(entry) {
            // filter for mark percentages, allow preceding characters for potential geq
            return entry.text.match(/.*[0-9]*%/g);
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
          verticalPanFactor: 0
        });
      })
      .catch((err) => {
        console.log("err :>> ", err);;
      }
    )
  }

  render() {
    return (
      <div style={{'display': 'flex', 'flexDirection': 'row', 'height': '100%'}}>
      <div id="generateDiv" style={{'position': 'initial', 'padding': '0 0.5em', 'height': '100%', 'fontSize': '12pt'}}>
        <h1 id="header-title">Search for courses</h1>
        <form id="generateForm">
          <input id="courses" name="courses" type="text"
                 placeholder="e.g., CSC207H1, CSC324H1"
                 value={this.state.courses}
                 onChange={this.handleInputChange} />

          <h2 id="filter-title">Optional filters</h2>

          <label htmlFor="departments">Only include courses these departments</label>
          <input id="departments" name="departments" type="text"
                  placeholder="Enter 3-letter department codes separated by commas"
                  value={this.state.departments}
                  onChange={this.handleInputChange}
                  style={{'marginBottom': '1em'}} />

          <label htmlFor="taken">Do not show these courses</label>
          <input id="taken" name="taken" type="text"
                  value={this.state.taken}
                  onChange={this.handleInputChange}
                  style={{'marginBottom': '1em'}}
                  placeholder="E.g., CSC207H1, CSC236H1" />

          <label htmlFor="maxDepth">Depth of prerequisite chain (0 shows all prerequisites)</label>
          <p><input id="maxDepth" name="maxDepth" type="number" min="0" step="1"
                  value={this.state.maxDepth}
                  onChange={this.handleInputChange}
                  style={{'marginBottom': '1em'}} />
          </p>

          {/* <label htmlFor="location">Campus</label>
          <select id="location" name="location" multiple
            value={this.state.location}
            onChange={this.handleInputChange}
            style={{'vertical-align': 'text-top', 'margin-left': '1em', 'margin-bottom': '1em', 'color': 'black'}} >
            <option value="utsg">St. George</option>
            <option value="utm">Mississauga</option>
            <option value="utsc">Scarborough</option>
          </select>

          <p>
          <label htmlFor="includeRaws">Include non-course prerequisites</label>
          <input id="includeRaws" name="includeRaws" type="checkbox"
                  value={this.state.includeRaws}
                  onChange={this.handleInputChange}
                  style={{'margin-left': '1em', 'vertical-align': 'middle'}} />
          </p>

          <label htmlFor="includeGrades">Include grade-based prerequisites</label>
          <input id="includeGrades" name="includeGrades" type="checkbox"
                  value={this.state.includeGrades}
                  onChange={this.handleInputChange}
                  style={{'margin-left': '1em', 'vertical-align': 'middle'}} /> */}

          <div style={{'marginTop': '1em', 'width': '100%', 'display': 'flex', 'justifyContent': 'center'}}>
          <button id="submit" onClick={this.handleSubmit}>
            Generate Graph
          </button>
          </div>
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
  <GenerateForm />,
  document.getElementById("generateRoot")
);
