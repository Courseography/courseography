import React from 'react';
import ReactDOM from 'react-dom';
// import coursesToPrereqGraph from '../app/DynamicGraphs/GraphGenerator.hs';


class Generate extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
    };
  }

  generate = () => {
    // var inputs = document.getElementById("generateForm");
    // coursesToPrereqGraph([inputs.elements[0].value]);

    // temporary data until we can parse user input
    const data = {
      "courses":["CSC324H1"],
      "includeGrades": false,
      "includeRaws": false,
      "departments": ["CSC", "MAT"]
    }
    const putData = {
      method: 'PUT', // Method itself
      headers: {
       'Content-Type': 'application/json' // Indicates the content 
      },
      body: JSON.stringify(data) // We send data in JSON format
     }
    
    fetch('graph-generate', putData).then(res => res.json()).then(
      (graph) => {
        console.log('graph :>> ', graph);
      },
      () => {
        throw "Error. Cannot load graph.";
      }
    )
  }

  render() { 
    return (
      <div id="generateDiv">
        <form id="generateForm">
          <div id="header">
            <div id="header-title"> PREREQUISITE GENERATOR</div>
            <div id="main-filter">
              <input type="text" placeholder="Enter Courses"/>
            </div>
          </div>

          <p id="filter-title">OPTIONAL FILTERS</p>

          <ul>
            <li>
              <label >Exclude Courses: </label>
              <input type="text" placeholder="..."/>
            </li>
            
            <li>
              <label >Include Departments: </label>
              <input type="text"  placeholder="..."/>
            </li>

            {/* <li><label >How many layers of courses from other department(s) do you want to include? </label>
              <input  placeholder="..."/>
            </li> */}

            <li>
              <label >(?) Number of layers: </label>
              <input type="text"  placeholder="..."/>
            </li> 

            <li>
              <label >Include Faculties: </label>
              <input type="text"  placeholder="..."/>
            </li>  

            <li>
              <label >Include Campuses: </label>
              <input type="text"  placeholder="..."/>
            </li>

            <li>
              <label >Exclude courses external to campuses: </label>
              <input type="checkbox" />
            </li>

            <li>
              <label >Exclude grade requirements: </label>
              <input type="checkbox" />
            </li>

          </ul>

          <div id="submit" onClick={() => this.generate()}>
            <div id="submit-text" type="button"> SUBMIT </div>
            {/* <input id="submit-text" type="submit" value="SUBMIT"></input> */}
          </div>
      </form>
    </div>
    
    )
  }
}

ReactDOM.render(
  <Generate />,
  document.getElementById('generateDiv')
);
