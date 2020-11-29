import React from 'react';
import ReactDOM from 'react-dom';

  // ========================================

  class Generate extends React.Component {
    constructor(props) {
      super(props);
      this.state = {
      };
    }

    generate = () => {
      alert('this should generate a graph');
    }

    render() { 

      return (
        <div id="generateDiv">

          <div id="header">
            <div id="header-title"> PREREQUISITE GENERATOR</div>
            <div id="main-filter">
              <input  placeholder="Enter Courses"/>
            </div>
          </div>

          <p id="filter-title">OPTIONAL FILTERS</p>

          <ul>
            <li>
              <p class="filter">Exclude Courses: </p>
              <input class="filter" placeholder="..."/>
            </li>
            
            <li>
              <p class="filter">Include Departments: </p>
              <input class="filter" placeholder="..."/>
            </li>

            {/* <li><p class="filter">How many layers of courses from other department(s) do you want to include? </p>
              <input class="filter" placeholder="..."/>
            </li> */}

            <li>
              <p class="filter">(?) Number of layers: </p>
              <input class="filter" placeholder="..."/>
            </li> 

            <li>
              <p class="filter">Include Faculties: </p>
              <input class="filter" placeholder="..."/>
            </li>  

            <li>
              <p class="filter">Include Campuses: </p>
              <input class="filter" placeholder="..."/>
            </li>

            <li>
              <p class="filter">Exclude courses external to campuses: </p>
              <input type="checkbox" class="filter"/>
            </li>

            <li>
              <p class="filter">Exclude grade requirements: </p>
              <input type="checkbox" class="filter"/>
            </li>

          </ul>

          <div id="submit" onClick={() => this.generate()}>
            <p id="submit-text"> SUBMIT</p>
          </div>

        </div>
      )
    }
  }

  ReactDOM.render(
    <Generate />,
    document.getElementById('generateDiv')
  );
