import React from 'react';
import ReactDOM from 'react-dom';

  // ========================================

  class Sidebar extends React.Component {
    constructor(props) {
      super(props);
      this.state = {
        toggled: false
      };
    }

    toggleSidebar = location => {
      if (this.state.toggled) {
        this.setState({
          toggled: false,
        })
      } else if (!this.state.toggled && location === "button") {
        this.setState({
          toggled: true,
        });
      }
    }

    renderSidebar = () => {
      const contentHiddenClass = this.state.contentHidden ? "hidden" : "";

      return (
        <div className={contentHiddenClass}>
        <p id="title">Generate Prerequisites:</p>
        <div>
        <ul>
        <li><p class="filter">Enter Course(s): </p>
        <input class="filter" placeholder="..."/></li>
        <li><p class="filter">Exclude Courses: </p>
        <input class="filter" placeholder="..."/></li>
        <li><p class="filter">Departments: </p>
        <input class="filter" placeholder="..."/>
        <ul>
          <li>
            <p class="filter">How many layers of courses from other department(s) do you want to include? </p>
            <input class="filter" placeholder="..."/>
          </li>
        </ul>
        </li>
        <li><p class="filter">Number of layers(?): </p>
        <input class="filter" placeholder="..."/></li> 
        <li><p class="filter">Faulties: </p>
        <input class="filter" placeholder="..."/></li>                           
        <li><p class="filter">Campuses: </p>
        <input class="filter" placeholder="..."/></li>
        <li><p class="filter">Exclude courses external to any university campus: </p>
        <input type="checkbox" class="filter"/></li>
        <li><p class="filter">Exclude grade requirements: </p>
        <input type="checkbox" class="filter"/></li>
        </ul>
        </div>
        </div>
      )
    }

    render() { 
      const flippedClass = this.state.toggled ? "flip" : "";
      const sidebarClass = this.state.toggled ? "opened" : "hidden";

      return (
        <div id="container">
        <div id="sidebar" className={sidebarClass}>
          {this.renderSidebar()}
        </div>

        <div id="sidebar-button" onClick={() => this.toggleSidebar("button")}>
            <img id="sidebar-icon"
            className={flippedClass}
            src='https://cdn4.iconfinder.com/data/icons/defcon/512/shift-512.png'/> 
          </div>
        </div>
        
      );
    }
  }

  ReactDOM.render(
    <Sidebar />,
    document.getElementById('root')
  );