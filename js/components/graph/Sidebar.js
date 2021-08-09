import React from "react";
import PropTypes from "prop-types";

export default class Sidebar extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      collapsed: true,
      results: []
    };
  }

  toggleSidebar = () => {
      this.setState({ collapsed: !this.state.collapsed });
  }

  filteredSearch = (posts, query) => {
    if (!query || !posts) {
      return;
    }

    return posts.filter((post) => {
      return post.includes(query);
    });
  }

  // Sidebar rendering methods
  /**
   * Render the FCE counter above the sidebar on the left side.
   * @return {HTMLDivElement} FCE to the DOM
   */
  renderFCE = () => {
    const fceString = Number.isInteger(this.props.fceCount) ? this.props.fceCount + ".0" : this.props.fceCount

    return (
      <div className="fcecount" data-testid="test-fcecount">FCE Count: {fceString}</div>
    )
  }

  /**
   * Render the dropdown results within the sidebar dropdown.
   * @return {HTMLDivElement} Searchbar to the DOM
   */
   renderDropdown = () => {
    if (this.props.nodesJSON) {

      let showDropdown = this.state.results ? '' : 'hidden';
      let masterDropdown = `${showDropdown} search-dropdown`;
      return (
          <ul className={masterDropdown} data-testid='test-searchDropdown'>
            {this.state.results?.map((result) =>
            <li key={`search ${result}`} data-testid={`test-search-${result}`}className="dropdown-item" onClick={() => this.props.courseClick(result)}>
              {result.toUpperCase()}
            </li>
            )}
          </ul>
        )
    }
  }

  /**
   * Render courses that are in the sidebar.
   * @return {HTMLBodyElement} list of div's for each course that is active
   */
  renderActiveCourses = () => {
    let temp = this.props.activeCourses ? [...this.props.activeCourses] : [];
    // sort the list of rendered courses, alphabetically
    temp.sort((a,b) => a.localeCompare(b));
    return (
      <div className="courses" data-testid="test-course-selection">
        {temp.map((course) => {
          return (
            <div key={`active ${course}`}
            data-testid={`test ${course}`}
            onClick={() => this.props.courseClick(course)}
            className="course-selection">
              {course.toUpperCase()}
            </div>
          );
        })}
      </div>
    );
  }

  render() {
    const collapsedClass = this.state.collapsed ? "collapsed" : "expanded";
    const masterSidebarClass = `${collapsedClass} sidebar`;

    return (
      <div className={masterSidebarClass} data-testid="test-toggle">
        {this.renderFCE()}
        <div className="sidebar-dropdown" data-testid="test-sidebar">
          <div>
            <label htmlFor="header-search">
              {/* For text to speech purposes */}
              <span className="label-hidden">Search courses</span>
            </label>
            <input id="header-search" className="search-bar" data-testid="test-search-bar" type="text" onChange={(e) => {this.setState({ results: this.filteredSearch(this.props.nodesJSON, e.target.value) })}}/>
          </div>
          {this.renderDropdown()}
          <h3 className="selected-courses">Selected courses</h3>
          {this.renderActiveCourses()}
          <button className="reset-selections" data-testid="test-reset" onClick={() => this.props.reset()}>Reset Selections</button>
        </div>
        <div className="sidebar-button" onClick={() => this.toggleSidebar()} data-testid="test-sidebar-button">
          <img id="sidebar-icon" src="/static/res/ico/sidebar.png"/>
        </div>
      </div>
    )
  }
}

Sidebar.propTypes = {
  fceCount: PropTypes.number,
  reset: PropTypes.func,
  activeCourses: PropTypes.instanceOf(Set),
  nodesJSON: PropTypes.array,
  courseClick: PropTypes.func
};
