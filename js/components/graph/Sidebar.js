import React from "react";
import PropTypes from "prop-types";

export default class Sidebar extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      hidden: true
    };
  }

  toggleSidebar = () => {
      this.setState({ hidden: !this.state.hidden });
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
            <div key={`active ${course}`} data-testid={`test ${course}`} className="course-selection">{course.toUpperCase()}</div>
          );
        })}
      </div>
    );
  }

  render() {
    const collapsedClass = this.state.hidden ? "collapsed" : "expanded";

    return (
      <div className={collapsedClass} id="sidebar" data-testid="test-toggle">
        {this.renderFCE()}
        <div className="sidebar-dropdown" data-testid="test-sidebar">
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
  nodesJSON: PropTypes.array
};
