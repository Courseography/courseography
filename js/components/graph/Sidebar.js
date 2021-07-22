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
       <div id="fcecount" data-testid="test-fcecount">FCE Count: {fceString}</div>
    )
  }

  /**
   * Render courses that are in the sidebar.
   * @return {array} list of div's for each course that is active
   */
   renderCourses = () => {
    let temp = this.props.activeCourses ? [...this.props.activeCourses] : [];
    // sort the list of rendered courses, purely numerically and ignoring course code (to group up by year)
    temp.sort((a,b) => a.slice(3, 5) - b.slice(3, 5));
    return temp.map((course) => {
      return (
          <div key={`active ${course}`} data-testid={`test ${course}`} id="course-selection">{course.toUpperCase()}</div>
      );
    });
  }

  renderActiveCourses = () => {
    return (
      <div className="courses" data-testid="test-course-selection">
        {this.renderCourses()}
      </div>
    );
  }

  render() {
    const openedClass = this.state.hidden ? "collapsed" : "expanded";

    return (
      <div className={openedClass} data-testid="test-toggle" onMouseEnter={() => this.props.toggleZoom()} onMouseLeave={() => this.props.toggleZoom()}>
        {this.renderFCE()}
        <div id="sidebar" data-testid="test-sidebar">
          <h3 id="selected-courses">Selected courses</h3>
          {this.renderActiveCourses()}
          <button id="reset-selections" data-testid="test-reset" onClick={() => this.props.reset()}>Reset Selections</button>
          <div id="sidebar-button" onClick={() => this.toggleSidebar()} data-testid="test-sidebar-button">
            <img id="sidebar-icon" src="/static/res/ico/sidebar.png"/>
          </div>
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
  toggleZoom: PropTypes.func
};
