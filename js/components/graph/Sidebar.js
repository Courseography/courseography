import React from "react";
import PropTypes from "prop-types";

export default class Sidebar extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      hidden: true
    };
  }

  /**
   * Update the rendered courses that are in the sidebar.
   * @return {array} list of div's for each course that is active
   */
  getCourses = () => {
    let temp = [...this.props.activeCourses];
    return temp.map((course, i) => {
      return (
          <div key={i} id="course-selection">{course.toUpperCase()}</div>
      )
    });
  }

  toggleSidebar = location => {
    if (!this.state.hidden) {
      // close graph
      this.setState({
        hidden: true
      })
    } else if (this.state.hidden && location === "button") {
      // open graph
      this.setState({
        hidden: false
      });
    }
  }

  // Sidebar rendering methods
  /**
   * Render the FCE counter above the sidebar on the left side.
   * @return {HTMLDivElement} FCE to the DOM
   */
  renderFCE = () => {
    const fceString = Number.isInteger(this.props.fceCount) ? this.props.fceCount + ".0" : this.props.fceCount

    return (
      <div id="fce">
        <div id="fcecount" data-testid="test-fcecount">FCE Count: {fceString}</div>
      </div>
    )
  }

  renderCourses = () => {
    return (
      <div className="courses">
        {this.getCourses()}
      </div>
    )
  }

  render() {
    const flippedClass = !this.state.hidden ? "flip" : "";
    const sidebarClass = !this.state.hidden ? "opened" : "";
    const allHidden = !this.state.hidden ? "" : "hidden";
    const buttonPos = !this.state.hidden ? "350" : "0";

    return (
      <div style={{ height: "0px" }}>
        {this.renderFCE()}
        <div id="sidebar-button" style={{transform: `translateY(${buttonPos}px)`}} onClick={() => this.toggleSidebar("button")} data-testid="test-sidebar-button">
          <img id="sidebar-icon"
           className={flippedClass}
           src="/static/res/ico/tempSidebar.png"
          />
        </div>
        <div id="sidebar" className={`${allHidden} ${sidebarClass}`} data-testid="test-sidebar">
          <label id="selected-courses">Selected courses:</label>
          {this.renderCourses()}
          <button id="reset-selections" data-testid="test-reset" onClick={() => this.props.reset()}>Reset Selections</button>
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
};
