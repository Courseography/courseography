import React from "react";
import PropTypes from "prop-types";

export default class Sidebar extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      hidden: true,
      courses: [],
    };
  }

  /**
   * If the active course props change, update the rendered courses that are in the sidebar.
   * @param  {prevProps} prevProps
   */
  componentDidUpdate(prevProps) {
    if (prevProps.activeCourses !== this.props.activeCourses) {
      var curr = this.props.activeCourses.map((course, i) => {
        return (
          <div key={i}>{course.toUpperCase()}</div>
        )
      });
      this.setState({ courses: curr });
    }
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
      <div id="courses">
        {this.state.courses}
      </div>
    )
  }

  render() {
    const flippedClass = !this.state.hidden ? "flip" : "";
    const sidebarClass = !this.state.hidden ? "opened" : "";
    const allHidden = !this.state.hidden ? "" : "hidden";
    const buttonPos = !this.state.hidden ? "440" : "0";

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
          {this.renderCourses()}
          <button id="reset" data-testid="test-reset" onClick={() => this.props.reset()}>Reset Selections</button>
        </div>
      </div>
    )
  }
}

Sidebar.propTypes = {
  fceCount: PropTypes.number,
  graphName: PropTypes.string,
  reset: PropTypes.func,
  activeCourses: PropTypes.array,
  nodesJSON: PropTypes.array,
  itemClick: PropTypes.func
};
