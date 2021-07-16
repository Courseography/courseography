import React from "react";
import PropTypes from "prop-types";

export default class Sidebar extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      contentHidden: true,
      toggled: false,
    };
  }

  getCurrentCourses = () => {
    return <div>Test</div>
  }

  toggleSidebar = location => {
    if (this.state.toggled) {
      // close graph
      this.setState({
        toggled: false,
        contentHidden: true,
      })
    } else if (!this.state.toggled && location === "button") {
      // open graph
      this.setState({
        toggled: true,
        contentHidden: false,
      });
    }
  }

  // Sidebar rendering methods
  renderFCE= () => {
    const fceString = Number.isInteger(this.props.fceCount) ? this.props.fceCount + ".0" : this.props.fceCount

    return (
      <div id="fce">
        <div id="fcecount" data-testid="test-fcecount">FCE Count: {fceString}</div>
      </div>
    )
  }

  renderSearchBar = () => {
    const searchHidden = this.state.toggled ? "" : "hidden";

    return (
      <form action="/" method="get" className={searchHidden}>
        <input
            type="text"
            id="header-search"
            placeholder="Search courses"
            name="s"
        />
        <input type="submit" value="search"/>
    </form>
    )
  }

  renderCourses = () => {
    const coursesHidden = this.state.toggled ? "" : "hidden";

    return (
      <div id="courses" className={coursesHidden}>
        {this.getCurrentCourses}
      </div>
    )
  }

  render() {
    const flippedClass = this.state.toggled ? "flip" : "";
    const sidebarClass = this.state.toggled ? "opened" : "";
    const currBackGroundColor = this.state.toggled ? "#ffffff" : "";
    const resetHidden = this.state.toggled ? "" : "hidden";

    return (
      <div>
        {this.renderFCE()}
        <div id="sidebar-button" onClick={() => this.toggleSidebar("button")} data-testid="test-sidebar-button">
          <img id="sidebar-icon"
           className={flippedClass}
           src="/static/res/ico/tempSidebar.png"
          />
        </div>
        <div id="sidebar" className={sidebarClass} style={{ backgroundColor: currBackGroundColor }} data-testid="test-sidebar">
          {this.renderSearchBar()}
          {/* {this.renderCourses()} */}
          <button id="reset" data-testid="test-reset" className={resetHidden} onClick={() => this.props.reset()}>Reset Selections</button>
        </div>
      </div>
    )
  }
}

Sidebar.propTypes = {
  fceCount: PropTypes.number,
  graphs: PropTypes.array,
  graphName: PropTypes.string,
  reset: PropTypes.func,
  updateGraph: PropTypes.func
};
