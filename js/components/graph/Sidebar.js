import React from "react";
import PropTypes from "prop-types";

export default class Sidebar extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      contentHidden: true,
      graphActive: 0,
      graphName: "",
      toggled: false
    };
  }

  componentWillUpdate(prevProps) {
    if (prevProps.graphName !== this.state.graphName) {
      this.setState({ graphName: prevProps.graphName });
      //PRCOM: You can delete this state and prop entirely, I don't think it's used any more.
    }
  }

  toggleSidebar = location => {
    if (this.state.toggled) {
      // close graph
      this.setState({
        contentHidden: true,
        graphActive: 1,
        toggled: false,
      })
    } else if (!this.state.toggled && location === "button") {
      // open graph
      this.setState({
        toggled: true,
        contentHidden: false,
        graphActive: 1,
      });
    }
  }

  // Sidebar rendering methods
  renderSidebarHeader= () => {
    const contentHiddenClass = this.state.contentHidden ? "hidden" : "";
    const fceString = Number.isInteger(this.props.fceCount) ? this.props.fceCount + ".0" : this.props.fceCount

    return (
      <div id="fce" className={contentHiddenClass}>
        <div id="fcecount" data-testid="test-fcecount">FCE Count: {fceString}</div>
        <button id="reset" data-testid="test-reset" onClick={() => this.props.reset()}>Reset Selection</button>
      </div>
    )
  }

  renderSidebarNav = () => {
    return (
      <nav id="sidebar-nav">
        <ul>
        </ul>
      </nav>
    )
  }

  renderSidebarButtons = () => {
    return (
      <div>
      </div>
    )
  }

  render() {
    const flippedClass = this.state.toggled ? "flip" : "";
    const sidebarClass = this.state.toggled ? "opened" : "";

    return (
      <div>
        <div id="sidebar" className={sidebarClass} data-testid="test-sidebar">
          {this.renderSidebarHeader()}
          {this.renderSidebarNav()}
          {this.renderSidebarButtons()}
        </div>

        <div id="sidebar-button" onClick={() => this.toggleSidebar("button")} data-testid="test-sidebar-button">
          <img id="sidebar-icon"
           className={flippedClass}
           src="/static/res/ico/sidebar.png"
          />
        </div>
      </div>
    )
  }
}

Sidebar.propTypes = {
  fceCount: PropTypes.number,
  graphName: PropTypes.string,
  reset: PropTypes.func,
};
