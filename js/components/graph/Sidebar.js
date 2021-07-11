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
    }
  }

  createGraphButtons = () => {
    return this.props.graphs.map((graph, i) => {
      return (
        <div
          className="graph-button"
          id={"graph-" + graph.id}
          data-testid={"test-graph-" + i}
          key={i}
          onClick={() => this.props.updateGraph(graph.title)}
        >
          {graph.title}
        </div>
      )
    });
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
    const graphActiveClass = this.state.graphActive === 1 ? "active" : "";

    return (
      <nav id="sidebar-nav">
        <ul>
          <li
            id="graphs-nav"
            className={graphActiveClass}
            onClick={() => this.showFocuses(false)}
            data-testid="test-graphs-nav"
          >
            <div className="nav-section">Graphs</div>
          </li>
        </ul>
      </nav>
    )
  }

  renderSidebarButtons = () => {
    const graphHiddenClass = this.state.graphActive === 0 ? "hidden" : "";

    return (
      <div>
        <div id="graphs" className={graphHiddenClass} data-testid="test-graph-buttons">
          {this.createGraphButtons()}
        </div>
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
  graphs: PropTypes.array,
  graphName: PropTypes.string,
  reset: PropTypes.func,
  updateGraph: PropTypes.func
};
