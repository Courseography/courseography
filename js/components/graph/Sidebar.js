import React from "react";
import PropTypes from "prop-types";
import Focus from "./Focus";

export default class Sidebar extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      contentHidden: true,
      focusDisabled: false,
      focusActive: false,
      graphActive: false,
      graphName: "",
      toggled: false
    }
  }

  componentWillUpdate(prevProps) {
    // Check to see if we have a graph from local storage on load
    if (this.state.graphName.length == 0 && prevProps.graphName !== this.state.graphName) {
      this.setState({ graphName: prevProps.graphName }, () => {
        this.handleGraph();
      });
    }
  }

  handleGraph = graphName => {
    if (graphName != undefined) {
      this.props.getGraph(graphName);
    } else {
      graphName = this.state.graphName;
    }
    // Enable Focuses nav if CS graph is selected
    if (graphName === "Computer Science") {
      this.setState({
        focusDisabled: false,
      });
    } else {
      this.setState({
        focusDisabled: true,
      });
    }
  }

  createGraphButtons = () => {
    const graphButtons = [];
    this.props.graphs.forEach((graph, i) => {
      const graphId = "graph-" + graph.id;
      const graphName = graph.title;
      graphButtons.push(
        <div
          className="graph-button"
          id={graphId}
          key={i}
          onClick={() => this.handleGraph(graphName)}
        >
          {graphName}
        </div>
      );
    });
    return graphButtons;
  }

  getFocusData = () => {
    const computerScienceFocusData = [
      ["sci", "Scientific Computing"],
      ["AI", "Artificial Intelligence"],
      ["NLP", "Natural Language Processing"],
      ["vision", "Computer Vision"],
      ["systems", "Computer Systems"],
      ["game", "Video Games"],
      ["HCI", "Human Computer Interaction"],
      ["theory", "Theory of Computation"],
      ["web", "Web Technologies"],
    ];
    const focusComponents = [];
    computerScienceFocusData.forEach((focus, i) => {
      const openDetails = this.props.currFocus == focus[0];
      focusComponents.push(
        <Focus
          key={i}
          pId={focus[0]}
          focusName={focus[1]}
          openDetails={openDetails}
          highlightFocus={(id) => this.props.highlightFocus(id)}
        />
      )
    })
    return focusComponents;
  }

  toggleSidebar = location => {
    if (this.state.toggled) {
      // close graph
      this.setState({
        contentHidden: true,
        focusActive: false,
        graphActive: true,
        toggled: false,
      })
    } else if (!this.state.toggled && location === "button") {
      // open graph
      this.setState({
        toggled: true,
        contentHidden: false,
        graphActive: true,
      });
    }
  }

  showFocuses = focus => {
    if (focus) {
      // show focuses
      this.setState({
        focusActive: true,
        graphActive: false
      });
    } else {
      // show graphs
      this.setState({
        focusActive: false,
        graphActive: true
      });
    }
  }

  render() {
    const contentHiddenClass = this.state.contentHidden ? "hidden" : "";
    const focusDisabled = this.state.focusDisabled ? "disabled" : "";
    const focusActiveClass = this.state.focusActive ? "active" : "";
    const focusHiddenClass = !this.state.focusActive ? "hidden" : "";
    const graphActiveClass = this.state.graphActive ? "active" : "";
    const graphHiddenClass = !this.state.graphActive ? "hidden" : "";
    const flippedClass = this.state.toggled ? "flip" : "";
    const sidebarClass = this.state.toggled ? "opened" : "";

    return (
      <div>
        <div id="sidebar" className={sidebarClass}>
          <div id="fce" className={contentHiddenClass}>
            <div id="fcecount">FCE Count: 0.0</div>
            <button id="reset" onClick={() => this.props.reset()}>Reset Graph</button>
          </div>
          <nav id="sidebar-nav">
            <ul>
              <li id="graphs-nav" className={graphActiveClass} onClick={() => this.showFocuses(false)}>
                <div>Graphs</div>
              </li>
              <li id="focuses-nav" className={`${focusActiveClass} ${focusDisabled}`} onClick={() => this.showFocuses(true)}>
                <div>Focuses</div>
              </li>
            </ul>
          </nav>

          <div id="focuses" className={focusHiddenClass}>
            {this.getFocusData()}
          </div>
          <div id="graphs" className={graphHiddenClass}>
            {this.createGraphButtons()}
          </div>
        </div>
        
        <div id="sidebar-button" onClick={() => this.toggleSidebar("button")}>
          <img id="sidebar-icon"
           className={flippedClass}
           src="static/res/ico/sidebar.png"
          />
        </div>
      </div>
    )
  }
}

Sidebar.propTypes = {
  currFocus: PropTypes.string,
  getLocalGraph: PropTypes.func,
  getGraph: PropTypes.func,
  graphs: PropTypes.array,
  graphName: PropTypes.string,
  highlightFocus: PropTypes.func,
  reset: PropTypes.func
};
