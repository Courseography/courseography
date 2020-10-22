import React from "react";
import PropTypes from "prop-types";
import Focus from "./Focus";


export default class Sidebar extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      toggled: false,
      hidden: true,
      focusDisabled: false,
      focusHidden: true,
      focusActive: false,
      graphActive: false
    }
    this.toggleSidebar = this.toggleSidebar.bind(this);
    this.showFocuses = this.showFocuses.bind(this);
    this.showGraphs = this.showGraphs.bind(this);
  }

  componentDidMount() {
    this.handleGraph();
  }

  handleGraph(graphName) {
    if (graphName != undefined) {
      this.props.getGraph(graphName);
    } else {
      console.log("Sidebar component is getting the local graph name.");
      graphName = this.props.getLocalGraph();
      console.log('Sidebar component just got this graphName :>> ', graphName);
    }
    // Enable Focuses nav if CS graph is selected
    if (graphName === "Computer Science") {
      this.setState({
        focusDisabled: false,
      })
    } else {
      this.setState({
        focusDisabled: true,
      })
    }
  }

  createGraphButtons() {
    const graphButtons = [];
    this.props.graphs.forEach((graph, i) => {
      const graphId = "graph-" + graph.id;
      const graphName = graph.title;
      graphButtons.push(
        <div
          id={graphId}
          className="graph-button"
          key={i}
          onClick={() => this.handleGraph(graphName)}>
          {graphName}
        </div>
      );
    });
    return graphButtons;
  }

  getFocusData() {
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
      focusComponents.push(
        <Focus
          key={i}
          pId={focus[0]}
          focusName={focus[1]}
          highlightFocus={(id) => this.props.highlightFocus(id)}
        />
      )
    })
    return focusComponents;
  }

  toggleSidebar(location) {
    if (this.state.toggled) {
      this.setState({
        toggled: false,
        hidden: true,
        focusHidden: true,
        focusActive: false,
        graphActive: true,
        sidebarFlipped: false,
      })
    } else if (!this.state.toggled && location === "button") {
      // open graph
      this.setState({
        toggled: true,
        hidden: false,
        graphActive: true,
        sidebarFlipped: true,
      })
    }
  }

  showFocuses() {
    this.setState({
      focusHidden: false,
      focusActive: true,
      graphActive: false,
    });
  }

  showGraphs() {
    this.setState({
      focusHidden: true,
      focusActive: false,
      graphActive: true,
    });
  }

  render() {
    const hiddenClass = this.state.hidden ? "hidden" : "";
    const focusClass = this.state.focusHidden ? "hidden" : "";
    const focusDisabled = this.state.focusDisabled ? "disabled" : "";
    const focusActiveClass = this.state.focusActive ? "active" : "";
    const graphActiveClass = this.state.graphActive ? "active" : "";
    const flippedClass = this.state.toggled ? "flip" : "";
    const sidebarClass = this.state.toggled ? "opened" : "";
    return (
      <div>
        <div id="sidebar" className={sidebarClass}>
          <div id="fce" className={hiddenClass}>
            <div id="fcecount" className={hiddenClass}>FCE Count: 0.0</div>
            <button id="reset" className={hiddenClass} onClick={() => this.props.reset()}>Reset Graph</button>
          </div>
          <nav id="sidebar-nav">
            <ul>
              <li id="graphs-nav" className={graphActiveClass} onClick={this.showGraphs}>
                <div>Graphs</div>
              </li>
              <li id="focuses-nav" className={`${focusActiveClass} ${focusDisabled}`} onClick={(event) => this.showFocuses(event)}>
                <div>Focuses</div>
              </li>
            </ul>
          </nav>

          <div id="focuses" className={focusClass}>
            {this.getFocusData()}
          </div>
          <div id="graphs" className={hiddenClass}>
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
  reset: PropTypes.func,
  graphs: PropTypes.array,
  getGraph: PropTypes.func,
  highlightFocus: PropTypes.func,
  getLocalGraph: PropTypes.func
};
