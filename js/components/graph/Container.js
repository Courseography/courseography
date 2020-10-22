import React from "react";
import Graph from "./Graph";
import Sidebar from "./Sidebar";
import PropTypes from "prop-types";
import * as focusInfo from "./sidebar/focus_descriptions";

export default class Container extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      graphName: null,
      graphs: [],
    }
    this.graph = React.createRef();
    this.sidebar = React.createRef();
  }

  componentDidMount() {
    fetch('graphs').then(res => res.json()).then(
      (graphsData) => {
        this.setState({
          graphs: graphsData
        });
      },
      () => {
        throw "No graphs in database";
      }
    )
  }

  getLocalGraph() {
    if (this.state.graphName == null) {
      console.log('Container component does not have a graph name state so I will look it up in local storage.');
      let graphName;
      const params = new URL(document.location).searchParams;
      const urlSpecifiedGraph = params.get("dept");

      // HACK: Temporary workaround for giving the statistics department a
      // link to our graph.
      // Should be replaced with a more general solution.
      if (urlSpecifiedGraph === "sta") {
        graphName = "Statistics";
      } else if (urlSpecifiedGraph !== null) {
        graphName = "Computer Science";
      } else {
        graphName = localStorage.getItem("active-graph") || "Computer Science";
      }
      this.setState({ graphName: graphName });
      return graphName;
    }
    console.log("Container component already has the graph name. Here it is!");
    return this.state.graphName;
  }

  highlightFocus(id) {
    if (this.graph.current.state.highlightedNodes == focusInfo[id + "FocusList"]) {
      this.graph.current.highlightFocuses([]);
    } else {
      // TODO SET SOME CSS STUFF LOL
      // $(".details").css("height", "2px");
      this.graph.current.highlightFocuses(focusInfo[id + "FocusList"]);
    }
  }

  render() {
    return (
      <React.Fragment>
        <Graph
          ref={this.graph}
          start_blank={this.props.start_blank}
          edit={this.props.edit}
          getLocalGraph={() => this.getLocalGraph()}
          closeSidebar={() => this.sidebar.current.toggleSidebar("graph")}
          initialDrawMode="draw-node"
        />
        <Sidebar
          ref={this.sidebar}
          graphs={this.state.graphs}
          getGraph={(name) => this.graph.current.getGraph(name)}
          getLocalGraph={() => this.getLocalGraph()}
          highlightFocus={(id) => this.highlightFocus(id)}
        />
      </React.Fragment>
    )
  }
}

Container.propTypes = {
  start_blank: PropTypes.bool,
  edit: PropTypes.bool,
};
