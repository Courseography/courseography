import React from "react";
import PropTypes from "prop-types";
import Graph from "./Graph";
import Sidebar from "./Sidebar";
import * as focusInfo from "./sidebar/focus_descriptions";

export default class Container extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      currFocus: null,
      graphName: null,
      graphs: []
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
    
    // Need to use jQuery because nav-export is still a Haskell generated HTML component
    let currGraph = this.graph.current;
    $("#nav-export").click(function() {
      currGraph.openExportModal();
    });
  }

  getLocalGraph = () => {
    // Gets graph from local storage, if it exists
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

  highlightFocus = id => {
    if (this.graph.current.state.highlightedNodes == focusInfo[id + "FocusList"]) {
      this.graph.current.highlightFocuses([]);
      this.setState({
        currFocus: null
      });
    } else {
      this.graph.current.highlightFocuses(focusInfo[id + "FocusList"]);
      this.setState({
        currFocus: id
      });
    }
  }

  render() {
    return (
      <div>
        <Graph
          ref={this.graph}
          initialDrawMode="draw-node"
          edit={this.props.edit}
          start_blank={this.props.start_blank}
          getLocalGraph={() => this.getLocalGraph()}
          closeSidebar={() => this.sidebar.current.toggleSidebar("graph")}
        />
        <Sidebar
          ref={this.sidebar}
          currFocus={this.state.currFocus}
          getGraph={(name) => this.graph.current.getGraph(name)}
          graphs={this.state.graphs}
          graphName={this.state.graphName}
          highlightFocus={(id) => this.highlightFocus(id)}
          reset={() => this.graph.current.reset()}
        />
      </div>
    )
  }
}

Container.propTypes = {
  start_blank: PropTypes.bool,
  edit: PropTypes.bool,
};
