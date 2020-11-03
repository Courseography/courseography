import React from "react";
import PropTypes from "prop-types";
import Graph from "./Graph";
import Sidebar from "./Sidebar";

export default class Container extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      currFocus: null,
      graphName: "",
      graphs: []
    }
    this.graph = React.createRef();
    this.sidebar = React.createRef();
  }

  componentWillMount() {
    this.getLocalGraph();
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
    $("#nav-export").click(() => {
      this.graph.current.openExportModal();
    });
  }

  updateGraph = graphName => {
    this.setState({ graphName: graphName.replace("-", " ") });
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
    if (this.state.currFocus === id) {
      this.setState({
        currFocus: null
      });
    } else {
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
          getLocalGraph={this.getLocalGraph}
          closeSidebar={() => this.sidebar.current.toggleSidebar("graph")}
          currFocus={this.state.currFocus}
          graphName={this.state.graphName}
        />
        <Sidebar
          ref={this.sidebar}
          currFocus={this.state.currFocus}
          graphs={this.state.graphs}
          graphName={this.state.graphName}
          highlightFocus={this.highlightFocus}
          reset={() => this.graph.current.reset()}
          updateGraph={this.updateGraph}
        />
      </div>
    )
  }
}

Container.propTypes = {
  start_blank: PropTypes.bool,
  edit: PropTypes.bool,
};
