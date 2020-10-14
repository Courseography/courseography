import React from "react";
import Graph from "./Graph";
import Sidebar from "./Sidebar";
import PropTypes from "prop-types";
// import * as focusInfo from "./sidebar/focus_descriptions";
// import * as sidebarDivs from "./sidebar/sidebar_divs.js";

export default class Container extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      graphs: [],
    }
    this.graphComponent = React.createRef();
    this.sidebarComponent = React.createRef();
  }

  componentDidMount() {
    fetch('graphs').then(res => res.json()).then(
      (graphsData) => {
        this.setState({
          graphs: graphsData
        })
      },
      () => {
        throw "No graphs in database";
      }
    )
  }

  render() {
    return (
      <React.Fragment>
        <Graph
          ref={this.graphComponent}
          start_blank={this.props.start_blank}
          edit={this.props.edit}
          closeSidebar={() => this.sidebarComponent.current.toggleSidebar("graph")}
          initialDrawMode="draw-node"
        />
        <Sidebar
          ref={this.sidebarComponent}
          graphs={this.state.graphs}
          getGraph={(name) => this.graphComponent.current.getGraph(name)}
        />
      </React.Fragment>
    )
  }
}

Container.propTypes = {
  start_blank: PropTypes.bool,
  edit: PropTypes.bool,
};

// // Set focus button onclicks
// $(".focus").click((event) => {
//   var id = $(event.target).attr("id");
//   var focusDetails = $("#" + id + "-details");
//   if (graphComponent.state.highlightedNodes == focusInfo[id + "FocusList"]) {
//     graphComponent.setState({ highlightedNodes: [] });
//     focusDetails.animate({ height: "2px" }, "fast");
//   } else {
//     $(".details").css("height", "2px");
//     focusDetails.animate({ height: "128px" }, "fast");
//     focusDetails.html(focusInfo[id + "Description"]);
//     graphComponent.setState({
//       highlightedNodes: focusInfo[id + "FocusList"]
//     });
//   }
// });
