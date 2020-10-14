import React from "react";
import Graph from "./Graph";
import Sidebar from "./Sidebar";
import PropTypes from "prop-types";
// import * as focusInfo from "./sidebar/focus_descriptions";
// import * as sidebarDivs from "./sidebar/sidebar_divs.js";

export default class Container extends React.Component {
  render() {
    return (
      <React.Fragment>
        <Graph start_blank={this.props.start_blank} edit={this.props.edit} initialDrawMode="draw-node" ref={instance => {this.graph = instance; }}/>
        <Sidebar reset={() => this.graph.reset()} />
      </React.Fragment>
    )
  }
}

Container.propTypes = {
  start_blank: PropTypes.bool,
  edit: PropTypes.bool,
};


// $("#reset").click(function() {
//   graphComponent.reset();
// });

// $(document).ready(function() {
//   $("#nav-export").click(function() {
//     graphComponent.openExportModal();
//   });
// });

// // Sends an ajax request to retrieve data for graph information
// $.ajax({
//   url: "graphs",
//   dataType: "json",
//   success: function(data) {
//     sidebarDivs.createGraphButtons(data);
//     $(".graph-button").click(function() {
//       var id = $(this).data("id");
//       var name = $(this).text();
//       graphComponent.getGraph(name);
//       sidebarDivs.changeFocusEnable(id);
//     });
//   },
//   error: function() {
//     throw "No graphs in database";
//   }
// });

// sidebarDivs.activateSidebar();

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
