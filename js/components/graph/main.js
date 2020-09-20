import React from "react";
import ReactDOM from "react-dom";
import Graph from "./Graph";
import Container from "./Container";
import * as focusInfo from "./sidebar/focus_descriptions";
import * as sidebarDivs from "./sidebar/sidebar_divs.js";

// The "main"
document.addEventListener("DOMContentLoaded", () => {
  ReactDOM.render(<Container />, document.getElementById("container"));
  const graphComponent = renderReactGraph("react-graph");

  $("#reset").click(function() {
    graphComponent.reset();
  });

  $(document).ready(function() {
    $("#nav-export").click(function() {
      graphComponent.openExportModal();
    });
  });

  // Sends an ajax request to retrieve data for graph information
  $.ajax({
    url: "graphs",
    dataType: "json",
    success: function(data) {
      sidebarDivs.createGraphButtons(data);
      $(".graph-button").click(function() {
        var id = $(this).data("id");
        var name = $(this).text();
        graphComponent.getGraph(name);
        sidebarDivs.changeFocusEnable(id);
      });
    },
    error: function() {
      throw "No graphs in database";
    }
  });

  sidebarDivs.activateSidebar();

  // Set focus button onclicks
  $(".focus").click((event) => {
    var id = $(event.target).attr("id");
    var focusDetails = $("#" + id + "-details");
    if (graphComponent.state.highlightedNodes == focusInfo[id + "FocusList"]) {
      graphComponent.setState({ highlightedNodes: [] });
      focusDetails.animate({ height: "2px" }, "fast");
    } else {
      $(".details").css("height", "2px");
      focusDetails.animate({ height: "128px" }, "fast");
      focusDetails.html(focusInfo[id + "Description"]);
      graphComponent.setState({
        highlightedNodes: focusInfo[id + "FocusList"]
      });
    }
  });
});

export function renderReactGraph(graph_container_id, start_blank, edit) {
  if (start_blank === undefined) {
    start_blank = false;
  }

  // If edit is NOT undefined, then the user is on the draw page
  if (edit === undefined) {
    edit = false;
  }

  return ReactDOM.render(
    <Graph start_blank={start_blank} edit={edit} initialDrawMode="draw-node" />,
    document.getElementById(graph_container_id)
  );
}
