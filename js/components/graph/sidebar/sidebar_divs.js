var toggled = false;

export function activateSidebar() {
  "use strict";

  // $("#sidebar-button").click(function() {
  //   toggleSidebar("button");
  // });

  $("#focuses-nav").click(function(e) {
    e.preventDefault();
    resetDivs();
    $("#focuses").show();
    $("#focuses-nav").addClass("active");
  });
}

/**
 * Hides all currently open divs and resets navbar to display none of the links as clicked.
 **/
function resetDivs() {
  "use strict";

  $("#focuses").hide();
  $("#graphs").hide();
  $("#graphs-nav, #focuses-nav").removeClass("active");
}

/**
 * Opens and closes the sidebar.
 * @param{string} location The location where you are clicking (either the sidebar button or the graph).
 **/
function toggleSidebar(location) {
  "use strict";

  if (toggled) {
    toggled = false;
    resetDivs();
    $("#sidebar").animate({ width: "40px" }, "fast", undefined, function() {
      $("#sidebar-icon").removeClass("flip");
    });
    $("#reset").hide();
    $("#fcecount").hide();
  } else if (!toggled && location === "button") {
    toggled = true;
    $("#sidebar").animate({ width: "400px" }, "fast", undefined, function() {
      $("#sidebar-icon").addClass("flip");
    });

    $("#graphs").show();
    $("#graphs-nav").addClass("active");

    $("#reset").show();
    $("#fcecount").show();
  }
}

/**
 * Dynamically creates buttons for each graph in the sidebar.
 */
export function createGraphButtons(graphs) {
  "use strict";

  for (var i = 0; i < graphs.length; i++) {
    var graphId = graphs[i].id;
    var graphTitle = graphs[i].title;
    var graphButton =
      '<div id = "graph-' + graphId + '" class = "graph-button">';
    $("#graphs").append(graphButton);
    $("#graph-" + graphId).html(graphTitle);
    $("#graph-" + graphId).data("id", graphs[i].id);
  }

  $("#graphs-nav").click(function(e) {
    e.preventDefault();
    resetDivs();
    $("#graphs").show();
    $("#graphs-nav").addClass("active");
  });

  $("#react-graph").click(function(e) {
    e.preventDefault();
    toggleSidebar("graph");
  });
}

/**
 * Enables the Focuses nav in the sidebar if the CS graph is selected.
 * @param:{string} id ID of the graph we just selected
 **/
export function changeFocusEnable(id) {
  var graph = $("#graph-" + id)[0];
  if (graph !== undefined && graph.innerHTML === "Computer Science") {
    $("#focuses-nav").removeClass("disabled");
  } else {
    $("#focuses-nav").addClass("disabled");
  }
}
