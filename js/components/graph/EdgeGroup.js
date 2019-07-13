import PropTypes from "prop-types";
import React from "react";
import Edge from "./Edge";

export default class EdgeGroup extends React.Component {
  constructor(props) {
    super(props);
    // EdgeGroup's state is used to keep track of the edgeIDs of
    // edges that are missing. Void is just a placeholder state so
    // we can declare an initial state; it does nothing.
    this.state = {};
    this.updateEdgeStatus = this.updateEdgeStatus.bind(this);
    this.reset = this.reset.bind(this);
    this.generateEdge = this.generateEdge.bind(this);
    this.setRefEntry = this.setRefEntry.bind(this);
  }

  // When an edge's state changes and the edge is not undefined,
  // it will call updateEdgeStatus and update EdgeGroup's state with its
  // edgeID and status. This function is passed as a props to Edge.
  updateEdgeStatus(edgeID, state) {
    var isMissing = state === "missing";
    this.setState({ [edgeID]: isMissing });
  }

  componentDidUpdate() {
    this.props.edgesJSON.forEach(edgeJSON => {
      this[edgeJSON.id_].updateStatus();
    });
  }

  reset() {
    this.props.edgesJSON.forEach(edgeJSON => {
      this[edgeJSON.id_].setState({ status: "inactive" });
    });
  }

  setRefEntry(edgeJSON) {
    return function(elem) {
      return elem && (this[edgeJSON.id_] = elem);
    }
  }

  // Generate data for an Edge component
  generateEdge(edgeJSON) {
    return (
      <Edge
        className="path"
        key={edgeJSON.id_}
        ref={this.setRefEntry(edgeJSON)}
        source={edgeJSON.source}
        target={edgeJSON.target}
        points={edgeJSON.points}
        svg={this.props.svg}
        edgeID={edgeJSON.id_}
        updateEdgeStatus={this.updateEdgeStatus}
      />
    );
  }

  render() {
    // Missing edges must be rendered last. The sort
    // method custom sorts a copy of edgesJSON so that all missing edges
    // are last in the list. Then render based on that list.
    var edges = this.props.edgesJSON;
    var edgesCopy = [...edges];
    var state = this.state;
    edgesCopy.sort((a, b) => {
      // If an edge is missing, its edgeID should be in EdgeGroup's
      // state and its value should be true.
      var aID = a.id_;
      var bID = b.id_;
      var aMiss = false;
      var bMiss = false;
      aMiss = aID in state && state[aID];
      bMiss = bID in state && state[bID];
      if ((aMiss && bMiss) || (!aMiss && !bMiss)) {
        // a and b are equal
        return 0;
      } else if (aMiss && !bMiss) {
        // sort a after b
        return 1;
      } else if (!aMiss && bMiss) {
        // sort b after a
        return -1;
      }
    });
    return <g id="edges">{edgesCopy.map(this.generateEdge)}</g>;
  }
}

EdgeGroup.propTypes = {
  edgesJSON: PropTypes.array,
  svg: PropTypes.object
};
