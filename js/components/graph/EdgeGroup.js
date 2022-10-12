import PropTypes from "prop-types"
import React from "react"
import Edge from "./Edge"
/**
 * Class representing a group of all Edges from the graph
 */
export default class EdgeGroup extends React.Component {
  constructor(props) {
    super(props)
    // EdgeGroup's state is used to keep track of the edgeIDs of
    // edges that are missing. Void is just a placeholder state so
    // we can declare an initial state; it does nothing.
    this.state = {}
  }

  /**
   * Updates EdgeGroup's state with the state of an Edge when the Edge's state changes.
   * This function is passed as a property to Edge.
   * @param {string} edgeID
   * @param {string} state
   */
  updateEdgeStatus = (edgeID, state) => {
    var isMissing = state === "missing"
    this.props.updateEdgeGroupStatus(edgeID, isMissing)
  }

  /**
   * After each render after the initial, update the status of each Edge
   */
  componentDidUpdate() {
    this.props.edgesJSON.forEach(edgeJSON => {
      this[edgeJSON.id_].updateStatus()
    })
  }

  /**
   * Set all Edges to the inactive status
   */
  reset = () => {
    this.props.edgesJSON.forEach(edgeJSON => {
      this[edgeJSON.id_].setState({ status: "inactive" })
    })
  }
  /**
   * This function is used as a callback ref. See {@link:https://reactjs.org/docs/refs-and-the-dom.html#callback-refs}
   * @param {JSON} edgeJSON Represents a single edge
   * @returns {function} A function that adds/updates a key value pair to EdgeGroup of an Edge's ID
   *  and the Edge object as long as the Edge exists
   */
  setRefEntry = edgeJSON => {
    return elem => elem && (this[edgeJSON.id_] = elem)
  }

  /**
   * Generate React Component representation of edge
   * @param {JSON} edgeJSON Represents a single edge
   * @returns {Edge} The React Component representing the edge
   */
  generateEdge = edgeJSON => {
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
    )
  }

  render() {
    // Missing edges must be rendered last. The sort
    // method custom sorts a copy of edgesJSON so that all missing edges
    // are last in the list. Then render based on that list.
    var edges = this.props.edgesJSON
    var edgesCopy = [...edges]
    var state = this.state
    edgesCopy.sort((a, b) => {
      // If an edge is missing, its edgeID should be in EdgeGroup's
      // state and its value should be true.
      var aID = a.id_
      var bID = b.id_
      var aMiss = false
      var bMiss = false
      aMiss = aID in state && state[aID]
      bMiss = bID in state && state[bID]
      if ((aMiss && bMiss) || (!aMiss && !bMiss)) {
        // a and b are equal
        return 0
      } else if (aMiss && !bMiss) {
        // sort a after b
        return 1
      } else if (!aMiss && bMiss) {
        // sort b after a
        return -1
      }
    })
    return <g id="edges">{edgesCopy.map(this.generateEdge)}</g>
  }
}

EdgeGroup.propTypes = {
  /**Array of all edges in the graph */
  edgesJSON: PropTypes.array,
  /**The overarching graph object */
  svg: PropTypes.object,
  edgeGroupStatus: PropTypes.object,
  updateEdgeGroupStatus: PropTypes.func,
}
