import PropTypes from "prop-types"
import React from "react"
import { refLookUp } from "../common/utils"

/**
 * Class representing an edge from a Node/Bool to a Node/Bool
 */
export default class Edge extends React.Component {
  constructor(props) {
    super(props)
  }

  updateStatus(status) {
    const sourceNode = refLookUp(this.props.source, this.props.svg)
    const targetNode = refLookUp(this.props.target, this.props.svg)
    if (sourceNode === undefined || targetNode === undefined) {
      return
    }
    if (!status) {
      if (
        !sourceNode.isSelected() &&
        ((targetNode.props.className === "bool" &&
          targetNode.props.status === "missing") ||
          (targetNode.props.className !== "bool" &&
            targetNode.state.status === "missing"))
      ) {
        status = "missing"
      } else if (!sourceNode.isSelected()) {
        status = "inactive"
      } else if (!targetNode.isSelected()) {
        status = "takeable"
      } else {
        status = "active"
      }
    }
    this.props.updateEdgeStatus(status, this.props.edgeID)
  }
  render() {
    var pathAttrs = { d: "M" }
    this.props.points.forEach(p => {
      pathAttrs.d += p[0] + "," + p[1] + " "
    })

    return (
      <path
        {...pathAttrs}
        className={this.props.className + " " + this.props.status}
        data-testid={`${this.props.source}->${this.props.target}`}
        markerEnd="url(#arrowHead)"
      />
    )
  }
}

Edge.propTypes = {
  className: PropTypes.string,
  edgeID: PropTypes.string,
  /** Array of points for the edge. A straight edge will have 2. Each turn in the edge means another point*/
  points: PropTypes.array,
  /** Node from which the edge is drawn*/
  source: PropTypes.string,
  /** The overarching graph object */
  svg: PropTypes.object,
  /** Node that the edge is pointing to */
  target: PropTypes.string,
  /** function called when the edge's state has changed */
  status: PropTypes.string,
  updateEdgeStatus: PropTypes.func,
}
