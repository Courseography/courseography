import PropTypes from "prop-types"
import React from "react"
import { refLookUp } from "../common/utils"

/**
 * Class representing an edge from a Node/Bool to a Node/Bool
 */
export default class Edge extends React.Component {
  constructor(props) {
    super(props)
    this.state = { status: "inactive" }
  }

  /**
   * Update the status of the Edge, based on the status of the Node/Bool it points from/to
   */
  updateStatus = () => {
    var source = refLookUp(this.props.source, this.props.svg)
    var target = refLookUp(this.props.target, this.props.svg)
    if (source === undefined || target === undefined) {
      return
    }

    if (
      !source.isSelected() &&
      ((target.props.className === "bool" && target.props.status === "missing") ||
        (target.props.className !== "bool" && target.state.status === "missing"))
    ) {
      this.setState({ status: "missing" })
    } else if (!source.isSelected()) {
      this.setState({ status: "inactive" })
    } else if (!target.isSelected()) {
      this.setState({ status: "takeable" })
    } else {
      this.setState({ status: "active" })
    }
  }
  /**
   *
    After each render beyond the initial, check if the edge's state has changed. If so,
    notify the state of EdgeGroup with updateEdgeStatus.
   * @param {*} prevProps
   * @param {Object} prevState The state of this object from the previous render
   */
  componentDidUpdate(prevProps, prevState) {
    if (this.state.status !== prevState.status) {
      this.props.updateEdgeStatus(this.props.edgeID, this.state.status)
    }
  }

  render() {
    var pathAttrs = { d: "M" }
    this.props.points.forEach(p => {
      pathAttrs.d += p[0] + "," + p[1] + " "
    })

    return (
      <path
        {...pathAttrs}
        className={this.props.className + " " + this.state.status}
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
  updateEdgeStatus: PropTypes.func,
}
