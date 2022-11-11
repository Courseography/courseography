import PropTypes from "prop-types"
import React from "react"

/**
 * Class representing an edge from a Node/Bool to a Node/Bool
 */
export default class Edge extends React.Component {
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
  /** Status of this edge */
  status: PropTypes.string,
}
