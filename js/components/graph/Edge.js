import PropTypes from "prop-types"
import React from "react"

/**
 * Function-based component representing an edge from a Node/Bool to a Node/Bool
 */
export default function Edge(props) {
  let pathDescription = "M"
  props.points.forEach(p => {
    pathDescription += p[0] + "," + p[1] + " "
  })

  return (
    <path
      d={pathDescription}
      transform={props.transform}
      className={props.className + " " + props.status}
      data-testid={`${props.source}->${props.target}`}
      markerEnd="url(#arrowHead)"
    />
  )
}

Edge.propTypes = {
  className: PropTypes.string,
  /** Array of points for the edge. A straight edge will have 2. Each turn in the edge means another point*/
  points: PropTypes.array,
  /** Node from which the edge is drawn*/
  source: PropTypes.string,
  /** Node that the edge is pointing to */
  target: PropTypes.string,
  /** Status of this edge */
  status: PropTypes.string,
  /** Transform of this edge */
  transform: PropTypes.string,
}
