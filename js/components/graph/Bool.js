import React from "react"
import PropTypes from "prop-types"

/** Function representing a boolean node (and/or) */
export default function Bool(props) {
  const ellipseAttrs = {
    cx: props.JSON.pos[0],
    cy: props.JSON.pos[1],
    rx: "9.8800001",
    ry: "7.3684001",
  }
  return (
    <g
      {...props.JSON}
      className={props.className + " " + props.status}
      data-testid={`and(${props.parents.join()})`}
    >
      <ellipse {...ellipseAttrs} />
      {props.JSON.text.map(function (textTag, i) {
        const textAttrs = {
          x: ellipseAttrs.cx,
          y: textTag.pos[1],
        }
        return (
          <text {...textAttrs} key={i}>
            {props.logicalType}
          </text>
        )
      })}
    </g>
  )
}

Bool.propTypes = {
  className: PropTypes.string,
  JSON: PropTypes.object,
  inEdges: PropTypes.array,
  logicalType: PropTypes.string,
  outEdges: PropTypes.array,
  parents: PropTypes.array,
  status: PropTypes.string,
}
