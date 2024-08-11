import PropTypes from "prop-types"
import React from "react"

/** React component class representing a Node on the graph
 *
 * Status and Selected Props:
 *  - status: holds the current status message (see below)
 *  - selected: whether the node is selected, based on the status message
 *
 *  Unselected status messages:
 *    - takeable: all prerequisites are satisfied (or no prereqs/parents)
 *    - inactive: missing some prerequisites
 *
 *  Selected status messages:
 *    - active: all prerequisities are satisfied
 *    - overridden: missing some prerequisities (will have a red border)
 *
 *  On Hover status message:
 *    - missing: means that this node is a prerequisite node that is not satisfied  (red border)
 *
 * Types of nodes:
 *  - Course nodes are nodes that represent a certain course
 *  - Hybrid nodes are the smaller, grey nodes on the graph that represent another course node
 *    farther away. They can only be either 'active' or 'inactive'
 */
export default function Node(props) {
  const getDataTestId = () => {
    if (props.hybrid) {
      return `h(${props.parents.join(",")})`
    }
    return props.JSON.id_
  }

  let ellipse = null
  const newClassName = props.className + " " + props.status
  if (props.highlightFoc || props.highlightDep) {
    const attrs = props.JSON
    const width = parseFloat(attrs.width) / 2
    const height = parseFloat(attrs.height) / 2
    ellipse = (
      <ellipse
        className={props.highlightDep ? "spotlight" : "spotlight-focus"}
        cx={parseFloat(attrs.pos[0]) + width}
        cy={parseFloat(attrs.pos[1]) + height}
        rx={width + 9}
        ry={height + 8.5}
      />
    )
  }

  const gAttrs = {
    textRendering: "geometricPrecision",
    shapeRendering: "geometricPrecision",
    onKeyDown: props.onKeyDown,
    onWheel: props.onWheel,
    onMouseEnter: props.onMouseEnter,
    onMouseLeave: props.onMouseLeave,
    onClick: props.onClick,
  }

  const rectAttrs = {
    height: props.JSON.height,
    width: props.JSON.width,
    x: props.JSON.pos[0],
    y: props.JSON.pos[1],
  }

  if (props.className === "node") {
    rectAttrs["rx"] = "8"
    rectAttrs["ry"] = "8"
  }

  const rectStyle = {
    fill: props.JSON.fill,
  }

  const textXOffset = props.JSON.pos[0] + props.JSON.width / 2

  return (
    <g
      {...gAttrs}
      id={props.JSON.id_}
      className={newClassName}
      data-testid={getDataTestId()}
    >
      {ellipse}
      <rect
        {...rectAttrs}
        style={rectStyle}
        filter={
          props.className === "hybrid" ? "" : `url(#${props.nodeDropshadowFilter})`
        }
      />
      {props.JSON.text.map(function (textTag, i) {
        const textAttrs = {
          x: textXOffset,
          y: textTag.pos[1],
        }
        return (
          <text {...textAttrs} key={i}>
            {textTag.text}
          </text>
        )
      })}
    </g>
  )
}

Node.propTypes = {
  className: PropTypes.string,
  editMode: PropTypes.bool,
  focused: PropTypes.bool,
  highlightDep: PropTypes.bool,
  highlightFoc: PropTypes.bool,
  hybrid: PropTypes.bool,
  JSON: PropTypes.object,
  onClick: PropTypes.func,
  onMouseEnter: PropTypes.func,
  onMouseLeave: PropTypes.func,
  onWheel: PropTypes.func,
  onKeyDown: PropTypes.func,
  status: PropTypes.string,
  parents: PropTypes.array,
  nodeDropshadowFilter: PropTypes.string,
}
