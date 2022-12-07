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
export default class Node extends React.Component {
  getDataTestId = () => {
    if (this.props.hybrid) {
      return `h(${this.props.parents.join(",")})`
    }
    return this.props.JSON.id_
  }

  render() {
    let ellipse = null
    const newClassName = this.props.className + " " + this.props.status
    if (this.props.highlighted) {
      const attrs = this.props.JSON
      const width = parseFloat(attrs.width) / 2
      const height = parseFloat(attrs.height) / 2
      ellipse = (
        <ellipse
          className="spotlight"
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
      onKeyDown: this.props.svg.onKeyDown,
      onWheel: this.props.svg.onWheel,
      onMouseEnter: this.props.onMouseEnter,
      onMouseLeave: this.props.onMouseLeave,
      onClick: this.props.onClick,
    }

    const rectAttrs = {
      height: this.props.JSON.height,
      width: this.props.JSON.width,
      x: this.props.JSON.pos[0],
      y: this.props.JSON.pos[1],
    }

    if (this.props.className === "node") {
      rectAttrs["rx"] = "8"
      rectAttrs["ry"] = "8"
    }

    const rectStyle = {
      fill: this.props.JSON.fill,
    }

    const textXOffset = this.props.JSON.pos[0] + this.props.JSON.width / 2

    // TODO: Look at this.props to see what we need to give the g
    return (
      <g
        {...gAttrs}
        id={this.props.JSON.id_}
        className={newClassName}
        data-testid={this.getDataTestId()}
      >
        {ellipse}
        <rect
          {...rectAttrs}
          style={rectStyle}
          filter={
            this.props.className === "hybrid"
              ? ""
              : `url(#${this.props.nodeDropshadowFilter})`
          }
        />
        {this.props.JSON.text.map(function (textTag, i) {
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
}

Node.propTypes = {
  className: PropTypes.string,
  editMode: PropTypes.bool,
  highlighted: PropTypes.bool,
  hybrid: PropTypes.bool,
  JSON: PropTypes.object,
  onClick: PropTypes.func,
  onMouseEnter: PropTypes.func,
  onMouseLeave: PropTypes.func,
  status: PropTypes.string,
  parents: PropTypes.array,
  svg: PropTypes.object,
  nodeDropshadowFilter: PropTypes.string,
}
