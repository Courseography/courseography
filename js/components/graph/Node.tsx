import React from "react"
import { GraphNodeJSON } from "./types"

interface NodeProps {
  className: string
  editMode?: boolean
  focused?: boolean
  highlightDeps?: boolean
  highlightFocus?: boolean
  hybrid: boolean
  JSON: GraphNodeJSON & { id_: string }
  onClick?: () => void
  onMouseEnter?: () => void
  onMouseLeave?: () => void
  onWheel?: React.WheelEventHandler
  onKeyDown?: React.KeyboardEventHandler
  status?: string
  transform?: string
  parents: string[]
  nodeDropshadowFilter?: string
}

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
export default function Node(props: NodeProps) {
  const getDataTestId = () => {
    if (props.hybrid) {
      return `h(${props.parents.join(",")})`
    }
    return props.JSON.id_
  }

  let ellipse = null
  const newClassName = props.className + " " + props.status
  if (props.highlightFocus || props.highlightDeps) {
    const attrs = props.JSON
    const width = attrs.width / 2
    const height = attrs.height / 2
    const isCombo = props.JSON.id_.length > 8
    ellipse = (
      <ellipse
        className={props.highlightDeps ? "spotlight" : "spotlight-focus"}
        cx={attrs.pos[0] + width}
        cy={attrs.pos[1] + height}
        rx={isCombo ? width + 18 : width + 9}
        ry={isCombo ? height + 17 : height + 8.5}
        filter="url(#blur-filter)"
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
  } as const

  const rectAttrs: {
    height: number
    width: number
    x: number
    y: number
    rx?: string
    ry?: string
  } = {
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
  const textYOffset = props.JSON.pos[1] + props.JSON.height / 2
  const singleLine = props.className === "node" && props.JSON.text.length === 1

  return (
    <g
      {...gAttrs}
      id={props.JSON.id_}
      className={newClassName}
      data-testid={getDataTestId()}
      transform={props.transform}
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
          y: singleLine ? textYOffset : textTag.pos[1],
          dominantBaseline: singleLine ? ("central" as const) : undefined,
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
