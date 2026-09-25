import React from "react"
import { GraphNodeJSON } from "./types"

interface BoolProps {
  className?: string
  JSON: GraphNodeJSON
  inEdges?: string[]
  logicalType?: string
  outEdges?: string[]
  parents: string[]
  status?: string
}

/** Function representing a boolean node (and/or) */
export default function Bool(props: BoolProps) {
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
          y: ellipseAttrs.cy,
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
