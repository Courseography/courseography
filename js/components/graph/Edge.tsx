import React from "react"

interface EdgeProps {
  className?: string
  /** Array of points for the edge. A straight edge will have 2. Each turn in the edge means another point*/
  points: [number, number][]
  /** Node from which the edge is drawn*/
  source?: string
  /** Node that the edge is pointing to */
  target?: string
  /** Status of this edge */
  status?: string
  /** Transform of this edge */
  transform?: string
}

/**
 * Function-based component representing an edge from a Node/Bool to a Node/Bool
 */
export default function Edge(props: EdgeProps) {
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
