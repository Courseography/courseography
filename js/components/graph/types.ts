/** A single line of text rendered inside a node/bool shape. */
export interface GraphTextTag {
  pos: [number, number]
  text: string
}

/** The SVG shape data for a graph node or boolean (and/or) node. */
export interface GraphNodeJSON {
  id_?: string
  pos: [number, number]
  width: number
  height: number
  fill: string
  text: GraphTextTag[]
}
