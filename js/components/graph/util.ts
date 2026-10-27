import { parseAnd } from "./requirementParser"
import { GraphNodeJSON } from "./types"

/**
 * Helper function to populate `parentsObj` and `childrenObj` with the connections of a hybrid node.
 * A parent-child connection is added for each course that appears in `hybridNode`'s prereq string.
 * @param hybridNode
 * @param nodesJSON
 * @param parentsObj
 * @param childrenObj
 */
export function populateHybridRelatives(
  hybridNode: GraphNodeJSON,
  nodesJSON: GraphNodeJSON[],
  parentsObj: Record<string, string[]>,
  childrenObj: Record<string, string[]>
): void {
  // parse prereqs based on text
  let hybridText = hybridNode.text.map(textTag => textTag.text).join("")
  const nodeParents: string[] = []
  // First search for a node that matches the entire string (see Stats graph)
  let prereqNode = findRelationship(hybridText, nodesJSON)
  if (prereqNode !== undefined) {
    nodeParents.push(prereqNode.id_)
    childrenObj[prereqNode.id_].push(hybridNode.id_)
  } else {
    // Otherwise, parse the hybrid node's text as a prerequisite string of multiple courses,
    // and add a parent-child connection for each involved course
    const prereqs = parseAnd(hybridText)
    if (typeof prereqs === "object") {
      // Cast away the recursive `PrereqTree` type before flattening — TypeScript's
      // type-level recursion for `.flat(Infinity)` on a self-referential array type
      // blows up ("Type instantiation is excessively deep").
      const flatPrereqs = (prereqs as unknown[]).flat(Infinity) as string[]
      flatPrereqs.forEach(course => {
        prereqNode = findRelationship(course, nodesJSON)
        if (prereqNode !== undefined) {
          nodeParents.push(prereqNode.id_)
          childrenObj[prereqNode.id_].push(hybridNode.id_)
        } else {
          console.error("Could not find prereq for ", hybridText)
        }
      })
    } else {
      console.error("Could not find prereq for ", hybridText)
    }
  }
  parentsObj[hybridNode.id_] = nodeParents
}

/**
 * Helper for hybrid computation. Finds the node in `nodesJSON` that matches the given course.
 * @param course
 * @param nodesJSON
 * @return the matching node, or undefined if none is found.
 */
export function findRelationship(
  course: string,
  nodesJSON: GraphNodeJSON[]
): GraphNodeJSON | undefined {
  return nodesJSON.find(
    n => n.type_ === "Node" && n.text.some(textTag => textTag.text.includes(course))
  )
}

/**
 * Assuming the first section of `courseId` follows the format:
 * 3 letters for department, 3 numbers for course, and characters for credit type and campus.
 * e.g. csc108h1 or mat235y1mat237y1mat257y1
 */
export function numCredits(courseId: string): number {
  const creditType = courseId[6]
  return typeof creditType === "string" && creditType.toLowerCase() === "y" ? 1.0 : 0.5
}

/**
 * Transforms `x` and `y` with the given `transformation`.
 * Assumes that transformation is a valid list [a,b,c,d,e,f] of length 6 that represents the
 * transformation matrix [[a, c, e], [b, d, f], [0, 0, 1]]
 * @param transformation - a list that represents a matrix transformation
 * @param x - the x coordinate to be transformed
 * @param y - the y coordinate to be transformed
 */
export function transformPoint(
  transformation: number[],
  x: number,
  y: number
): [number, number] {
  const [a, b, c, d, e, f] = transformation
  const newX = a * x + c * y + e
  const newY = b * x + d * y + f
  return [newX, newY]
}

/**
 * Formats transformation to a valid CSS transform value.
 * Assumes that `transform` is a list of length 6 that represents a matrix transformation if it exists,
 * that is, [a, b, c, d, e, f] => matrix(a, b, c, d, e, f)
 */
export function formatTransform(transform: number[] | undefined): string {
  return transform ? `matrix(${transform.join(", ")})` : ""
}
