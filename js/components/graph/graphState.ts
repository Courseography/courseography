import { parseAnd, PrereqTree } from "./requirementParser"
import { findRelationship } from "./util"
import { LoadedGraphData } from "./loadGraphData"

/** The parts of Graph's state that describe the graph's nodes, bools, and their statuses. */
export type GraphState = Pick<
  LoadedGraphData,
  | "nodesJSON"
  | "nodesStatus"
  | "hybridsJSON"
  | "boolsJSON"
  | "boolsStatus"
  | "connections"
>

/**
 * Checks if a node or bool node is currently selected or active.
 * @param state
 * @param nodeId
 * @return whether the node or bool is selected
 */
export function isSelected(state: GraphState, nodeId: string): boolean {
  if (state.nodesStatus[nodeId]) {
    return isSelectedNode(state, nodeId)
  } else {
    return isSelectedBool(state, nodeId)
  }
}

/**
 * Checks whether a node is selected. Course nodes are selected by the user, while
 * hybrid nodes count as selected when they are active.
 * @param state
 * @param nodeId
 * @return whether the node is selected
 */
export function isSelectedNode(state: GraphState, nodeId: string): boolean {
  if (state.nodesJSON[nodeId]) {
    return state.nodesStatus[nodeId].selected
  } else {
    return state.nodesStatus[nodeId].status === "active"
  }
}

/**
 * Checks whether a bool is selected.
 * @param state
 * @param boolId
 * @return whether the bool's status is active
 */
export function isSelectedBool(state: GraphState, boolId: string): boolean {
  return state.boolsStatus[boolId] === "active"
}

/**
 * Checks if the prerequisite courses have been satisfied based on bool type.
 * @param state
 * @param boolId the ID of the bool node.
 * @return whether the bool is satisfied based on its prerequisite nodes.
 */
export function arePrereqsSatisfiedBool(state: GraphState, boolId: string): boolean {
  const parents = state.connections.parents[boolId]
  const logicalType = state.boolsJSON[boolId].text[0].text
  if (logicalType === "and") {
    return parents.every(parent => isSelected(state, parent))
  } else if (logicalType === "or") {
    return parents.some(parent => isSelected(state, parent))
  }
  return false
}

/**
 * Checks whether all prerequisite/preceding nodes for a node are satisfied.
 * @param state
 * @param nodeId the ID of the node.
 * @return whether all of the node's prerequisite nodes are satisfied by the selected courses.
 */
export function arePrereqsSatisfiedNode(state: GraphState, nodeId: string): boolean {
  return state.connections.parents[nodeId].every(parent => {
    if (state.nodesStatus[parent] || state.boolsJSON[parent]) {
      return isSelected(state, parent)
    } else {
      return false
    }
  })
}

/**
 * Checks whether a hybrid node's prereq string is satisfied.
 * @param state
 * @param nodeId the ID of the hybrid node.
 * @return whether the node's text prerequisite string is satisfied by the selected courses.
 */
export function arePrereqsSatisfiedHybrid(state: GraphState, nodeId: string): boolean {
  // Concatenate prereq string
  const hybridNode = state.hybridsJSON[nodeId]
  const hybridText = hybridNode.text.map(textTag => textTag.text).join("")

  // Parse prereq string into a nested list alternating between AND and OR conditions
  const prereqList = parseAnd(hybridText)

  // Recursively check if each prerequisite condition is satisfied within the selected courses
  const nodesList = Object.values(state.nodesJSON)
  const isSelectedCourse = (course: string): boolean => {
    const prereqNode = findRelationship(course, nodesList)
    if (prereqNode !== undefined) {
      return isSelectedNode(state, prereqNode.id_)
    } else {
      return false
    }
  }
  const andSatisfied = (andList: PrereqTree): boolean => {
    if (typeof andList === "string") {
      return isSelectedCourse(andList)
    } else {
      return andList.every(orSatisfied)
    }
  }
  const orSatisfied = (orList: PrereqTree): boolean => {
    if (typeof orList === "string") {
      return isSelectedCourse(orList)
    } else {
      return orList.some(andSatisfied)
    }
  }
  return andSatisfied(prereqList)
}
