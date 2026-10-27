import { populateHybridRelatives } from "./util"
import { GraphNodeJSON } from "./types"

interface GraphLabelEntry {
  rId: string
}

interface GraphEdgeEntry {
  id_: string
  isRegion?: false
  source: string
  target: string
}

interface GraphRegionEntry {
  id_: string
  isRegion: true
}

type GraphPathEntry = GraphEdgeEntry | GraphRegionEntry

interface GraphRawData {
  texts: GraphLabelEntry[]
  shapes: (GraphNodeJSON & { type_: string })[]
  paths: GraphPathEntry[]
  width: number
  height: number
}

export interface GraphConnections {
  parents: Record<string, string[]>
  inEdges: Record<string, string[]>
  children: Record<string, string[]>
  outEdges: Record<string, string[]>
}

export interface GraphNodeStatus {
  status: string
  selected: boolean
}

export interface LoadedGraphData {
  labelsJSON: Record<string, GraphLabelEntry>
  regionsJSON: Record<string, GraphRegionEntry>
  nodesJSON: Record<string, GraphNodeJSON>
  hybridsJSON: Record<string, GraphNodeJSON>
  boolsJSON: Record<string, GraphNodeJSON>
  boolsStatus: Record<string, string>
  edgesJSON: Record<string, GraphEdgeEntry>
  width: number
  height: number
  zoomFactor: number
  horizontalPanFactor: number
  verticalPanFactor: number
  edgesStatus: Record<string, string>
  connections: GraphConnections
  nodesStatus: Record<string, GraphNodeStatus>
  selectedNodes: Set<string>
}

/**
 * Parses the raw graph JSON fetched from the server into the normalized shapes
 * (nodes, edges, bools, connections, and their statuses) that Graph keeps in state.
 * @param data the raw JSON fetched from `/get-json-data`
 * @return the parsed data, ready to be merged into Graph's state
 */
export function loadGraphData(data: GraphRawData): LoadedGraphData {
  const labelsJSON: Record<string, GraphLabelEntry> = {}
  const regionsJSON: Record<string, GraphRegionEntry> = {}
  const nodesJSON: Record<string, GraphNodeJSON> = {}
  const nodesStatus: Record<string, GraphNodeStatus> = {}
  const hybridsJSON: Record<string, GraphNodeJSON> = {}
  const boolsJSON: Record<string, GraphNodeJSON> = {}
  const boolsStatusObj: Record<string, string> = {}
  const edgesJSON: Record<string, GraphEdgeEntry> = {}
  const parentsObj: Record<string, string[]> = {}
  const inEdgesObj: Record<string, string[]> = {}
  const childrenObj: Record<string, string[]> = {}
  const outEdgesObj: Record<string, string[]> = {}
  const storedNodes: Set<string> = new Set()

  data.texts.forEach((entry: GraphLabelEntry) => {
    if (entry.rId.startsWith("tspan")) {
      labelsJSON[entry.rId] = entry
    }
  })

  data.shapes.forEach(function (entry: GraphNodeJSON & { type_: string }) {
    if (entry.type_ === "Node") {
      nodesJSON[entry.id_] = entry
    } else if (entry.type_ === "Hybrid") {
      hybridsJSON[entry.id_] = entry
    } else if (entry.type_ === "BoolNode") {
      boolsStatusObj[entry.id_] = localStorage.getItem(entry.id_) || "inactive"
      boolsJSON[entry.id_] = entry
    }
  })

  data.paths.forEach(function (entry: GraphPathEntry) {
    if (entry.isRegion) {
      regionsJSON[entry.id_] = entry
    } else {
      edgesJSON[entry.id_] = entry
    }
  })

  // The duplicate filter is a temporary fix, as previously there were two nodes, Hybrid and Node,
  // that were placed in the same spot on some graphs where there should be only a Hybrid node.
  const noDuplicatesNodesJSON: Record<string, GraphNodeJSON> = {}
  Object.values(nodesJSON).forEach((node: GraphNodeJSON) => {
    if (!(node.id_ in parentsObj)) {
      parentsObj[node.id_] = []
      inEdgesObj[node.id_] = []
      childrenObj[node.id_] = []
      outEdgesObj[node.id_] = []
      // Quickly adding any active nodes from local storage into the selected nodes
      if (localStorage.getItem(node.id_) === "active") {
        storedNodes.add(node.text[node.text.length - 1].text)
      }

      noDuplicatesNodesJSON[node.id_] = node
    }
  })

  Object.values(hybridsJSON).forEach((hybrid: GraphNodeJSON) => {
    childrenObj[hybrid.id_] = []
    outEdgesObj[hybrid.id_] = []
    const nodesList = Object.values(nodesJSON)
    populateHybridRelatives(hybrid, nodesList, parentsObj, childrenObj)
    let state = localStorage.getItem(hybrid.id_)
    if (state === null) {
      state = parentsObj[hybrid.id_].length === 0 ? "takeable" : "inactive"
    }
    nodesStatus[hybrid.id_] = {
      status: state,
      selected: ["active", "overridden"].indexOf(state) >= 0,
    }
  })

  Object.values(edgesJSON).forEach((edge: GraphEdgeEntry) => {
    if (edge.target in parentsObj && edge.target in inEdgesObj) {
      parentsObj[edge.target].push(edge.source)
      inEdgesObj[edge.target].push(edge.id_)
    }

    if (edge.source in childrenObj && edge.source in outEdgesObj) {
      childrenObj[edge.source].push(edge.target)
      outEdgesObj[edge.source].push(edge.id_)
    }
  })

  Object.keys(boolsJSON).forEach((boolId: string) => {
    const parents: string[] = []
    const childs: string[] = []
    const outEdges: string[] = []
    const inEdges: string[] = []
    Object.values(edgesJSON).forEach((edge: GraphEdgeEntry) => {
      if (boolId === edge.target) {
        parents.push(edge.source)
        inEdges.push(edge.id_)
      } else if (boolId === edge.source) {
        childs.push(edge.target)
        outEdges.push(edge.id_)
      }
    })
    parentsObj[boolId] = parents
    childrenObj[boolId] = childs
    outEdgesObj[boolId] = outEdges
    inEdgesObj[boolId] = inEdges
  })

  Object.keys(noDuplicatesNodesJSON).forEach((nodeId: string) => {
    let state = localStorage.getItem(nodeId)
    if (state === null) {
      state = parentsObj[nodeId].length === 0 ? "takeable" : "inactive"
    }
    nodesStatus[nodeId] = {
      status: state,
      selected: ["active", "overridden"].indexOf(state) >= 0,
    }
  })

  const edgesStatus: Record<string, string> = Object.values(edgesJSON).reduce(
    (acc: Record<string, string>, curr: GraphEdgeEntry) => {
      const source = curr.source
      const target = curr.target
      let status: string
      const isSourceSelected =
        nodesStatus[source]?.selected || boolsStatusObj[source] === "active"

      const isTargetSelected =
        nodesStatus[target]?.selected || boolsStatusObj[target] === "active"

      const targetStatus = nodesStatus[target]?.status || boolsStatusObj[target]

      if (!isSourceSelected && targetStatus === "missing") {
        status = "missing"
      } else if (!isSourceSelected) {
        status = "inactive"
      } else if (!isTargetSelected) {
        status = "takeable"
      } else {
        status = "active"
      }
      acc[curr.id_] = status
      return acc
    },
    {} as Record<string, string>
  )

  return {
    labelsJSON,
    regionsJSON,
    nodesJSON: noDuplicatesNodesJSON,
    hybridsJSON,
    boolsJSON,
    boolsStatus: boolsStatusObj,
    edgesJSON,
    width: data.width,
    height: data.height,
    zoomFactor: 1,
    horizontalPanFactor: 0,
    verticalPanFactor: 0,
    edgesStatus,
    connections: {
      parents: parentsObj,
      inEdges: inEdgesObj,
      children: childrenObj,
      outEdges: outEdgesObj,
    },
    nodesStatus,
    selectedNodes: storedNodes,
  }
}
