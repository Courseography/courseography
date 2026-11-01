import {
  isSelected,
  isSelectedNode,
  isSelectedBool,
  arePrereqsSatisfiedBool,
  arePrereqsSatisfiedNode,
  arePrereqsSatisfiedHybrid,
} from "../graphState"

const node = (id, text, type_ = "Node") => ({
  id_: id,
  type_,
  pos: [0, 0],
  width: 40,
  height: 32,
  fill: "",
  text: [{ pos: [0, 0], text }],
})

/**
 * A small graph: CSC108 (selected) and CSC148/CSC165 (unselected) course nodes,
 * "and"/"or" bools over them, and hybrid nodes with AND, OR, and unknown prereq strings.
 */
function buildState() {
  return {
    nodesJSON: {
      csc108: node("csc108", "CSC108"),
      csc148: node("csc148", "CSC148"),
      csc165: node("csc165", "CSC165"),
      csc236: node("csc236", "CSC236"),
    },
    nodesStatus: {
      csc108: { status: "active", selected: true },
      csc148: { status: "takeable", selected: false },
      csc165: { status: "takeable", selected: false },
      csc236: { status: "inactive", selected: false },
      hAnd: { status: "inactive", selected: false },
      hOr: { status: "active", selected: false },
      hMissing: { status: "inactive", selected: false },
    },
    hybridsJSON: {
      hAnd: node("hAnd", "CSC108, CSC148", "Hybrid"),
      hOr: node("hOr", "CSC108/CSC165", "Hybrid"),
      hMissing: node("hMissing", "MAT999", "Hybrid"),
    },
    boolsJSON: {
      andBool: node("andBool", "and", "BoolNode"),
      orBool: node("orBool", "or", "BoolNode"),
      otherBool: node("otherBool", "xor", "BoolNode"),
    },
    boolsStatus: {
      andBool: "inactive",
      orBool: "active",
      otherBool: "inactive",
    },
    connections: {
      parents: {
        csc148: ["csc108"],
        csc165: ["csc108", "unknown"],
        csc236: ["orBool"],
        andBool: ["csc108", "csc148"],
        orBool: ["csc108", "csc165"],
        otherBool: ["csc108"],
      },
      inEdges: {},
      children: {},
      outEdges: {},
    },
  }
}

describe("isSelected", () => {
  it("Should use the selected flag for course nodes", () => {
    const state = buildState()
    expect(isSelected(state, "csc108")).toBe(true)
    expect(isSelected(state, "csc148")).toBe(false)
  })

  it("Should use the bool status for bools", () => {
    const state = buildState()
    expect(isSelected(state, "orBool")).toBe(true)
    expect(isSelected(state, "andBool")).toBe(false)
  })
})

describe("isSelectedNode", () => {
  it("Should treat active hybrid nodes as selected", () => {
    const state = buildState()
    expect(isSelectedNode(state, "hOr")).toBe(true)
    expect(isSelectedNode(state, "hAnd")).toBe(false)
  })
})

describe("isSelectedBool", () => {
  it("Should be true only for active bools", () => {
    const state = buildState()
    expect(isSelectedBool(state, "orBool")).toBe(true)
    expect(isSelectedBool(state, "andBool")).toBe(false)
  })
})

describe("arePrereqsSatisfiedBool", () => {
  it("Should require every parent to be selected for an 'and' bool", () => {
    const state = buildState()
    expect(arePrereqsSatisfiedBool(state, "andBool")).toBe(false)
    state.nodesStatus.csc148.selected = true
    expect(arePrereqsSatisfiedBool(state, "andBool")).toBe(true)
  })

  it("Should require some parent to be selected for an 'or' bool", () => {
    const state = buildState()
    expect(arePrereqsSatisfiedBool(state, "orBool")).toBe(true)
    state.nodesStatus.csc108.selected = false
    expect(arePrereqsSatisfiedBool(state, "orBool")).toBe(false)
  })

  it("Should be false for an unrecognized logical type", () => {
    expect(arePrereqsSatisfiedBool(buildState(), "otherBool")).toBe(false)
  })
})

describe("arePrereqsSatisfiedNode", () => {
  it("Should be true when all parent nodes are selected", () => {
    expect(arePrereqsSatisfiedNode(buildState(), "csc148")).toBe(true)
  })

  it("Should accept selected bool parents", () => {
    expect(arePrereqsSatisfiedNode(buildState(), "csc236")).toBe(true)
  })

  it("Should be false when a parent is not in the graph", () => {
    expect(arePrereqsSatisfiedNode(buildState(), "csc165")).toBe(false)
  })
})

describe("arePrereqsSatisfiedHybrid", () => {
  it("Should require all courses in an AND prereq string", () => {
    const state = buildState()
    expect(arePrereqsSatisfiedHybrid(state, "hAnd")).toBe(false)
    state.nodesStatus.csc148.selected = true
    expect(arePrereqsSatisfiedHybrid(state, "hAnd")).toBe(true)
  })

  it("Should require one course in an OR prereq string", () => {
    const state = buildState()
    expect(arePrereqsSatisfiedHybrid(state, "hOr")).toBe(true)
    state.nodesStatus.csc108.selected = false
    expect(arePrereqsSatisfiedHybrid(state, "hOr")).toBe(false)
  })

  it("Should be false when a prereq course is not in the graph", () => {
    expect(arePrereqsSatisfiedHybrid(buildState(), "hMissing")).toBe(false)
  })
})
