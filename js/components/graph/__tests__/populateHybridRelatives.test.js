import { populateHybridRelatives } from "../Graph.js"

describe("populateHybridRelatives", () => {
  let hybridNode
  let nodesJSON
  let parents
  let childrenObj
  beforeEach(() => {
    jest.spyOn(global.console, "error").mockImplementation(() => {})
  })
  afterEach(() => {
    global.console.error.mockRestore()
  })

  describe("The entire text of hybridNode is in nodesJSON", () => {
    beforeEach(() => {
      hybridNode = {
        id_: "h50",
        text: [
          {
            text: "MAT223/240",
          },
        ],
        type_: "Hybrid",
      }
      nodesJSON = [
        {
          id_: "mat223240",
          text: [
            {
              text: "MAT223/240",
            },
          ],
          type_: "Node",
        },
      ]
      parents = {
        h50: [],
      }
      childrenObj = {
        mat223240: [],
      }
    })
    test("Test populateHybridRelatives should add the id of the hybridNode as a\
         value to its parent in childrenObj, and add the id of the parent node as \
         a value to hybridNode in parents", () => {
      populateHybridRelatives(hybridNode, nodesJSON, parents, childrenObj)
      expect(parents[hybridNode.id_]).toContain("mat223240")
      expect(childrenObj[nodesJSON[0].id_]).toContain("h50")
    })
  })

  describe("hybridNode only has one prereq node", () => {
    beforeEach(() => {
      hybridNode = {
        id_: "h50",
        text: [
          {
            text: "Alg1",
          },
        ],
        type_: "Hybrid",
      }
      nodesJSON = [
        {
          id_: "mat221223240alg1",
          text: [
            {
              text: "MAT221/223/240",
            },
            {
              text: "(Alg1)",
            },
          ],
          type_: "Node",
        },
      ]
      parents = {
        h50: [],
      }
      childrenObj = {
        mat221223240alg1: [],
      }
    })
    test("Test populateHybridRelatives should add the id of the hybridNode as a\
      value to its parent in childrenObj, and add the id of the parent node as \
      a value to hybridNode in parents", () => {
      populateHybridRelatives(hybridNode, nodesJSON, parents, childrenObj)
      expect(parents[hybridNode.id_]).toContain("mat221223240alg1")
      expect(childrenObj[nodesJSON[0].id_]).toContain("h50")
    })
    test("Test populateHybridRelatives should log an error when the prereq node\
        for hybridNode cannot be found. parents should have h0 as a key and\
        childrenObj should not change", () => {
      hybridNode = {
        id_: "h0",
        text: [
          {
            text: "Not In nodesJSON",
          },
        ],
        type_: "Hybrid",
      }
      const childrenOriginal = JSON.parse(JSON.stringify(childrenObj))
      populateHybridRelatives(hybridNode, nodesJSON, parents, childrenObj)
      const expectedParents = {
        h50: [],
        h0: [],
      }
      expect(console.error).toHaveBeenCalledTimes(1)
      expect(parents).toEqual(expectedParents)
      expect(childrenObj).toEqual(childrenOriginal)
    })
  })

  describe('hybridNode has multiple "or" prereq nodes', () => {
    beforeEach(() => {
      hybridNode = {
        id_: "h62",
        text: [
          {
            text: "CSC111/165/Calc1",
          },
        ],
        type_: "Hybrid",
      }
      nodesJSON = [
        {
          id_: "csc111",
          text: [
            {
              text: "CSC111",
            },
          ],
          type_: "Node",
        },
        {
          id_: "csc165",
          text: [
            {
              text: "CSC165",
            },
          ],
          type_: "Node",
        },
        {
          id_: "mat135136137157calc1",
          text: [
            {
              text: "MAT(135,136)/137/157",
            },
            {
              text: "Calc1",
            },
          ],
          type_: "Node",
        },
      ]
      parents = {
        csc111: [],
        csc165: [],
        mat135136137157calc1: [],
        h62: [],
      }
      childrenObj = {
        csc111: [],
        csc165: [],
        mat135136137157calc1: [],
      }
    })
    test("Test populateHybridRelatives should mutate parents and childrenObj \
        to create a relationship between parents and children", () => {
      populateHybridRelatives(hybridNode, nodesJSON, parents, childrenObj)
      const expectedParents = {
        csc111: [],
        csc165: [],
        mat135136137157calc1: [],
        h62: [["csc111", "csc165", "mat135136137157calc1"]],
      }
      const expectedChildren = {
        csc111: ["h62"],
        csc165: ["h62"],
        mat135136137157calc1: ["h62"],
      }
      expect(parents).toEqual(expectedParents)
      expect(childrenObj).toEqual(expectedChildren)
    })
    test("Test populateHybridRelatives should log an error when it cannot find a\
        prereq node with multiple other nodes. The nodes before should still\
        be mutated", () => {
      nodesJSON.splice(nodesJSON.indexOf(-1), 1)
      const expectedParents = {
        csc111: [],
        csc165: [],
        mat135136137157calc1: [],
        h62: [["csc111", "csc165"]],
      }
      const expectedChildren = {
        csc111: ["h62"],
        csc165: ["h62"],
        mat135136137157calc1: [],
      }
      populateHybridRelatives(hybridNode, nodesJSON, parents, childrenObj)
      expect(console.error).toHaveBeenCalledTimes(1)
      expect(parents).toEqual(expectedParents)
      expect(childrenObj).toEqual(expectedChildren)
    })
  })
})
