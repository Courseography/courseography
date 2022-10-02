import { findRelationship } from "../Graph.js"

describe("findRelationship", () => {
  describe("nodesJSON contains a node for CSC207", () => {
    const nodesJSON = [
      {
        id_: "csc207",
        text: [
          {
            text: "CSC207",
          },
        ],
        type_: "Node",
      },
    ]

    test("Test findRelationship should return the correct node based on course code ", () => {
      const actual = findRelationship("CSC207", nodesJSON)
      const expected = nodesJSON[0]
      expect(actual).toEqual(expected)
    })

    test("Test findRelationship should return undefined when the node for the course is not in the array", () => {
      const actual = findRelationship("MAT137", nodesJSON)
      expect(actual).toBeUndefined()
    })
  })

  describe("nodesJSON contains two elements and one of them has an incorrect structure", () => {
    const nodesJSON = [
      {
        id_: "csc207",
        text: [
          {
            text: "MAT137",
          },
        ],
        type_: "Node",
      },
      {
        id: "csc207",
        text: [
          {
            text: "CSC207",
          },
        ],
      },
    ]
    test("Test findRelationship should return undefined when the structure for the course is incorrect", () => {
      const actual = findRelationship("CSC207", nodesJSON)
      expect(actual).toBeUndefined()
    })
  })

  describe("nodesJSON contains two elements that contain the same id but different text", () => {
    const nodesJSON = [
      {
        id_: "csc207",
        text: [
          {
            text: "MAT137",
          },
        ],
        type_: "Node",
      },
      {
        id: "csc207",
        text: [
          {
            text: "CSC207",
          },
        ],
        type_: "Node",
      },
    ]
    test("Test findRelationship returns the second node when the first node has the correct id", () => {
      const actual = findRelationship("CSC207", nodesJSON)
      const expected = nodesJSON[1]
      expect(actual).toEqual(expected)
    })
  })

  describe("nodesJSON has object type other than Node", () => {
    const nodesJSON = [
      {
        id_: "MAT137",
        text: [
          {
            text: "MAT137",
          },
        ],
        type_: "NotNode",
      },
      {
        id: "csc207",
        text: [
          {
            text: "CSC207",
          },
        ],
        type_: "Node",
      },
    ]
    test("Test findRelationship should return undefined when the course node has type other than Node", () => {
      const actual = findRelationship("MAT137", nodesJSON)
      expect(actual).toBeUndefined()
    })
    test("Test findRelationship should return the course node even if another node has incorrect type", () => {
      const actual = findRelationship("CSC207", nodesJSON)
      const expected = nodesJSON[1]
      expect(actual).toEqual(expected)
    })
  })

  describe("nodesJSON contains two elements that contain the same type and same text", () => {
    const nodesJSON = [
      {
        id_: "MAT137",
        text: [
          {
            text: "MAT137",
          },
        ],
        type_: "Node",
      },
      {
        id: "MAT137",
        text: [
          {
            text: "MAT137",
          },
        ],
        type_: "Node",
      },
    ]
    test("Test findRelationship should return the first course node when there are more than one valid course nodes", () => {
      const actual = findRelationship("MAT137", nodesJSON)
      const expected = nodesJSON[0]
      expect(actual).toEqual(expected)
    })
  })
})
