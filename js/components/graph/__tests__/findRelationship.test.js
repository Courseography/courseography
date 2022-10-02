import { findRelationship } from "../Graph.js"

const recursiveChecker = (actual, expected) => {
  if (typeof actual === "string") {
    expect(actual).toMatch(expected)
  } else if (typeof actual === "object") {
    if (Array.isArray(actual)) {
      actual.forEach((arr, i) => {
        recursiveChecker(arr, expected[i])
      })
    } else {
      // not an array, an object, we go through each value
      Object.keys(actual).forEach(key => {
        recursiveChecker(actual[key], expected[key])
      })
    }
  }
}

describe("findRelationship", () => {
  // data found from acutal input
  // filter out everything other than

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

    test("Test findRelationship finds the correct node based on course code ", () => {
      const actual = findRelationship("CSC207", nodesJSON)
      const expected = nodesJSON[0]
      recursiveChecker(actual, expected)
    })

    test("Test findRelationship returns undefined when the node for the course is not in the array", () => {
      const actual = findRelationship("MAT137", nodesJSON)
      expect(actual).toBeUndefined()
    })
  })
  describe("nodesJSON contains two elements that contain the same type but different text, only the object with the correct text is returned", () => {
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
    test("Test findRelationship returns undefined when the structure for the course is incorrect", () => {
      const actual = findRelationship("CSC207", nodesJSON)
      const expected = nodesJSON[1]
      recursiveChecker(actual, expected)
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
      },
    ]
    test("Test findRelationship returns undefined when the course node has type other than Node", () => {
      const actual = findRelationship("MAT137", nodesJSON)
      expect(actual).toBeUndefined()
    })
    test("Test findRelationship returns the course node even if another node has incorrect type", () => {
      const actual = findRelationship("CSC207", nodesJSON)
      const expected = nodesJSON[1]
      recursiveChecker(actual, expected)
    })
  })
  describe("nodesJSON contains two elements that contain the same type and same text, so the first object should be returned", () => {
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
      },
    ]
    test("Test findRelationship returns the first course node when there are more than one valid course nodes", () => {
      const actual = findRelationship("MAT137", nodesJSON)
      const expected = nodesJSON[0]
      recursiveChecker(actual, expected)
    })
  })
})
