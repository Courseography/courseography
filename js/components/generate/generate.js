import "core-js/stable"
import "regenerator-runtime/runtime"

import React from "react"
import ReactDOM from "react-dom"

import { Graph, populateHybridRelatives } from "../graph/Graph"
import Disclaimer from "../common/Disclaimer"

class GenerateForm extends React.Component {
  constructor(props) {
    super(props)
    this.state = {
      courses: "",
      taken: "",
      departments: "CSC, MAT, STA",
      maxDepth: 0,
      location: ["utsg"],
      includeRaws: false,
      includeGrades: false,
      fceCount: 0,
    }

    this.graph = React.createRef()
  }

  setFCECount = credits => {
    this.setState({ fceCount: credits })
  }

  incrementFCECount = credits => {
    this.setState({ fceCount: this.state.fceCount + credits })
  }

  handleInputChange = event => {
    const target = event.target
    let value
    if (target.type === "checkbox") {
      value = target.checked
    } else if (target.type === "select-multiple") {
      value = Array.from(target.selectedOptions, option => option.value)
    } else if (target.type === "number") {
      value = parseInt(target.value)
    } else {
      value = target.value
    }
    const name = target.name

    this.setState({
      [name]: value,
    })
  }

  handleSubmit = event => {
    event.preventDefault()

    if (!this.state.courses.length) {
      alert("Cannot generate graph -- no courses entered!")
      return
    }
    const data = {}

    for (const key in this.state) {
      if (["courses", "taken", "departments"].includes(key)) {
        data[key] = this.state[key].split(",").map(s => s.trim())
      } else {
        data[key] = this.state[key]
      }
    }

    const putData = {
      method: "PUT",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify(data), // We send data in JSON format
    }

    fetch("/graph-generate", putData)
      .then(res => res.json())
      .then(data => {
        const labelsJSON = {}
        const regionsJSON = {}
        const nodesJSON = {}
        const hybridsJSON = {}
        const boolsJSON = {}
        const edgesJSON = {}
        const boolsStatus = {}
        const nodesStatus = {}
        const parentsObj = {}
        const inEdgesObj = {}
        const childrenObj = {}
        const outEdgesObj = {}
        const storedNodes = new Set()

        data.texts.forEach(entry => {
          if (entry.text.match(/.*[0-9]*%/g)) {
            labelsJSON[entry.rId] = entry
            // filter for mark percentages, allow preceding characters for potential geq
          }
        })

        data.shapes.forEach(function (entry) {
          if (entry.type_ === "Node") {
            nodesJSON[entry.id_] = entry
          } else if (entry.type_ === "Hybrid") {
            hybridsJSON[entry.id_] = entry
          } else if (entry.type_ === "BoolNode") {
            boolsStatus[entry.id_] = "inactive"
            boolsJSON[entry.id_] = entry
          }
        })

        data.paths.forEach(function (entry) {
          if (entry.isRegion) {
            regionsJSON[entry.id_] = entry
          } else {
            edgesJSON[entry.id_] = entry
          }
        })

        Object.values(nodesJSON).forEach(node => {
          parentsObj[node.id_] = []
          inEdgesObj[node.id_] = []
          childrenObj[node.id_] = []
          outEdgesObj[node.id_] = []
          // Quickly adding any active nodes from local storage into the selected nodes
          let state = localStorage.getItem(node.id_)
          if (state === null) {
            state = parentsObj[node.id_].length === 0 ? "takeable" : "inactive"
          } else if (state === "active") {
            storedNodes.add(node.text[node.text.length - 1].text)
          }

          nodesStatus[node.id_] = {
            status: state,
            selected: ["active", "overridden"].indexOf(state) >= 0,
          }
        })

        Object.values(hybridsJSON).forEach(hybrid => {
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

        Object.values(edgesJSON).forEach(edge => {
          if (edge.target in parentsObj) {
            parentsObj[edge.target].push(edge.source)
            inEdgesObj[edge.target].push(edge.id_)
          }

          if (edge.source in childrenObj) {
            childrenObj[edge.source].push(edge.target)
            outEdgesObj[edge.source].push(edge.id_)
          }
        })

        Object.keys(boolsJSON).forEach(boolId => {
          const parents = []
          const childs = []
          const outEdges = []
          const inEdges = []
          Object.values(edgesJSON).forEach(edge => {
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

        const edgesStatus = Object.values(edgesJSON).reduce((acc, curr) => {
          const source = curr.source
          const target = curr.target
          let status
          const isSourceSelected =
            nodesStatus[source]?.selected || boolsStatus[source] === "active"

          const isTargetSelected =
            nodesStatus[target]?.selected || boolsStatus[target] === "active"

          const targetStatus = nodesStatus[target]?.status || boolsStatus[target]

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
        }, {})

        this.graph.current.setState({
          labelsJSON: labelsJSON,
          regionsJSON: regionsJSON,
          nodesJSON: nodesJSON,
          hybridsJSON: hybridsJSON,
          boolsJSON: boolsJSON,
          edgesJSON: edgesJSON,
          nodesStatus: nodesStatus,
          edgesStatus: edgesStatus,
          boolsStatus: boolsStatus,
          width: data.width,
          height: data.height,
          zoomFactor: 1,
          horizontalPanFactor: 0,
          verticalPanFactor: 0,
          connections: {
            parents: parentsObj,
            inEdges: inEdgesObj,
            children: childrenObj,
            outEdges: outEdgesObj,
          },
          selectedNodes: storedNodes,
        })
      })
      .catch(err => {
        console.log("err :>> ", err)
      })
  }

  render() {
    return (
      <div style={{ display: "flex", flexDirection: "row", height: "100%" }}>
        <Disclaimer />
        <div
          id="generateDiv"
          style={{
            position: "initial",
            padding: "0 0.5em",
            height: "100%",
            fontSize: "12pt",
          }}
        >
          <h1 id="header-title">Search for courses</h1>
          <form id="generateForm">
            <input
              id="courses"
              name="courses"
              type="text"
              placeholder="e.g., CSC207H1, CSC324H1"
              value={this.state.courses}
              onChange={this.handleInputChange}
            />

            <h2 id="filter-title">Optional filters</h2>

            <label htmlFor="departments">Only include courses these departments</label>
            <input
              id="departments"
              name="departments"
              type="text"
              placeholder="Enter 3-letter department codes separated by commas"
              value={this.state.departments}
              onChange={this.handleInputChange}
              style={{ marginBottom: "1em" }}
            />

            <label htmlFor="taken">Do not show these courses</label>
            <input
              id="taken"
              name="taken"
              type="text"
              value={this.state.taken}
              onChange={this.handleInputChange}
              style={{ marginBottom: "1em" }}
              placeholder="E.g., CSC207H1, CSC236H1"
            />

            <label htmlFor="maxDepth">
              Depth of prerequisite chain (0 shows all prerequisites)
            </label>
            <p>
              <input
                id="maxDepth"
                name="maxDepth"
                type="number"
                min="0"
                step="1"
                value={this.state.maxDepth}
                onChange={this.handleInputChange}
                style={{ marginBottom: "1em" }}
              />
            </p>

            {/* <label htmlFor="location">Campus</label>
          <select id="location" name="location" multiple
            value={this.state.location}
            onChange={this.handleInputChange}
            style={{'vertical-align': 'text-top', 'margin-left': '1em', 'margin-bottom': '1em', 'color': 'black'}} >
            <option value="utsg">St. George</option>
            <option value="utm">Mississauga</option>
            <option value="utsc">Scarborough</option>
          </select>

          <p>
          <label htmlFor="includeRaws">Include non-course prerequisites</label>
          <input id="includeRaws" name="includeRaws" type="checkbox"
                  value={this.state.includeRaws}
                  onChange={this.handleInputChange}
                  style={{'margin-left': '1em', 'vertical-align': 'middle'}} />
          </p>

          <label htmlFor="includeGrades">Include grade-based prerequisites</label>
          <input id="includeGrades" name="includeGrades" type="checkbox"
                  value={this.state.includeGrades}
                  onChange={this.handleInputChange}
                  style={{'margin-left': '1em', 'vertical-align': 'middle'}} /> */}

            <div
              style={{
                marginTop: "1em",
                width: "100%",
                display: "flex",
                justifyContent: "center",
              }}
            >
              <button id="submit" onClick={this.handleSubmit}>
                Generate Graph
              </button>
            </div>
          </form>
        </div>

        <Graph
          ref={this.graph}
          start_blank={true}
          fceCount={this.state.fceCount}
          incrementFCECount={this.incrementFCECount}
          setFCECount={this.setFCECount}
        />
      </div>
    )
  }
}

ReactDOM.render(<GenerateForm />, document.getElementById("generateRoot"))
