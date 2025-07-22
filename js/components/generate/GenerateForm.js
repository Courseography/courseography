import React from "react"
import { ErrorMessage, Field, Form, Formik } from "formik"
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome"
import { faWandSparkles } from "@fortawesome/free-solid-svg-icons"
import { Tooltip } from "react-tooltip"
import { Graph, populateHybridRelatives } from "../graph/Graph"
import Disclaimer from "../common/Disclaimer"
import { NavBar } from "../common/NavBar.js.jsx"

export default class GenerateForm extends React.Component {
  constructor(props) {
    super(props)
    this.state = {
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

  handleSubmit = (values, { setErrors }) => {
    const data = {}

    for (const key in values) {
      if (["courses", "programs", "taken", "departments"].includes(key)) {
        data[key] = values[key].split(",").map(s => s.trim())
      } else {
        data[key] = values[key]
      }
    }

    let submittedCourses = data["courses"]

    if (values.category === "programs") {
      data["courses"] = []
    } else {
      data["programs"] = []
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
        const returnedTexts = data.texts?.map(t => t.text) ?? []

        const missingCourses = submittedCourses.filter(
          c => !returnedTexts.includes(c.toUpperCase())
        )

        if (missingCourses.length !== 0 && missingCourses[0] !== "") {
          setErrors({
            courses:
              missingCourses.length === 1
                ? `Invalid course code: ${missingCourses}`
                : `Invalid course codes: ${missingCourses.join(", ")}`,
          })
        }

        const missingPrograms = data.error?.invalidPrograms ?? []

        if (missingPrograms.length !== 0) {
          setErrors({
            programs:
              missingPrograms.length === 1
                ? `Invalid program code: ${missingPrograms}`
                : `Invalid program codes: ${missingPrograms.join(", ")}`,
          })
        }

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
          // filter for mark percentages, allow preceding characters for potential geq
          if (entry.text.match(/.*[0-9]*%/g)) {
            labelsJSON[entry.rId] = entry
          }
        })

        data.shapes.forEach(function (entry) {
          if (entry.id_.includes("|")) {
            return
          }
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

  validateForm = values => {
    const errors = {}

    const coursePattern = /^[A-Za-z]{3}\d{3}[HYhy]\d$/
    const deptPattern = /^[A-Za-z]{3}$/
    const programPattern = /^[A-Za-z]{5}\d{4}[A-Za-z]?$/

    if (values.category === "courses") {
      if (!values.courses.trim().length) {
        errors.courses = "Cannot generate graph – no courses entered!"
      } else {
        const courses = values.courses.split(",").map(course => course.trim())
        const invalidCourses = courses.filter(course => !coursePattern.test(course))

        if (invalidCourses.length > 0) {
          errors.courses =
            invalidCourses.length === 1
              ? `Invalid course code: ${invalidCourses}`
              : `Invalid course codes: ${invalidCourses.join(", ")}`
        }
      }
    }

    if (values.category === "programs") {
      if (!values.programs.trim().length) {
        errors.programs = "Cannot generate graph – no programs entered!"
      } else {
        const programs = values.programs.split(",").map(program => program.trim())
        const invalidPrograms = programs.filter(
          program => !programPattern.test(program)
        )

        if (invalidPrograms.length > 0) {
          errors.programs =
            invalidPrograms.length === 1
              ? `Invalid program code: ${invalidPrograms}`
              : `Invalid program codes: ${invalidPrograms.join(", ")}`
        }
      }
    }

    if (values.departments && values.departments.trim()) {
      const departments = values.departments.split(",").map(dept => dept.trim())
      const invalidDepartments = departments.filter(dept => !deptPattern.test(dept))

      if (invalidDepartments.length > 0) {
        errors.departments =
          invalidDepartments.length === 1
            ? `Invalid department: ${invalidDepartments}`
            : `Invalid departments: ${invalidDepartments.join(", ")}`
      }
    }

    if (values.taken && values.taken.trim()) {
      const takenCourses = values.taken.split(",").map(course => course.trim())
      const invalidTaken = takenCourses.filter(course => !coursePattern.test(course))

      if (invalidTaken.length > 0) {
        errors.taken =
          invalidTaken.length === 1
            ? `Invalid course code: ${invalidTaken}`
            : `Invalid course codes: ${invalidTaken.join(", ")}`
      }
    }

    return errors
  }

  render() {
    return (
      <>
        <NavBar selected_page="generate" open_modal={undefined}></NavBar>
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
            <Formik
              initialValues={{
                category: "courses",
                courses: "",
                programs: "",
                taken: "",
                departments: "CSC, MAT, STA",
                maxDepth: 0,
                location: ["utsg"],
                includeRaws: false,
                includeGrades: false,
              }}
              validate={this.validateForm}
              validateOnChange={false}
              validateOnBlur={false}
              onSubmit={this.handleSubmit}
            >
              {({ values }) => (
                <Form id="generateForm">
                  <div className="form-section">
                    <div className="title-container">
                      <h1 id="header-title" className="section-title">
                        Select Search Input
                      </h1>
                      <a
                        data-tooltip-id="category-tooltip"
                        data-tooltip-html="Select between courses and programs to search for"
                        className="tooltip-icon"
                        style={{ marginTop: "-0.3rem" }}
                      ></a>
                      <Tooltip id="category-tooltip" place="right" />
                    </div>
                    <Field as="select" id="category" name="category">
                      <option value="courses">Courses</option>
                      <option value="programs">Programs</option>
                    </Field>

                    {values.category === "courses" && (
                      <>
                        <div className="title-container">
                          <h1 id="header-title" className="section-title">
                            Search Courses
                          </h1>
                          <a
                            data-tooltip-id="courses-tooltip"
                            data-tooltip-html="Generate the prerequisites for the given course(s).<br />
                        Each course code must follow the format CSC108H1<br />
                        (i.e. department + code + session)"
                            className="tooltip-icon"
                            style={{ marginTop: "-0.3rem" }}
                          ></a>
                          <Tooltip id="courses-tooltip" place="right" />
                        </div>
                        <Field
                          id="courses"
                          name="courses"
                          type="text"
                          placeholder="e.g., CSC207H1, CSC324H1"
                        />
                        <div className="error-container">
                          <ErrorMessage
                            className="error-message"
                            name="courses"
                            component="div"
                          />
                        </div>
                      </>
                    )}

                    {values.category === "programs" && (
                      <>
                        <div className="title-container">
                          <h1 id="header-title" className="section-title">
                            Search Programs
                          </h1>
                          <a
                            data-tooltip-id="programs-tooltip"
                            data-tooltip-html="Generate the requirements for the given program(s).<br />
                        Each program code must follow the format ASMAJ1689"
                            className="tooltip-icon"
                            style={{ marginTop: "-0.3rem" }}
                          ></a>
                          <Tooltip id="programs-tooltip" place="right" />
                        </div>
                        <Field
                          id="programs"
                          name="programs"
                          type="text"
                          placeholder="e.g., ASMAJ1689, ASFOC1689B"
                        />
                        <div className="error-container">
                          <ErrorMessage
                            className="error-message"
                            name="programs"
                            component="div"
                          />
                        </div>
                      </>
                    )}
                  </div>

                  <div className="form-section">
                    <h2 id="filter-title" className="section-title">
                      Filters
                    </h2>

                    <div className="title-container">
                      <label htmlFor="departments">Departments</label>
                      <a
                        data-tooltip-id="departments-tooltip"
                        data-tooltip-html="Only include courses from these departments.<br />
                        Department codes must be 3 letters, seperated by commas."
                        className="tooltip-icon"
                      ></a>
                      <Tooltip id="departments-tooltip" place="right" />
                    </div>
                    <Field
                      id="departments"
                      name="departments"
                      type="text"
                      placeholder="e.g., CSC, MAT, STA"
                    />
                    <div className="error-container">
                      <ErrorMessage
                        className="error-message"
                        name="departments"
                        component="div"
                      />
                    </div>

                    <div className="title-container">
                      <label htmlFor="taken">Hide courses</label>
                      <a
                        data-tooltip-id="taken-tooltip"
                        data-tooltip-html="Do not show these courses or their prerequisites.<br />
                        Each course code must follow the format CSC108H1<br />
                        (i.e. department + code + session)"
                        className="tooltip-icon"
                      ></a>
                      <Tooltip
                        id="taken-tooltip"
                        className="tooltip-box"
                        place="right"
                      />
                    </div>
                    <Field
                      id="taken"
                      name="taken"
                      type="text"
                      placeholder="e.g., CSC207H1, CSC236H1"
                    />
                    <div className="error-container">
                      <ErrorMessage
                        className="error-message"
                        name="taken"
                        component="div"
                      />
                    </div>

                    <div className="title-container">
                      <label htmlFor="maxDepth">Prerequisite depth</label>
                      <a
                        data-tooltip-id="maxDepth-tooltip"
                        data-tooltip-content="Depth of prerequisite chain (0 shows all prerequisites)"
                        className="tooltip-icon"
                      ></a>
                      <Tooltip id="maxDepth-tooltip" place="right" />
                    </div>
                    <Field
                      id="maxDepth"
                      name="maxDepth"
                      type="number"
                      min="0"
                      step="1"
                    />

                    {/* <label htmlFor="location">Campus</label>
                  <Field id="location" name="location" as="select" multiple
                    style={{ verticalAlign: 'text-top', marginLeft: '1em', marginBottom: '1em', color: 'black' }}>
                    <option value="utsg">St. George</option>
                    <option value="utm">Mississauga</option>
                    <option value="utsc">Scarborough</option>
                  </Field>

                  <p>
                    <label htmlFor="includeRaws">Include non-course prerequisites</label>
                    <Field id="includeRaws" name="includeRaws" type="checkbox"
                      style={{ marginLeft: '1em', verticalAlign: 'middle' }}
                    />
                  </p>

                  <label htmlFor="includeGrades">Include grade-based prerequisites</label>
                  <Field id="includeGrades" name="includeGrades" type="checkbox"
                    style={{ 'margin-left': '1em', 'vertical-align': 'middle' }} /> */}
                  </div>

                  <div
                    style={{
                      width: "100%",
                      display: "flex",
                      justifyContent: "center",
                    }}
                  >
                    <button id="submit" type="submit">
                      <FontAwesomeIcon icon={faWandSparkles} id="generate-icon" />
                      Generate
                    </button>
                  </div>
                </Form>
              )}
            </Formik>
          </div>

          <Graph
            ref={this.graph}
            start_blank={true}
            fceCount={this.state.fceCount}
            incrementFCECount={this.incrementFCECount}
            setFCECount={this.setFCECount}
          />
        </div>
      </>
    )
  }
}
