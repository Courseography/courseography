import React from "react"
import PropTypes from "prop-types"
import { CourseModal } from "../common/react_modal.js.jsx"
import { ExportModal } from "../common/export.js.jsx"
import { getPost } from "../common/utils.js"
import Bool from "./Bool"
import Edge from "./Edge"
import Node from "./Node"
import Button from "./Button"
import InfoBox from "./InfoBox"
import GraphDropdown from "./GraphDropdown"
import Sidebar from "./Sidebar"
import { parseAnd } from "../../util/util.js"

const ZOOM_INCREMENT = 0.01
const KEYBOARD_PANNING_INCREMENT = 10
const ZOOM_ENUM = {
  ZOOM_OUT: -1,
  ZOOM_IN: 1,
}
const TIMEOUT_NAMES_ENUM = {
  INFOBOX: 0,
  DROPDOWN: 1,
}

export class Graph extends React.Component {
  constructor(props) {
    super(props)
    this.state = {
      labelsJSON: {},
      regionsJSON: {},
      nodesJSON: {},
      nodesStatus: {},
      hybridsJSON: {},
      boolsJSON: {},
      edgesJSON: {},
      edgesStatus: {},
      boolsStatus: {},
      highlightedNodesFocus: [],
      highlightedNodesDeps: [],
      infoboxTimeouts: [],
      dropdownTimeouts: [],
      width: window.innerWidth,
      height: window.innerHeight,
      zoomFactor: 1,
      horizontalPanFactor: 0,
      verticalPanFactor: 0,
      buttonHover: false,
      onDraw: this.props.edit,
      drawMode: this.props.initialDrawMode,
      drawNodeID: 0,
      draggingNode: null,
      focusCourses: {},
      graphName: null,
      connections: null,
      showInfoBox: false,
      infoBoxXPos: 0,
      infoBoxYPos: 0,
      infoBoxNodeId: "",
      panning: false,
      panStartX: 0,
      panStartY: 0,
      showCourseModal: false,
      showGraphDropdown: false,
      selectedNodes: new Set(),
    }
    this.exportModal = React.createRef()
    this.nodeDropshadowFilter = "dropshadow"
  }

  componentDidMount() {
    if (!this.props.start_blank) {
      this.getGraph()
    }

    // can't detect keydown event when adding event listener to react-graph
    document.body.addEventListener("keydown", this.onKeyDown)

    // Enable "Export" link
    if (document.getElementById("nav-export")) {
      document
        .getElementById("nav-export")
        .addEventListener("click", this.exportModal.current.openModal)
    }

    if (document.querySelector("#nav-graph > a")) {
      document
        .querySelector("#nav-graph > a")
        .addEventListener("mouseenter", this.setShowGraphDropdown)
      document
        .querySelector("#nav-graph > a")
        .addEventListener("mouseleave", this.hideGraphDropdown)
    }

    if (document.querySelector(".sidebar")) {
      document
        .querySelector(".sidebar")
        .addEventListener("wheel", event => event.stopPropagation())
    }
  }

  UNSAFE_componentWillUpdate(prevProps) {
    if (!!this.state.graphName && this.state.graphName !== prevProps.graphName) {
      this.getGraph()
    }
  }

  componentWillUnmount() {
    this.state.infoboxTimeouts.forEach(timeout => clearTimeout(timeout))
    this.state.dropdownTimeouts.forEach(timeout => clearTimeout(timeout))
    document.body.removeEventListener("keydown", this.onKeyDown)

    if (document.getElementById("nav-export")) {
      document
        .getElementById("nav-export")
        .removeEventListener("click", this.exportModal.current.openModal)
    }

    if (document.querySelector("#nav-graph > a")) {
      document
        .querySelector("#nav-graph > a")
        .removeEventListener("mouseenter", this.setShowGraphDropdown)
      document
        .querySelector("#nav-graph > a")
        .removeEventListener("mouseleave", this.hideGraphDropdown)
    }
  }

  getGraph = () => {
    const graphName = this.props.graphName.replace("-", " ")
    const url = new URL("/get-json-data", document.location)
    const params = { graphName: graphName }
    Object.keys(params).forEach(key => url.searchParams.append(key, params[key]))

    fetch(url)
      .then(headers => {
        if (!headers.ok) {
          // can't just return res
          const headerInfo = {
            status: headers.status,
            statusText: headers.statusText,
            type: headers.type,
            url: headers.url,
          }
          throw new Error(
            "When fetching from the url with info " + JSON.stringify(headerInfo)
          )
        }
        return headers.json() // only received headers, waiting for data
      })
      .then(data => {
        localStorage.setItem("active-graph", graphName)
        const labelsJSON = {}
        const regionsJSON = {}
        const nodesJSON = {}
        const nodesStatus = {}
        const hybridsJSON = {}
        const boolsJSON = {}
        const boolsStatusObj = {}
        const edgesJSON = {}
        const parentsObj = {}
        const inEdgesObj = {}
        const childrenObj = {}
        const outEdgesObj = {}
        const storedNodes = new Set()

        data.texts.forEach(entry => {
          if (entry.rId.startsWith("tspan")) {
            labelsJSON[entry.rId] = entry
          }
        })

        data.shapes.forEach(function (entry) {
          if (entry.type_ === "Node") {
            nodesJSON[entry.id_] = entry
          } else if (entry.type_ === "Hybrid") {
            hybridsJSON[entry.id_] = entry
          } else if (entry.type_ === "BoolNode") {
            boolsStatusObj[entry.id_] = localStorage.getItem(entry.id_) || "inactive"
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

        // The duplicate filter is a temporary fix, as previously there were two nodes, Hybrid and Node,
        // that were placed in the same spot on some graphs where there should be only a Hybrid node.
        const noDuplicatesNodesJSON = {}
        Object.values(nodesJSON).forEach(node => {
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

        Object.keys(noDuplicatesNodesJSON).forEach(nodeId => {
          let state = localStorage.getItem(nodeId)
          if (state === null) {
            state = parentsObj[nodeId].length === 0 ? "takeable" : "inactive"
          }
          nodesStatus[nodeId] = {
            status: state,
            selected: ["active", "overridden"].indexOf(state) >= 0,
          }
        })

        const edgesStatus = Object.values(edgesJSON).reduce((acc, curr) => {
          const source = curr.source
          const target = curr.target
          let status
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
        }, {})

        this.setState({
          labelsJSON: labelsJSON,
          regionsJSON: regionsJSON,
          nodesJSON: noDuplicatesNodesJSON,
          hybridsJSON: hybridsJSON,
          boolsJSON: boolsJSON,
          boolsStatus: boolsStatusObj,
          edgesJSON: edgesJSON,
          width: data.width,
          height: data.height,
          zoomFactor: 1,
          horizontalPanFactor: 0,
          verticalPanFactor: 0,
          graphName: graphName,
          edgesStatus: edgesStatus,
          connections: {
            parents: parentsObj,
            inEdges: inEdgesObj,
            children: childrenObj,
            outEdges: outEdgesObj,
          },
          nodesStatus: nodesStatus,
          selectedNodes: storedNodes,
        })
      })
      .catch(err => {
        console.error("Fetch API failed. Here are the headers: ")
        console.error(err)
      })
  }

  componentDidUpdate(prevProps, prevState) {
    if (prevState.nodesJSON !== this.state.nodesJSON) {
      let totalFCEs = 0
      Object.values(this.state.nodesJSON).forEach(nodeJSON => {
        if (
          !this.state.hybridsJSON[nodeJSON.id_] &&
          this.state.nodesStatus[nodeJSON.id_].selected
        ) {
          totalFCEs += 0.5
        }
      })
      if (this.props.setFCECount) {
        this.props.setFCECount(totalFCEs)
      }
    }

    if (this.props.currFocus !== prevProps.currFocus && this.props.currFocus === null) {
      this.highlightFocuses([])
    } else if (this.props.currFocus !== prevProps.currFocus) {
      const currFocusCourses = this.state.focusCourses[this.props.currFocus]
      getPost(
        this.props.currFocus,
        currFocusCourses?.modifiedTime || new Date(0).toUTCString()
      ).then(focusData => {
        if (!focusData.modified) {
          this.highlightFocuses(currFocusCourses.list)
        } else {
          const focusCourses = this.state.focusCourses
          focusCourses[this.props.currFocus] = {
            list: focusData.courseList,
            modifiedTime: focusData.modifiedTime,
          }
          this.setState({ focusCourses }, () =>
            this.highlightFocuses(this.state.focusCourses[this.props.currFocus].list)
          )
        }
      })
    }
  }

  clearAllTimeouts = timeoutName => {
    switch (timeoutName) {
      case TIMEOUT_NAMES_ENUM.INFOBOX:
        this.state.infoboxTimeouts.forEach(timeout => clearTimeout(timeout))
        this.setState({ infoboxTimeouts: [] })
        break
      case TIMEOUT_NAMES_ENUM.DROPDOWN:
        this.state.dropdownTimeouts.forEach(timeout => clearTimeout(timeout))
        this.setState({ dropdownTimeouts: [] })
        break
    }
  }

  /**
   * Update the status of the Edge, based on the status of the Node/Bool it points from/to.
   */
  updateEdgeStatus = (status, edgeID, source, target) => {
    const isSourceSelected = this.isSelected(source)
    const isTargetSelected = this.isSelected(target)
    const targetStatus = this.state.nodesStatus[target]
      ? this.state.nodesStatus[target].status
      : this.state.boolsStatus[target]

    if (!status) {
      if (!isSourceSelected && targetStatus === "missing") {
        status = "missing"
      } else if (!isSourceSelected) {
        status = "inactive"
      } else if (!isTargetSelected) {
        status = "takeable"
      } else {
        status = "active"
      }
    }
    this.setState(state => {
      const edgesStatus = { ...state.edgesStatus }
      edgesStatus[edgeID] = status
      return {
        edgesStatus: edgesStatus,
      }
    })
  }

  nodeClick = event => {
    let courseId
    if (event.currentTarget.tagName === "g") {
      courseId = event.currentTarget.id
    } else if (event.currentTarget.tagName === "LI") {
      courseId = event.currentTarget.getAttribute("data-node-id")
    } else {
      throw new Error("Invalid Element Type!")
    }
    const courseLabelArray = this.state.nodesJSON[courseId].text
    const courseLabel = courseLabelArray[courseLabelArray.length - 1].text
    const wasSelected = this.state.nodesStatus[courseId].selected
    const temp = [...this.state.selectedNodes]
    this.toggleSelection(courseId)
    if (typeof this.props.incrementFCECount === "function") {
      if (wasSelected) {
        // TODO: Differentiate half- and full-year courses
        this.props.incrementFCECount(-0.5)
        this.setState({ selectedNodes: new Set(temp.filter(e => e !== courseLabel)) })
      } else {
        this.props.incrementFCECount(0.5)
        this.setState({ selectedNodes: new Set([...temp, courseLabel]) })
      }
    }
  }

  nodeUnselect = courseId => {
    const courseLabelArray = this.state.nodesJSON[courseId].text
    const courseLabel = courseLabelArray[courseLabelArray.length - 1].text
    const wasSelected = this.state.nodesStatus[courseId].selected
    const temp = this.state.selectedNodes

    if (typeof this.props.incrementFCECount === "function" && wasSelected) {
      // TODO: Differentiate half- and full-year courses
      this.toggleSelection(courseId)
      this.props.incrementFCECount(-0.5)
      temp.delete(courseLabel)
    }
  }

  /**
   * Drawing mode is not implemented, meaning the onDraw defaults to false right now.
   */
  nodeMouseEnter = event => {
    const currTarg = event.currentTarget
    let courseId
    if (currTarg.tagName === "g") {
      courseId = currTarg.id
    } else if (currTarg.tagName === "LI") {
      courseId = currTarg.getAttribute("data-node-id")
    } else if (currTarg.tagName === "DIV") {
      courseId = currTarg.childNodes[0].textContent.toLowerCase()
    } else {
      throw new Error("Invalid Element Type!")
    }

    const currentNode = this.state.nodesJSON[courseId]
    this.focusPrereqs(courseId)

    this.clearAllTimeouts(TIMEOUT_NAMES_ENUM.INFOBOX)

    let xPos = currentNode.pos[0]
    let yPos = currentNode.pos[1]
    const rightSide = xPos > 222
    // The tooltip is offset with a 'padding' of 5.
    if (rightSide) {
      xPos = parseFloat(xPos) - 65
    } else {
      xPos = parseFloat(xPos) + parseFloat(currentNode.width) + 5
    }

    yPos = parseFloat(yPos)

    if (!this.state.onDraw) {
      this.setState({
        showInfoBox: true,
        infoBoxXPos: xPos,
        infoBoxYPos: yPos,
        infoBoxNodeId: courseId,
      })
    }
    this.setState({ buttonHover: true })
  }

  nodeMouseLeave = event => {
    const currTarg = event.currentTarget
    let courseId
    if (currTarg.tagName === "g") {
      courseId = currTarg.id
    } else if (currTarg.tagName === "LI") {
      courseId = currTarg.getAttribute("data-node-id")
    } else if (currTarg.tagName === "DIV") {
      courseId = currTarg.childNodes[0].textContent.toLowerCase()
    } else {
      throw new Error("Invalid Element Type!")
    }

    this.unfocusPrereqs(courseId)

    const timeout = setTimeout(() => {
      this.setState({ showInfoBox: false })
    }, 400)

    this.setState({
      infoboxTimeouts: this.state.infoboxTimeouts.concat(timeout),
      buttonHover: false,
    })
  }

  /**
   * This handles the clicking of course items from the side bar, pulling up
   * the corresponding course-info modal.
   * @param  {string} courseCode - the course code for the clicked course
   */
  handleCourseClick = courseCode => {
    this.setState({
      courseId: courseCode.substring(0, 6),
      showCourseModal: true,
    })
  }

  /**
   * Drawing mode not implemented, so this function may not work.
   */
  nodeMouseDown = event => {
    if (this.state.drawMode === "draw-node" && event.currentTarget.id.startsWith("n")) {
      this.setState({ draggingNode: event.currentTarget.id })
    }
  }

  /**
   * Drawing mode not implemented, so this function may not work.
   */
  drawMouseMove = event => {
    // in draw-node mode, drag a node as the mouse moves
    if (this.state.drawMode === "draw-node") {
      if (this.state.draggingNode !== null) {
        const newPos = this.getRelativeCoords(event)
        let currentNode
        for (const node of Object.values(this.state.nodesJSON)) {
          if (node.id_ === this.state.draggingNode) {
            currentNode = node
          }
        }
        currentNode.pos = [newPos.x - 20, newPos.y - 15]
        currentNode.text[0].pos = [newPos.x, newPos.y + 5]
        const newNodesJSON = { ...this.state.nodesJSON }
        newNodesJSON[currentNode.id_] = currentNode
        this.setState({ nodesJSON: newNodesJSON })
      }
    }
  }

  /**
   * Drawing mode not implemented, so this function may not work.
   */
  drawMouseUp = event => {
    // in draw-node mode, drop a dragged node to a new location
    if (this.state.drawMode === "draw-node") {
      if (this.state.draggingNode !== null) {
        const newPos = this.getRelativeCoords(event)
        let currentNode
        for (const node of Object.values(this.state.nodesJSON)) {
          if (node.id_ === this.state.draggingNode) {
            currentNode = node
          }
        }
        currentNode.pos = [newPos.x - 20, newPos.y - 15]
        currentNode.text[0].pos = [newPos.x, newPos.y + 5]
        const newNodesJSON = { ...this.state.nodesJSON }
        newNodesJSON[currentNode.id_] = currentNode
        this.setState({
          nodesJSON: newNodesJSON,
          draggingNode: null,
        })
      }
    }
  }

  /**
   * Initializes the panning process by recording the position of the mouse pointer or touch event.
   * Right now only support for one finger is implemented.
   * @param {Event} event
   */
  startPanning = event => {
    if (event.type === "mousedown") {
      this.setState({
        panning: true,
        panStartX: event.clientX + this.state.horizontalPanFactor,
        panStartY: event.clientY + this.state.verticalPanFactor,
      })
    } else {
      event.preventDefault()
      this.setState({
        panning: true,
        panStartX: event.touches[0].clientX + this.state.horizontalPanFactor,
        panStartY: event.touches[0].clientY + this.state.verticalPanFactor,
      })
    }
  }

  /**
   * Pans the graph by moving it in the direction that the mouse moved.
   * @param {Event} event
   */
  panGraph = event => {
    if (this.state.panning) {
      const currentX = event.clientX
      const currentY = event.clientY

      const deltaX = currentX - this.state.panStartX
      const deltaY = currentY - this.state.panStartY

      this.setState({
        horizontalPanFactor: -deltaX,
        verticalPanFactor: -deltaY,
      })
    }
  }

  /**
   * Stops the panning process.
   */
  stopPanning = () => {
    this.setState({
      panning: false,
      panStartX: 0,
      panStartY: 0,
    })
  }

  infoBoxMouseEnter = () => {
    this.clearAllTimeouts(TIMEOUT_NAMES_ENUM.INFOBOX)
    this.setState({ showInfoBox: true })
  }

  infoBoxMouseLeave = () => {
    const timeout = setTimeout(() => {
      this.setState({ showInfoBox: false })
    }, 400)

    this.setState({
      infoboxTimeouts: this.state.infoboxTimeouts.concat(timeout),
    })
  }

  infoBoxMouseClick = () => {
    const newCourse = this.state.infoBoxNodeId.substring(0, 6)
    this.setState({
      courseId: newCourse,
      showCourseModal: true,
    })
  }

  setShowGraphDropdown = () => {
    this.clearAllTimeouts(TIMEOUT_NAMES_ENUM.DROPDOWN)
    this.setState({ showGraphDropdown: true })
  }

  hideGraphDropdown = () => {
    const timeout = setTimeout(() => {
      this.setState({ showGraphDropdown: false })
    }, 500)
    this.setState({
      dropdownTimeouts: this.state.dropdownTimeouts.concat(timeout),
    })
  }

  onClose = () => {
    this.setState({ showCourseModal: false })
  }

  openExportModal = () => {
    this.exportModal.current.openModal()
  }

  // Reset graph
  reset = () => {
    this.props.setFCECount(0)
    const nodesStatus = Object.keys(this.state.nodesStatus).reduce((acc, curr) => {
      const state =
        this.state.connections.parents[curr].length === 0 ? "takeable" : "inactive"
      localStorage.setItem(curr, state)
      acc[curr] = {
        status: state,
        selected: false,
      }
      return acc
    }, {})

    const edgesStatus = Object.keys(this.state.edgesStatus).reduce(
      (acc, curr) => ((acc[curr] = "inactive"), acc),
      {}
    )

    const boolStatus = Object.keys(this.state.boolsStatus).reduce(
      (acc, curr) => ((acc[curr] = "inactive"), acc),
      {}
    )
    this.setState({
      boolsStatus: boolStatus,
      edgesStatus: edgesStatus,
      nodesStatus: nodesStatus,
      selectedNodes: new Set(),
    })
    if (this.state.currFocus !== null) {
      this.highlightFocuses([])
    }
  }

  renderArrowHead = () => {
    const polylineAttrs = { points: "0,1 10,5 0,9", fill: "black" }
    return (
      <defs>
        <marker
          id="arrowHead"
          viewBox="0 0 10 10"
          refX="4"
          refY="5"
          markerUnits="strokeWidth"
          markerWidth="7"
          markerHeight="7"
          orient="auto"
        >
          <polyline {...polylineAttrs} />
        </marker>
      </defs>
    )
  }

  /** Zoom into the graph by calculating new viewbox dimensions
   *
   * @param {number} zoomMode - Determines whether to zoom in, zoom out, or rerender at current zoom level
   */
  zoomViewbox = zoomMode => {
    let newZoomFactor = this.state.zoomFactor
    if (zoomMode === ZOOM_ENUM.ZOOM_IN) {
      newZoomFactor -= ZOOM_INCREMENT
    } else if (zoomMode === ZOOM_ENUM.ZOOM_OUT) {
      newZoomFactor += ZOOM_INCREMENT
    }
    this.setState({
      zoomFactor: newZoomFactor,
    })
  }

  calculateRatioGraphSizeToContainerSize = () => {
    const containerWidth = document.getElementById("react-graph").clientWidth
    const containerHeight = document.getElementById("react-graph").clientHeight
    const heightToContainerRatio = this.state.height / containerHeight
    const widthToContainerRatio = this.state.width / containerWidth
    return Math.max(heightToContainerRatio, widthToContainerRatio)
  }

  resetZoomAndPan = () => {
    this.setState({
      zoomFactor: 1,
      verticalPanFactor: 0,
      horizontalPanFactor: 0,
    })
  }

  onWheel = event => {
    const zoomIn = event.deltaY < 0
    if (zoomIn) {
      this.zoomViewbox(ZOOM_ENUM.ZOOM_IN)
    } else {
      this.zoomViewbox(ZOOM_ENUM.ZOOM_OUT)
    }
  }

  buttonMouseEnter = () => {
    this.setState({ buttonHover: true })
  }

  buttonMouseLeave = () => {
    this.setState({ buttonHover: false })
  }

  getRelativeCoords = event => {
    let x = event.nativeEvent.offsetX
    let y = event.nativeEvent.offsetY
    x = x * this.state.zoomFactor + this.state.horizontalPanFactor
    y = y * this.state.zoomFactor + this.state.verticalPanFactor
    return { x: x, y: y }
  }

  drawNode = (x, y) => {
    let xPos, yPos

    // if node would extend offscreen, instead place it at the
    // edge. Give 2 pixels extra for node border width.
    if (x + 42 > this.state.width) {
      xPos = this.state.width - 42
    } else if (x < 2) {
      xPos = 2
    } else {
      xPos = x
    }

    if (y + 34 > this.state.height) {
      yPos = this.state.height - 34
    } else if (y < 2) {
      yPos = 2
    } else {
      yPos = y
    }

    // text is an empty string for now until implementation,
    // text position uses node position for now
    const textJSON = {
      align: "begin",
      fill: "",
      graph: 0,
      pos: [xPos, yPos + 20],
      rId: "text" + this.state.drawNodeID,
      text: "la",
    }

    const nodeJSON = {
      fill: "#" + document.getElementById("select-colour").value,
      graph: 0,
      // default dimensions for a node
      height: 32,
      width: 40,
      id_: "n" + this.state.drawNodeID,
      pos: [xPos, yPos],
      stroke: "",
      text: [textJSON],
      tolerance: 9,
      type_: "Node",
    }

    const newNodesJSON = { ...this.state.nodesJSON }
    newNodesJSON[nodeJSON.id_] = nodeJSON
    this.setState({
      nodesJSON: newNodesJSON,
      drawNodeID: this.state.drawNodeID + 1,
    })
  }

  /**
    * In draw-node creates a new node at the position of the click event on the SVG canvas.
    * In path-mode creates an elbow at the position of the click event on the SVG canvas,
      if the startNode is defined.

      NOTE: Drawing mode is not fully implemented yet, so this method may not work as expected.
    * @param {object} e The mousedown event.
    */
  drawGraphObject = e => {
    const pos = this.getRelativeCoords(e)
    // check if the user is trying to draw a node. Also check
    // if the user is trying to press a button instead (ie zoom buttons)
    if (this.state.drawMode === "draw-node" && !this.state.buttonHover) {
      this.drawNode(pos.x, pos.y)
    }
  }

  highlightDependencies = dependencies => {
    this.setState({ highlightedNodesDeps: dependencies })
  }

  highlightFocuses = focuses => {
    this.setState({ highlightedNodesFocus: focuses })
  }

  /** Allows panning and entering draw mode via the keyboard.
   *
   * @param {KeyboardEvent} event
   */
  onKeyDown = event => {
    if (event.key === "ArrowRight") {
      this.setState({
        horizontalPanFactor:
          this.state.horizontalPanFactor - KEYBOARD_PANNING_INCREMENT,
      })
    } else if (event.key === "ArrowDown") {
      this.setState({
        verticalPanFactor: this.state.verticalPanFactor - KEYBOARD_PANNING_INCREMENT,
      })
    } else if (event.key === "ArrowLeft") {
      this.setState({
        horizontalPanFactor:
          this.state.horizontalPanFactor + KEYBOARD_PANNING_INCREMENT,
      })
    } else if (event.key === "ArrowUp") {
      this.setState({
        verticalPanFactor: this.state.verticalPanFactor + KEYBOARD_PANNING_INCREMENT,
      })
    } else if (event.key === "+") {
      this.zoomViewbox(ZOOM_ENUM.ZOOM_IN)
    } else if (event.key === "-") {
      this.zoomViewbox(ZOOM_ENUM.ZOOM_OUT)
    } else if (this.state.onDraw && event.key === "n") {
      this.setState({ drawMode: "draw-node" })
    }
  }

  /**
   * Update the Bool's state at any moment given the prereqs and current state.
   */
  updateNodeBool = boolId => {
    const newState = this.arePrereqsSatisfiedBool(boolId) ? "active" : "inactive"
    const childs = this.state.connections.children[boolId]
    const inEdges = this.state.connections.inEdges[boolId]
    const outEdges = this.state.connections.outEdges[boolId]

    this.setState(
      prevState => {
        const boolsStatus = { ...prevState.boolsStatus }
        boolsStatus[boolId] = newState
        return {
          boolsStatus: boolsStatus,
        }
      },
      () => {
        localStorage.setItem(boolId, newState)
        childs.forEach(node => {
          this.updateNode(node)
        })
        const allEdges = outEdges.concat(inEdges)
        allEdges.forEach(edge => {
          const currentEdge = this.state.edgesJSON[edge]
          this.updateEdgeStatus(
            undefined,
            currentEdge.id_,
            currentEdge.source,
            currentEdge.target
          )
        })
      }
    )
  }

  /**
   * Update the state/status of a node (and its children/edges).
   * @param  {boolean} recursive whether we should recurse on its children
   */
  updateNode = (nodeId, recursive) => {
    let newState
    if (this.arePrereqsSatisfiedNode(nodeId)) {
      if (this.isSelected(nodeId) || this.state.hybridsJSON[nodeId]) {
        newState = "active"
      } else {
        newState = "takeable"
      }
    } else {
      if (this.isSelected(nodeId) && !this.state.hybridsJSON[nodeId]) {
        newState = "overridden"
      } else {
        newState = "inactive"
      }
    }

    const childs = this.state.connections.children[nodeId]
    const status =
      this.state.nodesStatus[nodeId]?.status || this.state.boolsStatus[nodeId]

    // Updating the children will be unnecessary if the selected state of the current node has not
    // changed, and the original state was not 'missing'
    const allEdges =
      this.state.connections.inEdges[nodeId]?.concat(
        this.state.connections.outEdges[nodeId]
      ) || []

    if (
      ["active", "overridden"].includes(newState) &&
      ["active", "overridden"].includes(status) &&
      status !== "missing"
    ) {
      localStorage.setItem(nodeId, newState)

      this.setState(
        prevState => {
          if (nodeId in prevState.nodesStatus) {
            const nodesStatus = { ...prevState.nodesStatus }
            nodesStatus[nodeId].status = newState
            return { nodesStatus: nodesStatus }
          } else if (nodeId in prevState.boolsStatus) {
            const boolsStatus = { ...prevState.boolsStatus }
            boolsStatus[nodeId] = newState
            return { boolsStatus: boolsStatus }
          }
        },
        () => {
          allEdges.forEach(edge => {
            const currentEdge = this.state.edgesStatus[edge]
            this.updateEdgeStatus(
              undefined,
              currentEdge.id_,
              currentEdge.source,
              currentEdge.target
            )
          })
        }
      )
      return
    }

    if (recursive === undefined || recursive) {
      this.setState(
        prevState => {
          if (nodeId in prevState.nodesStatus) {
            const nodesStatus = {}
            Object.keys(prevState.nodesStatus).forEach(key => {
              nodesStatus[key] = { ...prevState.nodesStatus[key] }
            })
            nodesStatus[nodeId].status = newState
            return { nodesStatus: nodesStatus }
          } else if (nodeId in prevState.boolsStatus) {
            const boolsStatus = { ...prevState.boolsStatus }
            boolsStatus[nodeId] = newState
            return { boolsStatus: boolsStatus }
          }
        },
        () => {
          localStorage.setItem(nodeId, newState)
          childs?.forEach(n => {
            if (this.state.nodesStatus[n]) {
              this.updateNode(n)
            } else if (this.state.boolsJSON[n]) {
              this.updateNodeBool(n)
            }
          })
          allEdges.forEach(edge => {
            const currentEdge = this.state.edgesJSON[edge]
            this.updateEdgeStatus(
              undefined,
              currentEdge.id_,
              currentEdge.source,
              currentEdge.target
            )
          })
        }
      )
    } else {
      this.setState(
        prevState => {
          const nodesStatus = {}
          Object.keys(prevState.nodesStatus).forEach(key => {
            nodesStatus[key] = { ...prevState.nodesStatus[key] }
          })
          nodesStatus[nodeId].status = newState
          return { nodesStatus: nodesStatus }
        },
        () => {
          allEdges.forEach(edge => {
            const currentEdge = this.state.edgesJSON[edge]
            this.updateEdgeStatus(
              undefined,
              currentEdge.id_,
              currentEdge.source,
              currentEdge.target
            )
          })
        }
      )
      localStorage.setItem(nodeId, newState)
    }
  }

  /** Controls the selection and deselection of a node by switching states and updating the graph */
  toggleSelection = nodeId => {
    this.setState(
      prevState => {
        const nodesStatus = { ...prevState.nodesStatus }
        nodesStatus[nodeId].selected = !nodesStatus[nodeId].selected
        return { nodesStatus: nodesStatus }
      },
      () => {
        this.updateNode(nodeId)
      }
    )
  }

  /**
   * Cross check with the selected focus prerequisites.
   */
  focusPrereqsBool = boolId => {
    const status = this.state.boolsStatus[boolId]
    const inEdges = this.state.connections.inEdges[boolId]
    const parents = this.state.connections.parents[boolId]
    // Check if there are any missing prerequisites.
    if (status !== "active") {
      this.setState(
        prevState => {
          const boolsStatus = { ...prevState.boolsStatus }
          boolsStatus[boolId] = "missing"
          return {
            boolsStatus: boolsStatus,
          }
        },
        () => {
          inEdges?.forEach(edge => {
            const currentEdge = this.state.edgesJSON[edge]
            if (!this.isSelected(currentEdge.source)) {
              this.updateEdgeStatus(
                "missing",
                currentEdge.id_,
                currentEdge.source,
                currentEdge.target
              )
            }
          })
          parents?.forEach(node => {
            this.state.nodesStatus[node]
              ? this.focusPrereqs(node)
              : this.focusPrereqsBool(node)
          })
        }
      )
    }
  }
  /**
   * Remove the focus preqrequisites if the focus is unselected.
   */
  unfocusPrereqsBool = boolId => {
    this.updateNodeBool(boolId)
    const parents = this.state.connections.parents[boolId]
    parents.forEach(node => {
      this.state.nodesStatus[node]
        ? this.unfocusPrereqs(node)
        : this.unfocusPrereqsBool(node)
    })
  }

  /** Sets the status of all missing prerequisites to 'missing' */
  focusPrereqs = nodeId => {
    const status = this.state.nodesStatus[nodeId]?.status
    const inEdges = this.state.connections.inEdges[nodeId]
    const parents = this.state.connections.parents[nodeId]

    if (["inactive", "overridden", "takeable"].includes(status)) {
      this.setState(
        prevState => {
          const nodesStatus = {}
          Object.keys(prevState.nodesStatus).forEach(key => {
            nodesStatus[key] = { ...prevState.nodesStatus[key] }
          })
          nodesStatus[nodeId].status = "missing"
          return { nodesStatus: nodesStatus }
        },
        () => {
          inEdges?.forEach(edge => {
            const currentEdge = this.state.edgesJSON[edge]
            if (!this.isSelected(currentEdge.source)) {
              this.updateEdgeStatus(
                "missing",
                currentEdge.id_,
                currentEdge.source,
                currentEdge.target
              )
            }
          })
          parents?.forEach(node => {
            if (typeof node === "string") {
              this.state.nodesStatus[node]
                ? this.focusPrereqs(node)
                : this.focusPrereqsBool(node)
            } else {
              node.forEach(n => {
                this.state.nodesStatus[n]
                  ? this.focusPrereqs(n)
                  : this.focusPrereqsBool(n)
              })
            }
          })
          this.setState(prevState => ({
            highlightedNodesDeps: [...prevState.highlightedNodesDeps, nodeId],
          }))
        }
      )
    }
  }

  /**
   * Resets 'missing' nodes and edges to the previous statuses:
   *  active, inactive, overridden, takeable
   */
  unfocusPrereqs = nodeId => {
    this.highlightDependencies([])
    this.updateNode(nodeId, false)
    const parents = this.state.connections.parents[nodeId]
    const inEdges = this.state.connections.inEdges[nodeId]
    parents?.forEach(node => {
      if (typeof node === "string") {
        this.state.nodesStatus[node]
          ? this.unfocusPrereqs(node)
          : this.unfocusPrereqsBool(node)
      } else {
        node.forEach(n => {
          this.state.nodesStatus[n]
            ? this.unfocusPrereqs(n)
            : this.unfocusPrereqsBool(n)
        })
      }
    })
    inEdges?.forEach(edge => {
      if (this.state.edgesStatus[edge] === "missing") {
        const currentEdge = this.state.edgesJSON[edge]
        this.updateEdgeStatus(
          undefined,
          currentEdge.id_,
          currentEdge.source,
          currentEdge.target
        )
      }
    })
  }

  isSelected = nodeId => {
    if (this.state.nodesStatus[nodeId]) {
      return this.isSelectedNode(nodeId)
    } else {
      return this.isSelectedBool(nodeId)
    }
  }

  /**
   * Checks whether this Node is selected
   * @return {boolean}
   */
  isSelectedNode = nodeId => {
    if (this.state.nodesJSON[nodeId]) {
      return this.state.nodesStatus[nodeId].selected
    } else {
      return this.state.nodesStatus[nodeId].status === "active"
    }
  }

  /**
   * Check whether the Bool is selected.
   * @returns {boolean} Whether status is active or not.
   */
  isSelectedBool = boolId => {
    return this.state.boolsStatus[boolId] === "active"
  }

  /**
   * Check if the prerequisite courses have been satisfied based on bool type.
   * @returns {boolean} Whether any of the prereqs are satisfied.
   */
  arePrereqsSatisfiedBool = boolId => {
    const isAllTrue = element => {
      return this.state.nodesStatus[element]
        ? this.isSelected(element)
        : this.isSelected(element)
    }

    if (this.state.boolsJSON[boolId].text[0].text === "and") {
      return this.state.connections.parents[boolId].every(isAllTrue)
    } else if (this.state.boolsJSON[boolId].text[0].text === "or") {
      return this.state.connections.parents[boolId].some(isAllTrue)
    }
  }

  /**
   * Checks whether all prerequisite/preceding nodes for the current one are satisfied
   * @return {boolean}
   */
  arePrereqsSatisfiedNode = nodeId => {
    const parents = this.state.connections.parents[nodeId]
    /**
     * Recursively checks that preceding nodes are selected
     * @param  {string|Array} element Node(s)/other on the graph
     * @return {boolean}
     */
    const isAllTrue = element => {
      if (typeof element === "string") {
        if (this.state.nodesStatus[element] || this.state.boolsJSON[element]) {
          return this.isSelected(element)
        } else {
          return false
        }
      } else {
        return element.some(isAllTrue)
      }
    }

    return parents.every(isAllTrue)
  }

  /**
   * Renders a group of Bools
   * @param {JSON} boolsJSON
   * @param {object} boolsStatus
   * @param {object} connections
   * @return {JSX.Element}
   */
  renderBoolGroup = (boolsJSON, boolsStatus, connections) => {
    const generateBool = boolJSON => {
      const { parents } = connections
      return (
        <Bool
          JSON={boolJSON}
          className="bool"
          key={boolJSON.id_}
          parents={parents[boolJSON.id_]}
          logicalType={(boolJSON.text[0] && boolJSON.text[0].text) || "and"}
          inEdges={connections.inEdges[boolJSON.id_]}
          outEdges={connections.outEdges[boolJSON.id_]}
          status={boolsStatus[boolJSON.id_]}
        />
      )
    }

    return <g id="bools">{Object.values(boolsJSON).map(generateBool)}</g>
  }

  /**
   * Renders a group of Edges
   * @param {JSON} edgesJSON
   * @param {object} edgesStatus
   * @return {JSX.Element}
   */
  renderEdgeGroup = (edgesJSON, edgesStatus) => {
    const generateEdge = edgeJSON => {
      return (
        <Edge
          className="path"
          key={edgeJSON.id_}
          source={edgeJSON.source}
          target={edgeJSON.target}
          points={edgeJSON.points}
          status={edgesStatus[edgeJSON.id_]}
        />
      )
    }

    // Missing edges must be rendered last. The sort
    // method custom sorts a copy of edgesJSON so that all missing edges
    // are last in the list. Then render based on that list.
    const edges = Object.values(edgesJSON)
    const edgesCopy = [...edges]
    const state = edgesStatus
    edgesCopy.sort((a, b) => {
      // If an edge is missing, its edgeID should be in EdgeGroup's
      // state and its value should be true.
      const aID = a.id_
      const bID = b.id_
      let aMiss = false
      let bMiss = false
      aMiss = aID in state && state[aID]
      bMiss = bID in state && state[bID]
      if ((aMiss && bMiss) || (!aMiss && !bMiss)) {
        // a and b are equal
        return 0
      } else if (aMiss && !bMiss) {
        // sort a after b
        return 1
      } else if (!aMiss && bMiss) {
        // sort b after a
        return -1
      }
    })

    return <g id="edges">{edgesCopy.map(generateEdge)}</g>
  }

  /**
   * Renders a group of Nodes
   * @param {JSON} edgesJSON
   * @param {object} edgesStatus
   * @return {JSX.Element}
   */
  renderNodeGroup = (
    nodeClick,
    nodeMouseEnter,
    nodeMouseLeave,
    nodeMouseDown,
    onKeyDown,
    onWheel,
    nodesStatus,
    nodesJSON,
    hybridsJSON,
    highlightedNodesFocus,
    highlightedNodesDeps,
    connections,
    nodeDropshadowFilter
  ) => {
    return (
      <g id="nodes">
        {Object.values(hybridsJSON).map(entry => {
          return (
            <Node
              JSON={entry}
              className={"hybrid"}
              key={entry.id_}
              hybrid={true}
              parents={connections.parents[entry.id_]}
              childs={connections.children[entry.id_]}
              status={nodesStatus[entry.id_].status}
              onWheel={onWheel}
              onKeydown={onKeyDown}
              nodeDropshadowFilter={nodeDropshadowFilter}
            />
          )
        })}
        {Object.values(nodesJSON).map(entry => {
          // using `includes` to match "mat235" from "mat235237257calc2" and other math/stats courses
          const highlightFoc = highlightedNodesFocus.some(node =>
            entry.id_.includes(node)
          )
          const highlightDep = highlightedNodesDeps.some(node =>
            entry.id_.includes(node)
          )
          return (
            <Node
              JSON={entry}
              className="node"
              key={entry.id_}
              hybrid={false}
              parents={connections.parents[entry.id_]}
              status={nodesStatus[entry.id_].status}
              highlightDep={highlightDep}
              highlightFoc={highlightFoc}
              onClick={nodeClick}
              onMouseEnter={nodeMouseEnter}
              onMouseLeave={nodeMouseLeave}
              onMouseDown={nodeMouseDown}
              onWheel={onWheel}
              onKeydown={onKeyDown}
              nodeDropshadowFilter={nodeDropshadowFilter}
            />
          )
        })}
      </g>
    )
  }

  renderRegions = regionsJSON => {
    return Object.values(regionsJSON).map(function (entry, value) {
      const pathAttrs = { d: "M" }
      entry.points.forEach(function (x) {
        pathAttrs["d"] += x[0] + "," + x[1] + " "
      })

      const pathStyle = { fill: entry.fill }
      return <path {...pathAttrs} key={value} className="region" style={pathStyle} />
    })
  }

  renderLabels = labelsJSON => {
    return Object.values(labelsJSON).map(function (entry, value) {
      const textAttrs = {
        x: entry.pos[0],
        y: entry.pos[1],
      }

      const textStyle = { fill: entry.fill }

      return (
        <text
          {...textAttrs}
          key={value}
          style={textStyle}
          className="region-label"
          textAnchor={entry["text-anchor"]}
        >
          {entry["text"]}
        </text>
      )
    })
  }
  renderRegionsLabels(regionsJSON, labelsJSON) {
    return (
      <g id="regions">
        {this.renderRegions(regionsJSON)}
        {this.renderLabels(labelsJSON)}
      </g>
    )
  }

  render() {
    let containerWidth = 0
    let containerHeight = 0

    if (document.getElementById("react-graph") !== null) {
      const reactGraph = document.getElementById("react-graph")
      containerWidth = reactGraph.clientWidth
      containerHeight = reactGraph.clientHeight
    }

    let newViewboxHeight = this.state.height
    let newViewboxWidth = this.state.width
    if (document.getElementById("generateRoot") !== null) {
      newViewboxHeight =
        Math.max(this.state.height, containerHeight) * this.state.zoomFactor
      newViewboxWidth =
        Math.max(this.state.width, containerWidth) * this.state.zoomFactor
    } else {
      newViewboxWidth = this.state.width * this.state.zoomFactor
      newViewboxHeight = this.state.height * this.state.zoomFactor
    }

    const viewBoxContainerRatio =
      containerHeight !== 0 ? newViewboxHeight / containerHeight : 1
    const viewboxX =
      (this.state.width - newViewboxWidth) / 2 +
      this.state.horizontalPanFactor * viewBoxContainerRatio
    const viewboxY =
      (this.state.height - newViewboxHeight) / 2 +
      this.state.verticalPanFactor * viewBoxContainerRatio

    // not all of these properties are supported in React
    const svgAttrs = {
      height: "100%",
      width: "100%",
      viewBox: `${viewboxX} ${viewboxY} ${newViewboxWidth} ${newViewboxHeight}`,
      preserveAspectRatio: "xMidYMin",
      "xmlns:svg": "http://www.w3.org/2000/svg",
      "xmlns:dc": "http://purl.org/dc/elements/1.1/",
      "xmlns:cc": "http://creativecommons.org/ns#",
      "xmlns:rdf": "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    }

    const resetDisabled =
      this.state.zoomFactor === 1 &&
      this.state.horizontalPanFactor === 0 &&
      this.state.verticalPanFactor === 0

    // Mouse events for draw tool
    let svgMouseEvents = {}
    if (this.state.onDraw) {
      svgMouseEvents = {
        onMouseDown: this.drawGraphObject,
        onMouseUp: this.drawMouseUp,
        onMouseMove: this.drawMouseMove,
      }
    } else {
      svgMouseEvents = {
        onMouseDown: this.startPanning,
        onTouchStart: this.startPanning,
      }
    }

    const reactGraphPointerEvents = {
      onMouseMove: this.panGraph,
      onMouseUp: this.stopPanning,
      onTouchMove: this.panGraph,
      onTouchEnd: this.stopPanning,
      onWheel: this.onWheel,
    }

    let reactGraphClass = "react-graph"
    if (this.state.panning) {
      reactGraphClass += " panning"
    }
    if (this.state.highlightedNodesFocus.length > 0) {
      if (this.props.currFocus) {
        reactGraphClass += " highlight-nodes"
      }
    }

    return (
      <div
        id="react-graph"
        data-testid="react-graph"
        className={reactGraphClass}
        {...reactGraphPointerEvents}
      >
        {
          // Filtering by node.text.length is a temporary fix for a bug on Generate where the first node is empty
          Object.keys(this.state.nodesJSON).length > 1 && (
            <Sidebar
              fceCount={this.props.fceCount}
              reset={this.reset}
              activeCourses={this.state.selectedNodes}
              courses={Object.values(this.state.nodesJSON).map(node => [
                node.id_,
                node.text.length > 0 ? node.text[node.text.length - 1].text : "",
              ])}
              courseClick={this.handleCourseClick}
              xClick={this.nodeUnselect}
              sidebarItemClick={this.nodeClick}
              onHover={this.nodeMouseEnter}
              onMouseLeave={this.nodeMouseLeave}
            />
          )
        }
        <CourseModal
          showCourseModal={this.state.showCourseModal}
          courseId={this.state.courseId}
          onClose={this.onClose}
        />
        <ExportModal context="graph" session="" ref={this.exportModal} />
        <GraphDropdown
          showGraphDropdown={this.state.showGraphDropdown}
          onMouseMove={this.setShowGraphDropdown}
          onMouseLeave={this.hideGraphDropdown}
          graphs={this.props.graphs}
          updateGraph={this.props.updateGraph}
        />
        {Object.keys(this.state.nodesJSON).length > 1 && (
          <div className="graph-button-group">
            <div className="button-group">
              <Button
                text="+"
                mouseDown={() => this.zoomViewbox(ZOOM_ENUM.ZOOM_IN)}
                onMouseEnter={this.buttonMouseEnter}
                onMouseLeave={this.buttonMouseLeave}
              />
              <Button
                text="&ndash;"
                mouseDown={() => this.zoomViewbox(ZOOM_ENUM.ZOOM_OUT)}
                onMouseEnter={this.buttonMouseEnter}
                onMouseLeave={this.buttonMouseLeave}
              />
            </div>
            <div className="button-group">
              <Button
                divId="reset-view-button"
                mouseDown={this.resetZoomAndPan}
                onMouseEnter={this.buttonMouseEnter}
                onMouseLeave={this.buttonMouseLeave}
                disabled={resetDisabled}
              >
                <img
                  src="/static/res/ico/reset-view.png"
                  alt="Reset View"
                  title="Click to reset view"
                />
              </Button>
            </div>
          </div>
        )}

        <svg
          xmlns="http://www.w3.org/2000/svg"
          xmlnsXlink="http://www.w3.org/1999/xlink"
          {...svgAttrs}
          version="1.1"
          {...svgMouseEvents}
        >
          <filter id={this.nodeDropshadowFilter} height="130%">
            <feGaussianBlur in="SourceAlpha" stdDeviation="3" />
            <feOffset dx="2" dy="2" result="offsetblur" />
            <feComponentTransfer>
              <feFuncA type="linear" slope="0.8" />
            </feComponentTransfer>
            <feMerge>
              <feMergeNode />
              <feMergeNode in="SourceGraphic" />
            </feMerge>
          </filter>
          {this.renderArrowHead()}
          {this.renderRegionsLabels(this.state.regionsJSON, this.state.labelsJSON)}

          {this.renderBoolGroup(
            this.state.boolsJSON,
            this.state.boolsStatus,
            this.state.connections
          )}

          {this.renderEdgeGroup(this.state.edgesJSON, this.state.edgesStatus)}

          {this.renderNodeGroup(
            this.nodeClick,
            this.nodeMouseEnter,
            this.nodeMouseLeave,
            this.nodeMouseDown,
            this.onKeyDown,
            this.onWheel,
            this.state.nodesStatus,
            this.state.nodesJSON,
            this.state.hybridsJSON,
            this.state.highlightedNodesFocus,
            this.state.highlightedNodesDeps,
            this.state.connections,
            this.nodeDropshadowFilter
          )}

          <InfoBox
            onClick={this.infoBoxMouseClick}
            onMouseEnter={this.infoBoxMouseEnter}
            onMouseLeave={this.infoBoxMouseLeave}
            showInfoBox={this.state.showInfoBox}
            xPos={this.state.infoBoxXPos}
            yPos={this.state.infoBoxYPos}
            nodeId={this.state.infoBoxNodeId}
          />
        </svg>
      </div>
    )
  }
}

export { ZOOM_INCREMENT, KEYBOARD_PANNING_INCREMENT }

/** Helper function that adds parents of hybridNode to the parents object, and adds hybrid nodes as children of the Nodes they represent
 *
 * @param {Node} hybridNode
 * @param {Array} nodesJSON
 * @param {Object} parents
 * @param {Object} childrenObj
 */
export function populateHybridRelatives(hybridNode, nodesJSON, parents, childrenObj) {
  // parse prereqs based on text
  let hybridText = ""
  hybridNode.text.forEach(textTag => (hybridText += textTag.text))
  const nodeParents = []
  // First search for entire string (see Stats graph)
  let prereqNode = findRelationship(hybridText, nodesJSON)
  if (prereqNode !== undefined) {
    nodeParents.push(prereqNode.id_)
    childrenObj[prereqNode.id_].push(hybridNode.id_)
  } else {
    // Parse text first
    const prereqs = parseAnd(hybridText)[0]
    prereqs.forEach(course => {
      if (typeof course === "string") {
        prereqNode = findRelationship(course, nodesJSON)
        if (prereqNode !== undefined) {
          nodeParents.push(prereqNode.id_)
          childrenObj[prereqNode.id_].push(hybridNode.id_)
        } else {
          console.error("Could not find prereq for ", hybridText)
        }
      } else if (typeof course === "object") {
        const orPrereq = []
        course.forEach(c => {
          const prereqNode = findRelationship(c, nodesJSON)
          if (prereqNode !== undefined) {
            orPrereq.push(prereqNode.id_)
            childrenObj[prereqNode.id_].push(hybridNode.id_)
          } else {
            console.error("Could not find prereq for ", hybridText)
          }
        })
        if (orPrereq.length > 0) {
          nodeParents.push(orPrereq)
        }
      }
    })
  }
  parents[hybridNode.id_] = nodeParents
}

/**
 * Helper for hybrid computation. Finds the node with the same course label as the hybrid.
 * @param  {string} course
 * @param {Array} nodesJSON
 * @return {Node}
 */
export var findRelationship = (course, nodesJSON) => {
  const nodes = nodesJSON
  const node = nodes.find(
    n => n.type_ === "Node" && n.text.some(textTag => textTag.text.includes(course))
  )
  return node
}

Graph.propTypes = {
  currFocus: PropTypes.string,
  edit: PropTypes.bool,
  getLocalGraph: PropTypes.func,
  graphName: PropTypes.string,
  incrementFCECount: PropTypes.func,
  initialDrawMode: PropTypes.string,
  setFCECount: PropTypes.func,
  start_blank: PropTypes.bool,
  fceCount: PropTypes.number,
  graphs: PropTypes.array,
  updateGraph: PropTypes.func,
}

Graph.defaultProps = {
  currFocus: null,
  graphName: "",
  start_blank: false,
}
