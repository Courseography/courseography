import React from "react"
import PropTypes from "prop-types"
import { CourseModal } from "../common/react_modal.js.jsx"
import { ExportModal } from "../common/export.js.jsx"
import { getPost } from "../common/utils.js"
import BoolGroup from "./BoolGroup"
import Button from "./Button"
import EdgeGroup from "./EdgeGroup"
import InfoBox from "./InfoBox"
import NodeGroup from "./NodeGroup"
import GraphDropdown from "./GraphDropdown"
import Sidebar from "./Sidebar"
import { parseAnd } from "../../util/util.js"
import { refLookUp } from "../common/utils"

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
      labelsJSON: [],
      regionsJSON: [],
      nodesJSON: [],
      hybridsJSON: [],
      boolsJSON: [],
      edgesJSON: [],
      edgeMissingStatus: {},
      edgesStatus: {},
      highlightedNodes: [],
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

    this.nodes = React.createRef()
    this.bools = React.createRef()
    this.edges = React.createRef()
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
    document.body.removeEventListener("keydown", this.onKeyDown)
  }

  getGraph = () => {
    let graphName = this.props.graphName.replace("-", " ")
    let url = new URL("/get-json-data", document.location)
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
        var regionsList = []
        var nodesList = []
        var hybridsList = []
        var boolsList = []
        var edgesList = []
        var parentsObj = {}
        var inEdgesObj = {}
        var childrenObj = {}
        var outEdgesObj = {}
        var storedNodes = new Set()

        var labelsList = data.texts.filter(function (entry) {
          return entry.rId.startsWith("tspan")
        })

        data.shapes.forEach(function (entry) {
          if (entry.type_ === "Node") {
            nodesList.push(entry)
          } else if (entry.type_ === "Hybrid") {
            hybridsList.push(entry)
          } else if (entry.type_ === "BoolNode") {
            boolsList.push(entry)
          }
        })

        data.paths.forEach(function (entry) {
          if (entry.isRegion) {
            regionsList.push(entry)
          } else {
            edgesList.push(entry)
          }
        })

        // The duplicate filter is a temporary fix, as previously there were two nodes, Hybrid and Node,
        // that were placed in the same spot on some graphs where there should be only a Hybrid node.
        var noDuplicatesNodesList = []
        nodesList.forEach(node => {
          if (!(node.id_ in parentsObj)) {
            parentsObj[node.id_] = []
            inEdgesObj[node.id_] = []
            childrenObj[node.id_] = []
            outEdgesObj[node.id_] = []
            // Quickly adding any active nodes from local storage into the selected nodes
            if (localStorage.getItem(node.id_) === "active") {
              storedNodes.add(node.text[node.text.length - 1].text)
            }

            noDuplicatesNodesList.push(node)
          }
        })

        hybridsList.forEach(hybrid => {
          childrenObj[hybrid.id_] = []
          outEdgesObj[hybrid.id_] = []
          populateHybridRelatives(hybrid, nodesList, parentsObj, childrenObj)
        })

        edgesList.forEach(edge => {
          if (edge.target in parentsObj) {
            parentsObj[edge.target].push(edge.source)
            inEdgesObj[edge.target].push(edge.id_)
          }

          if (edge.source in childrenObj) {
            childrenObj[edge.source].push(edge.target)
            outEdgesObj[edge.source].push(edge.id_)
          }
        })

        boolsList.forEach(boolJSON => {
          var parents = []
          var childs = []
          var outEdges = []
          var inEdges = []
          edgesList.forEach(edge => {
            if (boolJSON.id_ === edge.target) {
              parents.push(edge.source)
              inEdges.push(edge.id_)
            } else if (boolJSON.id_ === edge.source) {
              childs.push(edge.target)
              outEdges.push(edge.id_)
            }
          })
          parentsObj[boolJSON.id_] = parents
          childrenObj[boolJSON.id_] = childs
          outEdgesObj[boolJSON.id_] = outEdges
          inEdgesObj[boolJSON.id_] = inEdges
        })
        const edgesStatus = edgesList.reduce(
          (acc, curr) => ((acc[curr.id_] = "inactive"), acc),
          {}
        )

        this.setState({
          labelsJSON: labelsList,
          regionsJSON: regionsList,
          nodesJSON: noDuplicatesNodesList,
          hybridsJSON: hybridsList,
          boolsJSON: boolsList,
          edgesJSON: edgesList,
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
      var totalFCEs = 0
      this.state.nodesJSON.forEach(nodeJSON => {
        let node = this.nodes.current[nodeJSON.id_]
        if (!node.props.hybrid && node.state.selected) {
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
      getPost(this.props.currFocus).then(focusData =>
        this.highlightFocuses(focusData.courseList)
      )
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

  updateEdgeMissingStatus = (edgeID, status) => {
    this.setState(state => {
      const temp = { ...state.edgeMissingStatus }
      temp[edgeID] = status
      return { edgeMissingStatus: temp }
    })
  }

  /**
   * Update the status of the Edge, this will take in a source, a target so it can be looked up
   * We will loop through the edgesJSON and find use the source, target directly from edgeJSON
   * the state will be stored and updated in edgeGroup object.
   */
  updateEdgeStatus = (source, target, status, edgeID) => {
    const sourceNode = refLookUp(source, this)
    const targetNode = refLookUp(target, this)
    if (sourceNode === undefined || targetNode === undefined) {
      return
    }
    if (!status) {
      if (!sourceNode.isSelected() && targetNode.state.status === "missing") {
        status = "missing"
      } else if (!sourceNode.isSelected()) {
        status = "inactive"
      } else if (!targetNode.isSelected()) {
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
    var courseId = event.currentTarget.id
    var currentNode = this.nodes.current[courseId]
    var courseLabelArray = currentNode.props.JSON.text
    var courseLabel = courseLabelArray[courseLabelArray.length - 1].text
    var wasSelected = currentNode.state.selected
    var temp = this.state.selectedNodes
    currentNode.toggleSelection(this)
    if (typeof this.props.incrementFCECount === "function") {
      if (wasSelected) {
        // TODO: Differentiate half- and full-year courses
        this.props.incrementFCECount(-0.5)
        temp.delete(courseLabel)
      } else {
        this.props.incrementFCECount(0.5)
        temp.add(courseLabel)
      }
    }
  }

  /**
   * Drawing mode is not implemented, meaning the onDraw defaults to false right now.
   */
  nodeMouseEnter = event => {
    var courseId = event.currentTarget.id
    var currentNode = this.nodes.current[courseId]
    currentNode.focusPrereqs(this)

    this.clearAllTimeouts(TIMEOUT_NAMES_ENUM.INFOBOX)

    var xPos = currentNode.props.JSON.pos[0]
    var yPos = currentNode.props.JSON.pos[1]
    var rightSide = xPos > 222
    // The tooltip is offset with a 'padding' of 5.
    if (rightSide) {
      xPos = parseFloat(xPos) - 65
    } else {
      xPos = parseFloat(xPos) + parseFloat(currentNode.props.JSON.width) + 5
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
    var courseId = event.currentTarget.id
    var currentNode = this.nodes.current[courseId]
    currentNode.unfocusPrereqs(this)

    var timeout = setTimeout(() => {
      this.setState({ showInfoBox: false })
    }, 400)

    this.setState({
      infoboxTimeouts: this.state.infoboxTimeouts.concat(timeout),
      buttonHover: false,
    })
  }

  /**
   * This handles clicking of dropdown items from the side bar search.
   * @param  {string} id
   */
  handleCourseClick = id => {
    id = id.toLowerCase()
    var currentNode = this.nodes.current[id]
    currentNode.toggleSelection(this)
    var courseLabelArray = currentNode.props.JSON.text
    var courseLabel = courseLabelArray[courseLabelArray.length - 1].text
    var temp = [...this.state.selectedNodes]
    if (currentNode.state.selected) {
      this.setState({
        selectedNodes: new Set(temp.filter(course => course !== courseLabel)),
      })
      this.props.incrementFCECount(-0.5)
    } else {
      this.setState({
        selectedNodes: new Set([...temp, courseLabel]),
      })
      this.props.incrementFCECount(0.5)
    }
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
        var newPos = this.getRelativeCoords(event)
        var currentNode
        for (var node of this.state.nodesJSON) {
          if (node.id_ === this.state.draggingNode) {
            currentNode = node
          }
        }
        currentNode.pos = [newPos.x - 20, newPos.y - 15]
        currentNode.text[0].pos = [newPos.x, newPos.y + 5]
        var newNodesJSON = [...this.state.nodesJSON]
        newNodesJSON.push(currentNode)
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
        var newPos = this.getRelativeCoords(event)
        var currentNode
        for (var node of this.state.nodesJSON) {
          if (node.id_ === this.state.draggingNode) {
            currentNode = node
          }
        }
        currentNode.pos = [newPos.x - 20, newPos.y - 15]
        currentNode.text[0].pos = [newPos.x, newPos.y + 5]
        var newNodesJSON = [...this.state.nodesJSON]
        newNodesJSON.push(currentNode)
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
      var currentX = event.clientX
      var currentY = event.clientY

      var deltaX = currentX - this.state.panStartX
      var deltaY = currentY - this.state.panStartY

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
    var timeout = setTimeout(() => {
      this.setState({ showInfoBox: false })
    }, 400)

    this.setState({
      infoboxTimeouts: this.state.infoboxTimeouts.concat(timeout),
    })
  }

  infoBoxMouseClick = () => {
    var newCourse = this.state.infoBoxNodeId.substring(0, 6)
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
    var timeout = setTimeout(() => {
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
    this.nodes.current.reset()
    this.bools.current.reset()
    this.edges.current.reset()
    this.setState({ selectedNodes: new Set() })
    if (this.state.currFocus !== null) {
      this.highlightFocuses([])
    }
  }

  renderArrowHead = () => {
    var polylineAttrs = { points: "0,1 10,5 0,9", fill: "black" }
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
    var newZoomFactor = this.state.zoomFactor
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
    var containerWidth = document.getElementById("react-graph").clientWidth
    var containerHeight = document.getElementById("react-graph").clientHeight
    var heightToContainerRatio = this.state.height / containerHeight
    var widthToContainerRatio = this.state.width / containerWidth
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
    let zoomIn = event.deltaY < 0
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
    var x = event.nativeEvent.offsetX
    var y = event.nativeEvent.offsetY
    x = x * this.state.zoomFactor + this.state.horizontalPanFactor
    y = y * this.state.zoomFactor + this.state.verticalPanFactor
    return { x: x, y: y }
  }

  drawNode = (x, y) => {
    var xPos, yPos

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
    var textJSON = {
      align: "begin",
      fill: "",
      graph: 0,
      pos: [xPos, yPos + 20],
      rId: "text" + this.state.drawNodeID,
      text: "la",
    }

    var nodeJSON = {
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

    var newNodesJSON = [...this.state.nodesJSON]
    newNodesJSON.push(nodeJSON)
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
    var pos = this.getRelativeCoords(e)
    // check if the user is trying to draw a node. Also check
    // if the user is trying to press a button instead (ie zoom buttons)
    if (this.state.drawMode === "draw-node" && !this.state.buttonHover) {
      this.drawNode(pos.x, pos.y)
    }
  }

  highlightFocuses = focuses => {
    this.setState({ highlightedNodes: focuses })
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

  renderRegions = regionsJSON => {
    return regionsJSON.map(function (entry, value) {
      var pathAttrs = { d: "M" }
      entry.points.forEach(function (x) {
        pathAttrs["d"] += x[0] + "," + x[1] + " "
      })

      var pathStyle = { fill: entry.fill }
      return <path {...pathAttrs} key={value} className="region" style={pathStyle} />
    })
  }

  renderLabels = labelsJSON => {
    return labelsJSON.map(function (entry, value) {
      var textAttrs = {
        x: entry.pos[0],
        y: entry.pos[1],
      }

      var textStyle = { fill: entry.fill }

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
      let reactGraph = document.getElementById("react-graph")
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
    var svgAttrs = {
      height: "100%",
      width: "100%",
      viewBox: `${viewboxX} ${viewboxY} ${newViewboxWidth} ${newViewboxHeight}`,
      preserveAspectRatio: "xMidYMin",
      "xmlns:svg": "http://www.w3.org/2000/svg",
      "xmlns:dc": "http://purl.org/dc/elements/1.1/",
      "xmlns:cc": "http://creativecommons.org/ns#",
      "xmlns:rdf": "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    }

    var resetDisabled =
      this.state.zoomFactor === 1 &&
      this.state.horizontalPanFactor === 0 &&
      this.state.verticalPanFactor === 0

    // Mouse events for draw tool
    var svgMouseEvents = {}
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

    var reactGraphPointerEvents = {
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
    if (this.state.highlightedNodes.length > 0) {
      reactGraphClass += " highlight-nodes"
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
          this.state.nodesJSON.length > 1 && (
            <Sidebar
              fceCount={this.props.fceCount}
              reset={this.reset}
              activeCourses={this.state.selectedNodes}
              courses={this.state.nodesJSON.map(node => [
                node.id_,
                node.text.length > 0 ? node.text[node.text.length - 1].text : "",
              ])}
              courseClick={this.handleCourseClick}
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
        {this.state.nodesJSON.length > 1 && (
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
          <NodeGroup
            ref={this.nodes}
            nodeClick={this.nodeClick}
            nodeMouseEnter={this.nodeMouseEnter}
            nodeMouseLeave={this.nodeMouseLeave}
            nodeMouseDown={this.nodeMouseDown}
            svg={this}
            nodesJSON={this.state.nodesJSON}
            hybridsJSON={this.state.hybridsJSON}
            edgesJSON={this.state.edgesJSON}
            highlightedNodes={this.state.highlightedNodes}
            onDraw={this.state.onDraw}
            connections={this.state.connections}
            nodeDropshadowFilter={this.nodeDropshadowFilter}
          />
          <BoolGroup
            ref={this.bools}
            boolsJSON={this.state.boolsJSON}
            edgesJSON={this.state.edgesJSON}
            connections={this.state.connections}
            svg={this}
          />
          <EdgeGroup
            svg={this}
            ref={this.edges}
            edgesJSON={this.state.edgesJSON}
            edgeMissingStatus={this.state.edgeMissingStatus}
            updateEdgeMissingStatus={this.updateEdgeMissingStatus}
            edgesStatus={this.state.edgesStatus}
            updateEdgeStatus={this.updateEdgeStatus}
          />
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
  var hybridText = ""
  hybridNode.text.forEach(textTag => (hybridText += textTag.text))
  var nodeParents = []
  // First search for entire string (see Stats graph)
  var prereqNode = findRelationship(hybridText, nodesJSON)
  if (prereqNode !== undefined) {
    nodeParents.push(prereqNode.id_)
    childrenObj[prereqNode.id_].push(hybridNode.id_)
  } else {
    // Parse text first
    var prereqs = parseAnd(hybridText)[0]
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
        var orPrereq = []
        course.forEach(c => {
          var prereqNode = findRelationship(c, nodesJSON)
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
  var nodes = nodesJSON
  var node = nodes.find(
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
