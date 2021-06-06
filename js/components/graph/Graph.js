import React from "react";
import PropTypes from "prop-types";
import { CourseModal } from "../common/react_modal.js.jsx";
import { ExportModal } from "../common/export.js.jsx";
import BoolGroup from "./BoolGroup";
import Button from "./Button";
import EdgeGroup from "./EdgeGroup";
import InfoBox from "./InfoBox";
import NodeGroup from "./NodeGroup";
import RegionGroup from "./RegionGroup";
import * as focusInfo from "./sidebar/focus_descriptions";

export default class Graph extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      labelsJSON: [],
      regionsJSON: [],
      nodesJSON: [],
      hybridsJSON: [],
      boolsJSON: [],
      edgesJSON: [],
      highlightedNodes: [],
      timeouts: [],
      fceCount: 0,
      width: window.innerWidth,
      height: window.innerHeight,
      zoomFactor: 1,
      mouseDown: false,
      buttonHover: false,
      onDraw: this.props.edit,
      drawMode: this.props.initialDrawMode,
      drawNodeID: 0,
      draggingNode: null,
      currFocus: null,
      graphName: null,
      connections: null,
      showInfoBox: false,
      infoBoxXPos: 0,
      infoBoxYPos: 0,
      infoBoxNodeId: "",
      panning: false,
      panStartX: 0,
      panStartY:0,
      mouseTimeoutID:false,
      viewBoxPos: {x:0, y:0},
      viewBoxDim: {width:window.innerWidth, height:window.innerHeight},
      viewboxContainerRatio:1,
      showCourseModal: false
    };

    this.nodes = React.createRef();
    this.bools = React.createRef();
    this.edges = React.createRef();
    this.exportModal = React.createRef();
    this.zoomIncrement = 0.010;
  }

  componentDidMount() {
    if (!this.props.start_blank) {
      this.getGraph();
    }

    // can't detect keydown event when adding event listener to react-graph
    document.body.addEventListener("keydown", this.onKeyDown);
    document
      .getElementById("react-graph")
      .addEventListener("wheel", this.onWheel);

    // Enable "Export" link
    if (document.getElementById("nav-export")) {
      document.getElementById("nav-export")
        .addEventListener("click", this.exportModal.current.openModal);
    }
  }

  componentWillUpdate(prevProps) {
    if (this.state.currFocus !== prevProps.currFocus) {
      this.setState({ currFocus: prevProps.currFocus }, () => {
        let focuses = this.state.currFocus === null ? [] : focusInfo[this.state.currFocus + "FocusList"];
        this.highlightFocuses(focuses);
      });
    }
    if (!!this.state.graphName && this.state.graphName !== prevProps.graphName) {
      this.getGraph();
    }
  }

  componentWillUnmount() {
    document.body.removeEventListener("keydown", this.onKeyDown);
    document
      .getElementById("react-graph")
      .removeEventListener("wheel", this.onWheel);
  }

  getGraph = () => {
    let graphName = this.props.graphName.replace("-", " ");
    let url = new URL("/get-json-data", document.location);
    const params = { graphName: graphName };
    Object.keys(params).forEach(key =>
      url.searchParams.append(key, params[key])
    );

    fetch(url)
      .then(headers => {
        if (!headers.ok) {
          // can't just return res
          const headerInfo = {
            status: headers.status,
            statusText: headers.statusText,
            type: headers.type,
            url: headers.url
          };
          throw new Error(
            "When fetching from the url with info " + JSON.stringify(headerInfo)
          );
        }
        return headers.json(); // only received headers, waiting for data
      })
      .then(data => {
        localStorage.setItem("active-graph", graphName);
        var regionsList = [];
        var nodesList = [];
        var hybridsList = [];
        var boolsList = [];
        var edgesList = [];
        var parentsObj = {};
        var inEdgesObj = {};
        var childrenObj = {};
        var outEdgesObj = {};

        var labelsList = data.texts.filter(function(entry) {
          return entry.rId.startsWith("tspan");
        });

        data.shapes.forEach(function(entry) {
          if (entry.type_ === "Node") {
            nodesList.push(entry);
          } else if (entry.type_ === "Hybrid") {
            hybridsList.push(entry);
          } else if (entry.type_ === "BoolNode") {
            boolsList.push(entry);
          }
        });

        data.paths.forEach(function(entry) {
          if (entry.isRegion) {
            regionsList.push(entry);
          } else {
            edgesList.push(entry);
          }
        });

        nodesList.forEach(node => {
          parentsObj[node.id_] = [];
          inEdgesObj[node.id_] = [];
          childrenObj[node.id_] = [];
          outEdgesObj[node.id_] = [];
        });

        edgesList.forEach(edge => {
          if (edge.target in parentsObj) {
            parentsObj[edge.target].push(edge.source);
            inEdgesObj[edge.target].push(edge.id_);
          }

          if (edge.source in childrenObj) {
            childrenObj[edge.source].push(edge.target);
            outEdgesObj[edge.source].push(edge.id_);
          }
        });

        this.setState({
          labelsJSON: labelsList,
          regionsJSON: regionsList,
          nodesJSON: nodesList,
          hybridsJSON: hybridsList,
          boolsJSON: boolsList,
          edgesJSON: edgesList,
          width: data.width,
          height: data.height,
          zoomFactor: 1,
          viewBoxPos: {x:0, y:0},
          graphName: graphName,
          connections: {
            'parents': parentsObj,
            'inEdges': inEdgesObj,
            'children': childrenObj,
            'outEdges': outEdgesObj
          }
        });
      })
      .catch(err => {
        console.error("Fetch API failed. Here are the headers: ");
        console.error(err);
      });
  };

  componentDidUpdate(prevProps, prevState) {
    if (prevState.nodesJSON !== this.state.nodesJSON) {
      var totalFCEs = 0;
      this.state.nodesJSON.forEach(nodeJSON => {
        let node = this.nodes.current[nodeJSON.id_];
        if (!node.props.hybrid && node.state.selected) {
          totalFCEs += 0.5;
        }
      });
      if (this.props.setFCECount) {
        this.props.setFCECount(totalFCEs);
      }
    }
  }

  clearAllTimeouts = () => {
    for (var i = 0; i < this.state.timeouts.length; i++) {
      clearTimeout(this.state.timeouts[i]);
    }

    this.setState({ timeouts: [] });
  };

  nodeClick = event => {
    var courseId = event.currentTarget.id;
    var currentNode = this.nodes.current[courseId];
    var wasSelected = currentNode.state.selected;
    currentNode.toggleSelection(this);
    if (typeof this.props.incrementFCECount === 'function') {
      if (wasSelected) {
        // TODO: Differentiate half- and full-year courses
        this.props.incrementFCECount(-0.5);
      } else {
        this.props.incrementFCECount(0.5);
      }
    }
  };

  nodeMouseEnter = event => {
    var courseId = event.currentTarget.id;
    var currentNode = this.nodes.current[courseId];
    currentNode.focusPrereqs(this);

    this.clearAllTimeouts();

    var xPos = currentNode.props.JSON.pos[0];
    var yPos = currentNode.props.JSON.pos[1];
    var rightSide = xPos > 222;
    // The tooltip is offset with a 'padding' of 5.
    if (rightSide) {
      xPos = parseFloat(xPos) - 65;
    } else {
      xPos = parseFloat(xPos) + parseFloat(currentNode.props.JSON.width) + 5;
    }

    yPos = parseFloat(yPos);

    if (!this.state.onDraw) {
      this.setState({
        showInfoBox: true,
        infoBoxXPos: xPos,
        infoBoxYPos: yPos,
        infoBoxNodeId:courseId
      });
    }
    this.setState({ buttonHover: true });
  };

  nodeMouseLeave = event => {
    var courseId = event.currentTarget.id;
    var currentNode = this.nodes.current[courseId];
    currentNode.unfocusPrereqs(this);

    var timeout = setTimeout(() => {
      this.setState({showInfoBox: false});
    }, 400);

    this.setState({
      timeouts: this.state.timeouts.concat(timeout),
      buttonHover: false
    });
  };

  nodeMouseDown = event => {
    if (
      this.state.drawMode === "draw-node" &&
      event.currentTarget.id.startsWith("n")
    ) {
      this.setState({ draggingNode: event.currentTarget.id });
    }
  };

  drawMouseMove = event => {
    // in draw-node mode, drag a node as the mouse moves
    if (this.state.drawMode === "draw-node") {
      if (this.state.draggingNode !== null) {
        var newPos = this.getRelativeCoords(event);
        var currentNode;
        for (var node of this.state.nodesJSON) {
          if (node.id_ === this.state.draggingNode) {
            currentNode = node;
          }
        }
        currentNode.pos = [newPos.x - 20, newPos.y - 15];
        currentNode.text[0].pos = [newPos.x, newPos.y + 5];
        var newNodesJSON = [...this.state.nodesJSON];
        newNodesJSON.push(currentNode);
        this.setState({ nodesJSON: newNodesJSON });
      }
    }
  };

  drawMouseUp = event => {
    // in draw-node mode, drop a dragged node to a new location
    if (this.state.drawMode === "draw-node") {
      if (this.state.draggingNode !== null) {
        var newPos = this.getRelativeCoords(event);
        var currentNode;
        for (var node of this.state.nodesJSON) {
          if (node.id_ === this.state.draggingNode) {
            currentNode = node;
          }
        }
        currentNode.pos = [newPos.x - 20, newPos.y - 15];
        currentNode.text[0].pos = [newPos.x, newPos.y + 5];
        var newNodesJSON = [...this.state.nodesJSON];
        newNodesJSON.push(currentNode);
        this.setState({
          nodesJSON: newNodesJSON,
          draggingNode: null
        });
      }
    }
  };

  adjustCoordsToViewbox = (clientX, clientY) =>{
    return {adjustedX: clientX + this.state.viewBoxPos.x, adjustedY: clientY + this.state.viewBoxPos.y};
  }

  mouseDown = event => {
    // maybe need to add listener stuff for mouseUp?
    document.body.style.cursor = "grab";
    console.log(`mouse down: startX:${event.clientX}, startY:${event.clientY}`);
    const {adjustedX, adjustedY} = this.adjustCoordsToViewbox(event.clientX, event.clientY);
    this.setState({
      panning: true,
      panStartX: adjustedX,
      panStartY: adjustedY
    });

  }

  mouseMove = event => {
      if (this.state.panning){

        var currentX = event.clientX;
        var currentY = event.clientY;

        var deltaX = currentX - this.state.panStartX;
        var deltaY = currentY - this.state.panStartY;
        this.setState({
          viewBoxPos: {x: -deltaX * this.state.viewboxContainerRatio, y: -deltaY * this.state.viewboxContainerRatio},
        });

      }
  }

  mouseUp = () =>{
    console.log("mouse up");
    document.body.style.cursor = "auto"
    this.setState({
      panning:false,
      panStartX:0,
      panStartY:0
    })
  }

  infoBoxMouseEnter = () => {
    this.clearAllTimeouts();
    this.setState({showInfoBox: true});
  };

  infoBoxMouseLeave = () => {
    var timeout = setTimeout(() => {
      this.setState({showInfoBox: false});
    }, 400);

    this.setState({ timeouts: this.state.timeouts.concat(timeout) });
  };

  infoBoxMouseClick = () => {
    var newCourse = this.state.infoBoxNodeId.substring(0, 6);
    this.setState({
      courseId: newCourse,
      showCourseModal: true
    });
  };

  onClose = () => {
    this.setState({ showCourseModal: false });
  }

  openExportModal = () => {
    this.exportModal.current.openModal();
  };

  // Reset graph
  reset = () => {
    this.props.setFCECount(0);
    this.nodes.current.reset();
    this.bools.current.reset();
    this.edges.current.reset();
    if (this.state.currFocus !== null) {
      this.highlightFocuses([]);
    }
  }

  renderArrowHead = () => {
    var polylineAttrs = { points: "0,1 10,5 0,9", fill: "black" };
    return (
      <defs>
        <marker id="arrowHead" viewBox="0 0 10 10" refX="4" refY="5"
            markerUnits="strokeWidth" markerWidth="7" markerHeight="7"
            orient="auto">
          <polyline {...polylineAttrs} />
        </marker>
      </defs>
    );
  };

  zoomViewbox = (deltaY, containerWidth, containerHeight) => {
    clearTimeout(this.state.mouseTimeoutID);
    var newZoomFactor = this.state.zoomFactor;
    if (deltaY < 0){
      document.body.style.cursor = "zoom-in";
      newZoomFactor -= this.zoomIncrement;
    } else if (deltaY > 0){
      document.body.style.cursor = "zoom-out";
      newZoomFactor += this.zoomIncrement;
    }
    const newViewboxWidth = Math.max(this.state.width, containerWidth) * newZoomFactor;
    const newViewboxHeight = Math.max(this.state.height, containerHeight) * newZoomFactor;
    const ratio = containerWidth!=0 ? newViewboxWidth / containerWidth: 1;

    var timeoutID = setTimeout(() => {
      document.body.style.cursor = "auto";
    }, 250);
    this.setState({
      mouseTimeoutID: timeoutID,
      viewBoxDim: {width:newViewboxWidth, height:newViewboxHeight},
      viewboxContainerRatio: ratio,
      zoomFactor: newZoomFactor
    })
  }

  calculateRatioGraphSizeToContainerSize = () => {
    var containerWidth = document.getElementById("react-graph").clientWidth;
    var containerHeight = document.getElementById("react-graph").clientHeight;
    var heightToContainerRatio = this.state.height / containerHeight;
    var widthToContainerRatio = this.state.width / containerWidth;
    return Math.max(heightToContainerRatio, widthToContainerRatio);
  };

  resetZoomAndPan = () => {
    this.setState({
      zoomFactor: 1,
      viewBoxPos: {x:0, y:0},
    });
  };

  onWheel = event => {
    let containerWidth = 0;
    let containerHeight = 0;

    if (document.getElementById("react-graph") !== null){
      var reactGraph = document.getElementById("react-graph");
      containerWidth = reactGraph.clientWidth;
      containerHeight = reactGraph.clientHeight;
    }
    this.zoomViewbox(event.deltaY, containerWidth, containerHeight);
  };

  buttonMouseEnter = () => {
    this.setState({ buttonHover: true });
  };

  buttonMouseLeave = () => {
    this.setState({ buttonHover: false });
  };

  getRelativeCoords = event => {
    var x = event.nativeEvent.offsetX;
    var y = event.nativeEvent.offsetY;
    x = x * this.state.zoomFactor + this.state.viewBoxPos.x;
    y = y * this.state.zoomFactor + this.state.viewBoxPos.y;
    return { x: x, y: y };
  };

  drawNode = (x, y) => {
    var xPos, yPos;

    // if node would extend offscreen, instead place it at the
    // edge. Give 2 pixels extra for node border width.
    if (x + 42 > this.state.width) {
      xPos = this.state.width - 42;
    } else if (x < 2) {
      xPos = 2;
    } else {
      xPos = x;
    }

    if (y + 34 > this.state.height) {
      yPos = this.state.height - 34;
    } else if (y < 2) {
      yPos = 2;
    } else {
      yPos = y;
    }

    // text is an empty string for now until implementation,
    // text position uses node position for now
    var textJSON = {
      align: "begin",
      fill: "",
      graph: 0,
      pos: [xPos, yPos + 20],
      rId: "text" + this.state.drawNodeID,
      text: "la"
    };

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
      type_: "Node"
    };

    var newNodesJSON = [...this.state.nodesJSON];
    newNodesJSON.push(nodeJSON);
    this.setState({
      nodesJSON: newNodesJSON,
      drawNodeID: this.state.drawNodeID + 1
    });
  };

  /**
    * In draw-node creates a new node at the position of the click event on the SVG canvas.
    * In path-mode creates an elbow at the position of the click event on the SVG canvas,
      if the startNode is defined.
    * @param {object} e The mousedown event.
    */
  drawGraphObject = e => {
    var pos = this.getRelativeCoords(e);
    // check if the user is trying to draw a node. Also check
    // if the user is trying to press a button instead (ie zoom buttons)
    if (this.state.drawMode === "draw-node" && !this.state.buttonHover) {
      this.drawNode(pos.x, pos.y);
    }
  };

  highlightFocuses = focuses => {
    this.setState({ highlightedNodes: focuses });
  }

  render() {
    // not all of these properties are supported in React
    var svgAttrs = {
      width: "100%",
      height: "100%",
      viewBox: `${this.state.viewBoxPos.x} ${this.state.viewBoxPos.y} ${this.state.viewBoxDim.width} ${this.state.viewBoxDim.height}`,
      preserveAspectRatio: "xMinYMin",
      "xmlns:svg": "http://www.w3.org/2000/svg",
      "xmlns:dc": "http://purl.org/dc/elements/1.1/",
      "xmlns:cc": "http://creativecommons.org/ns#",
      "xmlns:rdf": "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    };

    var resetDisabled =
      this.state.zoomFactor === 1 &&
      this.state.viewboxX === 0 &&
      this.state.viewboxY === 0;

    // Mouse events for draw tool
    var mouseEvents = {};
    if (this.state.onDraw) {
      mouseEvents = {
        onMouseDown: this.drawGraphObject,
        onMouseUp: this.drawMouseUp,
        onMouseMove: this.drawMouseMove
      };
    }
    else {
      mouseEvents = {
        onMouseDown: this.mouseDown,
        onMouseUp: this.mouseUp,
        onMouseMove: this.mouseMove
      }
    }

    return (
      <div id="react-graph" className="react-graph" onClick={this.props.closeSidebar}>
        <CourseModal showCourseModal={this.state.showCourseModal} courseId={this.state.courseId} onClose={this.onClose} />
        <ExportModal context="graph" session="" ref={this.exportModal} />
        <Button
          divId="reset-button"
          text="Reset View"
          mouseDown={this.resetZoomAndPan}
          mouseUp={this.onButtonRelease}
          onMouseEnter={this.buttonMouseEnter}
          onMouseLeave={this.buttonMouseLeave}
          disabled={resetDisabled}
        />

        <svg
          xmlns="http://www.w3.org/2000/svg"
          xmlnsXlink="http://www.w3.org/1999/xlink"
          {...svgAttrs}
          version="1.1"
          className={
            this.state.highlightedNodes.length > 0 ? "highlight-nodes" : ""
          }
          {...mouseEvents}
        >
          {this.renderArrowHead()}
          <RegionGroup
            regionsJSON={this.state.regionsJSON}
            labelsJSON={this.state.labelsJSON}
          />
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
          />
          <BoolGroup
            ref={this.bools}
            boolsJSON={this.state.boolsJSON}
            edgesJSON={this.state.edgesJSON}
            svg={this}
          />
          <EdgeGroup
            svg={this}
            ref={this.edges}
            edgesJSON={this.state.edgesJSON}
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
    );
  }
}

Graph.propTypes = {
  closeSidebar: PropTypes.func,
  currFocus: PropTypes.string,
  edit: PropTypes.bool,
  getLocalGraph: PropTypes.func,
  graphName: PropTypes.string,
  incrementFCECount: PropTypes.func,
  initialDrawMode: PropTypes.string,
  setFCECount: PropTypes.func,
  start_blank: PropTypes.bool
};

Graph.defaultProps = {
  currFocus: null,
  graphName: '',
  start_blank: false
};