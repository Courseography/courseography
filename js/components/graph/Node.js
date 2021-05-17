import PropTypes from "prop-types";
import React from "react";
import { refLookUp } from "../common/utils";

/** React component class representing a Node on the graph */
export default class Node extends React.Component {
  /** Create a node */
  constructor(props) {
    super(props);
    var state = localStorage.getItem(this.props.JSON.id_);

    /**
     * Unselected states:
     *  - takeable: all prerequisites are satisfied (or no prereqs/parents)
     *  - inactive: missing some prerequisites
     * 
     * Selected states:
     *  - active: all prerequisites are satisfied
     *  - overridden: missing some prerequisites (will have a red border)
     */

    if (this.props.editMode) {
      state = ""; // TODO: define what editMode is
    } else if (state === null) {
      // sets state to takeable if it has no parent nodes (prereqs)
      state = this.props.parents.length === 0 ? "takeable" : "inactive";
    }

    this.state = {
      status: state,
      selected: ["active", "overridden"].indexOf(state) >= 0 // checks if state means its selected
    };
  }

  /**
   * Checks whether this Node is selected
   * @return {boolean}
   */
  isSelected = () => {
    // Hybrid nodes: the grey, smaller nodes representing another node farther way
    // They can only be either active or inactive.
    if (this.props.hybrid) {
      return this.state.status === "active";
    } else {
      return this.state.selected;
    }
  }

  /**
   * Checks whether all prerequisite/ preceeding nodes for the current one are satisfied
   * @return {boolean}
   */
  arePrereqsSatisfied = () => {
    var svg = this.props.svg;  // contains information about the whole graph

    /**
     * Recursively checks that preceeding nodes are selected
     * @param  {string|Array} element Node(s)/ other on the graph
     * @return {boolean}
     */
    function isAllTrue(element) {
      // base case: single node
      if (typeof element === "string") {
        // if the element represents a defined node, return whether it is a selected
        if (svg.nodes.current[element] !== undefined) {
          return svg.nodes.current[element].isSelected();
        } else if (svg.bools.current[element] !== undefined) { // else if it is a defined Bool
          return svg.bools.current[element].isSelected();
        } else {
          return false;
        }
      } else {
        // check if at least one node is selected from element
        return element.some(isAllTrue);
      }
    }

    // checks if parent nodes are selected
    return this.props.parents.every(isAllTrue);
  }

  /**
   * Update the state/status of a node (and its children/edges)
   * @param  {boolean} recursive whether we should recurse on its children
   */
  updateNode = recursive => {
    var newState;

    // if the prereqs are satisfied, the node is takeable. Once selected it becomes active.
    if (this.arePrereqsSatisfied()) {
      if (this.isSelected() || this.props.hybrid) {
        newState = "active";
      } else {
        newState = "takeable";
      }
    } else {
      // if they are not satisfied, the node is inactive. Once selected it becomes overridden.
      if (this.isSelected() && !this.props.hybrid) {
        newState = "overridden";
      } else {
        newState = "inactive";
      }
    }

    var nodeId = this.props.JSON.id_;

    // Check whether need to update children
    if (
      ["active", "overridden"].indexOf(newState) >= 0 ===
        ["active", "overridden"].indexOf(this.state.status) >= 0 &&
      this.state.status !== "missing"
    ) {
      localStorage.setItem(nodeId, newState);
      this.setState({ status: newState });
      return;
    }

    // update the children if needed
    if (recursive === undefined || recursive) {
      var svg = this.props.svg;  // contains information about the whole graph

      this.setState({ status: newState }, function() {
        localStorage.setItem(nodeId, newState);
        // update children nodes
        this.props.childs.forEach(function(node) {
          var currentNode = refLookUp(node, svg);
          if (currentNode !== undefined) {
            currentNode.updateNode();
          }
        });
        // update edges
        var allEdges = this.props.outEdges.concat(this.props.inEdges);
        allEdges.forEach(edge => {
          var currentEdge = svg.edges.current[edge];
          if (currentEdge !== undefined) {
            currentEdge.updateStatus();
          }
        });
      });
    } else {
      this.setState({ status: newState });
      localStorage.setItem(nodeId, newState);
    }
  }

  /** Controls the selection and deselection of a node by switching states and updating the graph */
  toggleSelection = () => {
    this.setState({ selected: !this.state.selected }, function() {
      this.updateNode();
    });
  }

  /** Sets the status of all missing prerequisites to 'missing' */
  focusPrereqs = () => {
    var svg = this.props.svg;  // contains information about the whole graph

    // Check if there are any missing prerequisites
    if (
      ["inactive", "overridden", "takeable"].indexOf(this.state.status) >= 0
    ) {
      this.setState({ status: "missing" }, () => {
        // set the Edges to missing if needed
        this.props.inEdges.forEach(edge => {
          var currentEdge = svg.edges.current[edge];
          if (currentEdge === null || currentEdge === undefined) {
            return;
          }
          var sourceNode = refLookUp(currentEdge.props.source, svg);
          if (!sourceNode.isSelected()) {
            currentEdge.setState({ status: "missing" });
          }
        });

        // recurse on the parents
        this.props.parents.forEach(node => {
          // base case: single node
          if (typeof node === "string") {
            var currentNode = refLookUp(node, svg);
            if (currentNode !== undefined) {
              currentNode.focusPrereqs();
            }
          } else {
            node.forEach(n => {
              var currentNode = refLookUp(n, svg);
              if (currentNode !== undefined) {
                currentNode.focusPrereqs();
              }
            });
          }
        });
      });
    }
  }

  /**
   * Resets 'missing' nodes and edges to the previous statuses:
   *  active, inactive, overridden, takeable
   */
  unfocusPrereqs = () => {
    var svg = this.props.svg;  // contains information about the whole graph
    this.updateNode(false);  // change the status of the current node

    // recurse on the parents
    this.props.parents.forEach(function(node) {
      // base case
      if (typeof node === "string") {
        var currentNode = refLookUp(node, svg);
        currentNode.unfocusPrereqs();
      } else {
        node.forEach(n => {
          var currentNode = refLookUp(n, svg);
          currentNode.unfocusPrereqs();
        });
      }
    });

    // update the edges
    this.props.inEdges.forEach(function(edge) {
      var currentEdge = svg.edges.current[edge];
      if (currentEdge.state.status === "missing") {
        currentEdge.updateStatus();
      }
    });
  }

  getDataTestId = () => {
    if (this.props.hybrid) {
      return `h(${this.props.parents.join(',')})`;
    }
    return this.props.JSON.id_;
  }

  render() {
    let ellipse = null;
    var newClassName = this.props.className + " " + this.state.status;
    if (this.props.highlighted) {
      var attrs = this.props.JSON;
      var width = parseFloat(attrs.width) / 2;
      var height = parseFloat(attrs.height) / 2;
      ellipse = (
        <ellipse
          className="spotlight"
          cx={parseFloat(attrs.pos[0]) + width}
          cy={parseFloat(attrs.pos[1]) + height}
          rx={width + 9}
          ry={height + 8.5}
        />
      );
    }

    var gAttrs = {
      textRendering: "geometricPrecision",
      shapeRendering: "geometricPrecision",
      onKeyDown: this.props.svg.onKeyDown,
      onWheel: this.props.svg.onWheel,
      onMouseEnter: this.props.onMouseEnter,
      onMouseLeave: this.props.onMouseLeave,
      onClick: this.props.onClick
    };

    var rectAttrs = {
      height: this.props.JSON.height,
      width: this.props.JSON.width,
      rx: "4",
      ry: "4",
      x: this.props.JSON.pos[0],
      y: this.props.JSON.pos[1]
    };

    var rectStyle = {
      fill: this.props.JSON.fill
    };

    var textXOffset = this.props.JSON.pos[0] + this.props.JSON.width / 2;

    // TODO: Look at this.props to see what we need to give the g
    return (
      <g {...gAttrs} id={this.props.JSON.id_} className={newClassName} data-testid={this.getDataTestId()}>
        {ellipse}
        <rect {...rectAttrs} style={rectStyle} />
        {this.props.JSON.text.map(function(textTag, i) {
          var textAttrs = {
            x: textXOffset,
            y: textTag.pos[1]
          };
          return (
            <text {...textAttrs} key={i}>
              {textTag.text}
            </text>
          );
        })}
      </g>
    );
  }
}

Node.propTypes = {
  childs: PropTypes.array,
  className: PropTypes.string,
  editMode: PropTypes.bool,
  highlighted: PropTypes.bool,
  hybrid: PropTypes.bool,
  inEdges: PropTypes.array,
  JSON: PropTypes.object,
  onClick: PropTypes.func,
  onMouseEnter: PropTypes.func,
  onMouseLeave: PropTypes.func,
  outEdges: PropTypes.array,
  parents: PropTypes.array,
  svg: PropTypes.object
};
