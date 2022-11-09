import PropTypes from "prop-types"
import React from "react"
import { refLookUp } from "../common/utils"

/** React component class representing a Node on the graph
 *
 * Status and Selected Props:
 *  - status: holds the current status message (see below)
 *  - selected: whether the node is selected, based on the status message
 *
 *  Unselected status messages:
 *    - takeable: all prerequisites are satisfied (or no prereqs/parents)
 *    - inactive: missing some prerequisites
 *
 *  Selected status messages:
 *    - active: all prerequisities are satisfied
 *    - overridden: missing some prerequisities (will have a red border)
 *
 *  On Hover status message:
 *    - missing: means that this node is a prerequisite node that is not satisfied  (red border)
 *
 * Types of nodes:
 *  - Course nodes are nodes that represent a certain course
 *  - Hybrid nodes are the smaller, grey nodes on the graph that represent another course node
 *    farther away. They can only be either 'active' or 'inactive'
 */
export default class Node extends React.Component {
  /**
   * Checks whether this Node is selected
   * @return {boolean}
   */
  isSelected = () => {
    if (this.props.hybrid) {
      return this.props.status === "active"
    } else {
      return this.props.selected
    }
  }

  /**
   * Checks whether all prerequisite/preceding nodes for the current one are satisfied
   * @return {boolean}
   */
  arePrereqsSatisfied = () => {
    var svg = this.props.svg
    /**
     * Recursively checks that preceding nodes are selected
     * @param  {string|Array} element Node(s)/other on the graph
     * @return {boolean}
     */
    function isAllTrue(element) {
      if (typeof element === "string") {
        if (svg.nodes.current[element] !== undefined) {
          return svg.nodes.current[element].isSelected()
        } else if (svg.bools.current[element] !== undefined) {
          return svg.bools.current[element].isSelected()
        } else {
          return false
        }
      } else {
        return element.some(isAllTrue)
      }
    }

    return this.props.parents.every(isAllTrue)
  }

  /**
   * Update the state/status of a node (and its children/edges)
   * @param  {boolean} recursive whether we should recurse on its children
   */
  updateNode = recursive => {
    var newState
    if (this.arePrereqsSatisfied()) {
      if (this.isSelected() || this.props.hybrid) {
        newState = "active"
      } else {
        newState = "takeable"
      }
    } else {
      if (this.isSelected() && !this.props.hybrid) {
        newState = "overridden"
      } else {
        newState = "inactive"
      }
    }

    var nodeId = this.props.JSON.id_

    // Updating the children will be unnecessary if the selected state of the current node has not
    // changed, and the original state was not 'missing'
    if (
      ["active", "overridden"].indexOf(newState) >= 0 ===
        ["active", "overridden"].indexOf(this.props.status) >= 0 &&
      this.props.status !== "missing"
    ) {
      localStorage.setItem(nodeId, newState)
      this.props.updateNodeStatus(nodeId, newState)
      return
    }

    if (recursive === undefined || recursive) {
      var svg = this.props.svg
      this.props.updateNodeStatus(nodeId, newState, () => {
        localStorage.setItem(nodeId, newState)
        this.props.childs.forEach(node => {
          var currentNode = refLookUp(node, svg)
          if (currentNode !== undefined) {
            currentNode.updateNode()
          }
        })
        var allEdges = this.props.outEdges.concat(this.props.inEdges)
        allEdges.forEach(edge => {
          var currentEdge = svg.edges.current[edge]
          if (currentEdge !== undefined) {
            currentEdge.updateStatus()
          }
        })
      })
    } else {
      this.props.updateNodeStatus(nodeId, newState)
      localStorage.setItem(nodeId, newState)
    }
  }

  /** Controls the selection and deselection of a node by switching states and updating the graph */
  toggleSelection = () => {
    this.props.updateNodeSelected(
      this.props.JSON.id_,
      !this.props.selected,
      this.updateNode
    )
  }

  /** Sets the status of all missing prerequisites to 'missing' */
  focusPrereqs = () => {
    var svg = this.props.svg
    // Missing prerequisites need to have their status updated to 'missing'
    if (["inactive", "overridden", "takeable"].indexOf(this.props.status) >= 0) {
      this.props.updateNodeStatus(this.props.JSON.id_, "missing", () => {
        this.props.inEdges.forEach(function (edge) {
          var currentEdge = svg.edges.current[edge]
          if (currentEdge === null || currentEdge === undefined) {
            return
          }
          var sourceNode = refLookUp(currentEdge.props.source, svg)
          if (!sourceNode.isSelected()) {
            currentEdge.setState({ status: "missing" })
          }
        })
        this.props.parents.forEach(function (node) {
          if (typeof node === "string") {
            var currentNode = refLookUp(node, svg)
            if (currentNode !== undefined) {
              currentNode.focusPrereqs()
            }
          } else {
            node.forEach(n => {
              var currentNode = refLookUp(n, svg)
              if (currentNode !== undefined) {
                currentNode.focusPrereqs()
              }
            })
          }
        })
      })
    }
  }

  /**
   * Resets 'missing' nodes and edges to the previous statuses:
   *  active, inactive, overridden, takeable
   */
  unfocusPrereqs = () => {
    var svg = this.props.svg
    this.updateNode(false)
    this.props.parents.forEach(function (node) {
      if (typeof node === "string") {
        var currentNode = refLookUp(node, svg)
        currentNode.unfocusPrereqs()
      } else {
        node.forEach(n => {
          var currentNode = refLookUp(n, svg)
          currentNode.unfocusPrereqs()
        })
      }
    })
    this.props.inEdges.forEach(function (edge) {
      var currentEdge = svg.edges.current[edge]
      if (currentEdge.state.status === "missing") {
        currentEdge.updateStatus()
      }
    })
  }

  getDataTestId = () => {
    if (this.props.hybrid) {
      return `h(${this.props.parents.join(",")})`
    }
    return this.props.JSON.id_
  }

  render() {
    let ellipse = null
    var newClassName = this.props.className + " " + this.props.status
    if (this.props.highlighted) {
      var attrs = this.props.JSON
      var width = parseFloat(attrs.width) / 2
      var height = parseFloat(attrs.height) / 2
      ellipse = (
        <ellipse
          className="spotlight"
          cx={parseFloat(attrs.pos[0]) + width}
          cy={parseFloat(attrs.pos[1]) + height}
          rx={width + 9}
          ry={height + 8.5}
        />
      )
    }

    var gAttrs = {
      textRendering: "geometricPrecision",
      shapeRendering: "geometricPrecision",
      onKeyDown: this.props.svg.onKeyDown,
      onWheel: this.props.svg.onWheel,
      onMouseEnter: this.props.onMouseEnter,
      onMouseLeave: this.props.onMouseLeave,
      onClick: this.props.onClick,
    }

    var rectAttrs = {
      height: this.props.JSON.height,
      width: this.props.JSON.width,
      x: this.props.JSON.pos[0],
      y: this.props.JSON.pos[1],
    }

    if (this.props.className === "node") {
      rectAttrs["rx"] = "8"
      rectAttrs["ry"] = "8"
    }

    var rectStyle = {
      fill: this.props.JSON.fill,
    }

    var textXOffset = this.props.JSON.pos[0] + this.props.JSON.width / 2

    // TODO: Look at this.props to see what we need to give the g
    return (
      <g
        {...gAttrs}
        id={this.props.JSON.id_}
        className={newClassName}
        data-testid={this.getDataTestId()}
      >
        {ellipse}
        <rect
          {...rectAttrs}
          style={rectStyle}
          filter={
            this.props.className === "hybrid"
              ? ""
              : `url(#${this.props.nodeDropshadowFilter})`
          }
        />
        {this.props.JSON.text.map(function (textTag, i) {
          var textAttrs = {
            x: textXOffset,
            y: textTag.pos[1],
          }
          return (
            <text {...textAttrs} key={i}>
              {textTag.text}
            </text>
          )
        })}
      </g>
    )
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
  updateNodeStatus: PropTypes.func,
  updateNodeSelected: PropTypes.func,
  status: PropTypes.string,
  selected: PropTypes.bool,
  parents: PropTypes.array,
  svg: PropTypes.object,
  nodeDropshadowFilter: PropTypes.string,
}
