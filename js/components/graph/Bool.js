import React from "react"
import { refLookUp } from "../common/utils"
import PropTypes from "prop-types"

/** Class representing a boolean node (and/or) */
export default class Bool extends React.Component {
  /**
   * Create a boolean node.
   * @param {string} state - The status of the node which can be active, inactive, or missing
   */
  constructor(props) {
    super(props)
    this.state = { status: "inactive" }
  }

  /**
   * Check whether the Bool is selected.
   * @returns {boolean} Whether status is active or not.
   */
  isSelected = () => {
    return this.state.status == "active"
  }

  /**
   * Check if the prerequisite courses have been satisfied based on bool type.
   * @returns {boolean} Whether any of the prereqs are satisfied.
   */
  arePrereqsSatisfied = () => {
    var svg = this.props.svg
    function isAllTrue(element) {
      return svg.nodes.current[element]
        ? svg.nodes.current[element].isSelected()
        : svg.bools.current[element].isSelected()
    }

    if (this.props.logicalType === "and") {
      return this.props.parents.every(isAllTrue)
    } else if (this.props.logicalType === "or") {
      return this.props.parents.some(isAllTrue)
    }
  }

  /**
   * Update the Bool's state at any moment given the prereqs and current state.
   */
  updateNode = () => {
    var svg = this.props.svg
    var newState = this.arePrereqsSatisfied() ? "active" : "inactive"

    var boolId = this.props.JSON.id_
    this.setState({ status: newState }, function () {
      localStorage.setItem(boolId, newState)
      this.props.childs.forEach(function (node) {
        var currentNode = refLookUp(node, svg)
        currentNode.updateNode(svg)
      })
      var allEdges = this.props.outEdges.concat(this.props.inEdges)
      allEdges.forEach(function (edge) {
        var currentEdge = svg.edges.current[edge]
        currentEdge.updateStatus()
      })
    })
  }

  /**
   * Cross check with the selected focus prerequisites.
   */
  focusPrereqs = () => {
    var svg = this.props.svg
    // Check if there are any missing prerequisites.
    if (this.state.status !== "active") {
      this.setState({ status: "missing" }, () => {
        this.props.inEdges.forEach(edge => {
          var currentEdge = svg.edges.current[edge]
          var sourceNode = refLookUp(currentEdge.props.source, svg)
          if (!sourceNode.isSelected()) {
            currentEdge.updateStatus("missing")
          }
        })
        this.props.parents.forEach(node => {
          var currentNode = refLookUp(node, svg)
          currentNode.focusPrereqs()
        })
      })
    }
  }

  /**
   * Remove the focus preqrequisites if the focus is unselected.
   */
  unfocusPrereqs = () => {
    var svg = this.props.svg
    this.updateNode(svg)
    this.props.parents.forEach(function (node) {
      var currentNode = refLookUp(node, svg)
      currentNode.unfocusPrereqs(svg)
    })
  }

  render() {
    var ellipseAttrs = {
      cx: this.props.JSON.pos[0],
      cy: this.props.JSON.pos[1],
      rx: "9.8800001",
      ry: "7.3684001",
    }
    return (
      <g
        {...this.props.JSON}
        className={this.props.className + " " + this.state.status}
        data-testid={`and(${this.props.parents.join()})`}
      >
        <ellipse {...ellipseAttrs} />
        {this.props.JSON.text.map(
          function (textTag, i) {
            var textAttrs = {
              x: ellipseAttrs.cx,
              y: textTag.pos[1],
            }
            return (
              <text {...textAttrs} key={i}>
                {this.props.logicalType}
              </text>
            )
          }.bind(this)
        )}
      </g>
    )
  }
}

Bool.propTypes = {
  childs: PropTypes.array,
  className: PropTypes.string,
  JSON: PropTypes.object,
  inEdges: PropTypes.array,
  logicalType: PropTypes.string,
  outEdges: PropTypes.array,
  parents: PropTypes.array,
  svg: PropTypes.object,
}
