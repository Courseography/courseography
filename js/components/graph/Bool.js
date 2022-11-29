import React from "react"
import PropTypes from "prop-types"

/** Class representing a boolean node (and/or) */
export default class Bool extends React.Component {
  /**
   * Check whether the Bool is selected.
   * @returns {boolean} Whether status is active or not.
   */
  isSelected = () => {
    return this.props.status == "active"
  }

  /**
   * Check if the prerequisite courses have been satisfied based on bool type.
   * @returns {boolean} Whether any of the prereqs are satisfied.
   */
  arePrereqsSatisfied = () => {
    const svg = this.props.svg
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

  render() {
    const ellipseAttrs = {
      cx: this.props.JSON.pos[0],
      cy: this.props.JSON.pos[1],
      rx: "9.8800001",
      ry: "7.3684001",
    }
    return (
      <g
        {...this.props.JSON}
        className={this.props.className + " " + this.props.status}
        data-testid={`and(${this.props.parents.join()})`}
      >
        <ellipse {...ellipseAttrs} />
        {this.props.JSON.text.map(
          function (textTag, i) {
            const textAttrs = {
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
  className: PropTypes.string,
  JSON: PropTypes.object,
  logicalType: PropTypes.string,
  parents: PropTypes.array,
  svg: PropTypes.object,
  status: PropTypes.string,
}
