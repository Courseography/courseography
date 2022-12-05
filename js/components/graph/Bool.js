import React from "react"
import PropTypes from "prop-types"

/** Class representing a boolean node (and/or) */
export default class Bool extends React.Component {
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
  status: PropTypes.string,
}
