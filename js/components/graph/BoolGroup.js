import Bool from "./Bool"
import React from "react"
import PropTypes from "prop-types"

/** Class representing the group of Bools */
export default class BoolGroup extends React.Component {
  // Generate data for a Bool node
  generateBool = boolJSON => {
    const { parents } = this.props.connections
    return (
      <Bool
        JSON={boolJSON}
        className="bool"
        key={boolJSON.id_}
        parents={parents[boolJSON.id_]}
        logicalType={(boolJSON.text[0] && boolJSON.text[0].text) || "and"}
        svg={this.props.svg}
        status={this.props.boolsStatus[boolJSON.id_]}
      />
    )
  }

  render() {
    return (
      <g id="bools">{Object.values(this.props.boolsJSON).map(this.generateBool)}</g>
    )
  }
}

BoolGroup.propTypes = {
  boolsJSON: PropTypes.object,
  boolsStatus: PropTypes.object,
  connections: PropTypes.object,
  svg: PropTypes.object,
}
