import Bool from "./Bool"
import React from "react"
import PropTypes from "prop-types"

/** Class representing the group of Bools */
export default class BoolGroup extends React.Component {
  /**
   * For each entry in boolsJSON update the id with the svg
   */
  componentDidMount() {
    this.props.boolsJSON.forEach(boolJSON => {
      this[boolJSON.id_].updateNode(this.props.svg)
    })
  }

  /**
   * Reset the Bool JSON to have all states be inactive
   */
  reset = () => {
    this.props.boolsJSON.forEach(boolJSON => {
      var bool = this[boolJSON.id_]
      bool.setState({ status: "inactive" })
    })
  }

  /**
   * Set the reference entry for a Bool JSON entry
   * @param {array} boolJSON Bool JSON
   * @returns {function (elem) {
     whether elem and the boolJSON id are True
   }}
   */
  setRefEntry = boolJSON => {
    return elem => elem && (this[boolJSON.id_] = elem)
  }

  // Generate data for a Bool node
  generateBool = boolJSON => {
    const relationships = this.props.boolToInfo[boolJSON.id_]
    return (
      <Bool
        JSON={boolJSON}
        className="bool"
        key={boolJSON.id_}
        ref={this.setRefEntry(boolJSON)}
        parents={relationships.parents}
        childs={relationships.childs}
        inEdges={relationships.inEdges}
        outEdges={relationships.outEdges}
        logicalType={(boolJSON.text[0] && boolJSON.text[0].text) || "and"}
        svg={this.props.svg}
      />
    )
  }

  render() {
    return <g id="bools">{this.props.boolsJSON.map(this.generateBool)}</g>
  }
}

BoolGroup.propTypes = {
  boolsJSON: PropTypes.array,
  edgesJSON: PropTypes.array,
  boolToInfo: PropTypes.object,
  svg: PropTypes.object,
}
