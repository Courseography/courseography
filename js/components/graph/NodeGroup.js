import PropTypes from "prop-types"
import React from "react"
import Node from "./Node"

/** A React component class representing a group of Nodes on the graph */
export default class NodeGroup extends React.Component {
  /**
   *
   * @param {*} entry
   * @return
   */
  setRefEntry = entry => {
    return elem => elem && (this[entry.id_] = elem)
  }

  render() {
    var svg = this.props.svg
    var highlightedNodes = this.props.highlightedNodes
    return (
      <g id="nodes">
        {this.props.hybridsJSON.map(entry => {
          return (
            <Node
              JSON={entry}
              className={"hybrid"}
              key={entry.id_}
              hybrid={true}
              ref={this.setRefEntry(entry)}
              parents={this.props.connections.parents[entry.id_]}
              childs={this.props.connections.children[entry.id_]}
              status={this.props.nodesStatus[entry.id_].status}
              selected={this.props.nodesStatus[entry.id_].selected}
              unfocusPrereqs={this.props.unfocusPrereqs}
              arePrereqsSatisfied={this.props.arePrereqsSatisfied}
              updateNode={this.props.updateNode}
              focusPrereqs={this.props.focusPrereqs}
              toggleSelection={this.props.toggleSelection}
              inEdges={[]}
              outEdges={this.props.connections.outEdges[entry.id_]}
              svg={svg}
              logicalType={"AND"}
              nodeDropshadowFilter={this.props.nodeDropshadowFilter}
            />
          )
        })}
        {this.props.nodesJSON.map(entry => {
          // using `includes` to match "mat235" from "mat235237257calc2" and other math/stats courses
          const highlighted = highlightedNodes.some(node => entry.id_.includes(node))
          return (
            <Node
              JSON={entry}
              className="node"
              key={entry.id_}
              ref={this.setRefEntry(entry)}
              hybrid={false}
              parents={this.props.connections.parents[entry.id_]}
              childs={this.props.connections.children[entry.id_]}
              inEdges={this.props.connections.inEdges[entry.id_]}
              outEdges={this.props.connections.outEdges[entry.id_]}
              svg={svg}
              status={this.props.nodesStatus[entry.id_].status}
              selected={this.props.nodesStatus[entry.id_].selected}
              unfocusPrereqs={this.props.unfocusPrereqs}
              focusPrereqs={this.props.focusPrereqs}
              updateNode={this.props.updateNode}
              arePrereqsSatisfied={this.props.arePrereqsSatisfied}
              toggleSelection={this.props.toggleSelection}
              highlighted={highlighted}
              onClick={this.props.nodeClick}
              onMouseEnter={this.props.nodeMouseEnter}
              onMouseLeave={this.props.nodeMouseLeave}
              onMouseDown={this.props.nodeMouseDown}
              editMode={this.props.editMode}
              nodeDropshadowFilter={this.props.nodeDropshadowFilter}
            />
          )
        })}
      </g>
    )
  }
}

NodeGroup.propTypes = {
  edgesJSON: PropTypes.array,
  editMode: PropTypes.bool,
  highlightedNodes: PropTypes.array,
  hybridsJSON: PropTypes.array,
  nodeClick: PropTypes.func,
  nodeMouseDown: PropTypes.func,
  nodeMouseEnter: PropTypes.func,
  nodeMouseLeave: PropTypes.func,
  focusPrereqs: PropTypes.func,
  unfocusPrereqs: PropTypes.func,
  updateNode: PropTypes.func,
  toggleSelection: PropTypes.func,
  arePrereqsSatisfied: PropTypes.func,
  nodesStatus: PropTypes.object,
  nodesJSON: PropTypes.array,
  connections: PropTypes.object,
  svg: PropTypes.object,
  nodeDropshadowFilter: PropTypes.string,
}
