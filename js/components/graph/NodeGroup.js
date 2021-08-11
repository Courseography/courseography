import PropTypes from "prop-types";
import React from "react";
import Node from "./Node";

/** A React component class representing a group of Nodes on the graph */
export default class NodeGroup extends React.Component {
  /** Returns nodes to their original unselected state, with a status of "takeable" or "inactive" */
  reset = () => {
    this.props.nodesJSON.forEach(nodeJSON => {
      var node = this[nodeJSON.id_];
      var state = node.props.parents.length === 0 ? "takeable" : "inactive";
      node.setState({ status: state, selected: false });
      localStorage.setItem(node.props.JSON.id_, state);
    });

    this.props.hybridsJSON.forEach(hybridJSON => {
      var hybrid = this[hybridJSON.id_];
      var state = hybrid.props.parents.length === 0 ? "takeable" : "inactive";
      hybrid.setState({ status: state, selected: false });
      localStorage.setItem(hybrid.props.JSON.id_, state);
    });
  }

  /**
   * Helper for hybrid computation. Finds the node with the same course label as the hybrid.
   * @param  {string} course
   * @return {Node}
   */
  findRelationship = course => {
    var nodes = this.props.nodesJSON;
    var node = nodes.find(
      n =>
        n.type_ === "Node" &&
        n.text.some(textTag => textTag.text.includes(course))
    );
    return node;
  }

  /**
   *
   * @param {*} entry
   * @return
   */
  setRefEntry = entry => {
    return (elem) => elem && (this[entry.id_] = elem);
  }

  render() {
    var svg = this.props.svg;
    var highlightedNodes = this.props.highlightedNodes;
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
              inEdges={[]}
              outEdges={this.props.connections.outEdges[entry.id_]}
              svg={svg}
              logicalType={"AND"}
              nodeDropshadowFilter={this.props.nodeDropshadowFilter}
            />
          );
        })}
        {this.props.nodesJSON.map(entry => {
          var highlighted = highlightedNodes.indexOf(entry.id_) >= 0;
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
              highlighted={highlighted}
              onClick={this.props.nodeClick}
              onMouseEnter={this.props.nodeMouseEnter}
              onMouseLeave={this.props.nodeMouseLeave}
              onMouseDown={this.props.nodeMouseDown}
              editMode={this.props.editMode}
              nodeDropshadowFilter={this.props.nodeDropshadowFilter}
            />
          );
        })}
      </g>
    );
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
  nodesJSON: PropTypes.array,
  connections: PropTypes.object,
  svg: PropTypes.object,
  nodeDropshadowFilter: PropTypes.string
};
