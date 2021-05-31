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
    var hybridRelationships = [];
    return (
      <g id="nodes">
        {this.props.hybridsJSON.map(entry => {
          var parents = this.props.connections.hybridParents[entry.id_];
          return (
            <Node
              JSON={entry}
              className={"hybrid"}
              key={entry.id_}
              hybrid={true}
              ref={this.setRefEntry(entry)}
              parents={parents}
              childs={this.props.connections.hybridChildren[entry.id_]}
              inEdges={[]}
              outEdges={this.props.connections.hybridOutEdges[entry.id_]}
              svg={svg}
              logicalType={"AND"}
            />
          );
        })}
        {this.props.nodesJSON.map(entry => {
          var highlighted = highlightedNodes.indexOf(entry.id_) >= 0;
          var childs = this.props.connections.children[entry.id_].slice();
          hybridRelationships.forEach(element => {

            if (element[0] === entry.id_) {
              childs.push(element[1]);
            }
          });
          return (
            <Node
              JSON={entry}
              className="node"
              key={entry.id_}
              ref={this.setRefEntry(entry)}
              hybrid={false}
              parents={this.props.connections.parents[entry.id_]}
              childs={childs}
              inEdges={this.props.connections.inEdges[entry.id_]}
              outEdges={this.props.connections.outEdges[entry.id_]}
              svg={svg}
              highlighted={highlighted}
              onClick={this.props.nodeClick}
              onMouseEnter={this.props.nodeMouseEnter}
              onMouseLeave={this.props.nodeMouseLeave}
              onMouseDown={this.props.nodeMouseDown}
              editMode={this.props.editMode}
            />
          );
        })}
      </g>
    );
  }
}

/**
 *
 * @param {string} s
 * @returns {Array}
 */
function parseAnd(s) {
  "use strict";

  var curr = s;
  var andList = [];
  while (curr.length > 0) {
    if (
      curr.charAt(0) === "," ||
      curr.charAt(0) === ";" ||
      curr.charAt(0) === " "
    ) {
      curr = curr.substr(1);
    } else {
      var result = parseOr(curr);
      if (curr === result[1]) {
        console.error("Parsing failed for " + s + "  with curr = " + curr);
        break;
      } else {
        curr = result[1];
        andList.push(result[0]);
      }
    }
  }
  return [andList, curr];
}

/**
 *
 * @param {string} s
 * @returns {Array}
 */
function parseOr(s) {
  "use strict";

  var curr = s;
  var orList = [];
  var tmp;
  var result;
  var coursePrefix;
  while (curr.length > 0 && curr.charAt(0) !== "," && curr.charAt(0) !== ";") {
    if (curr.charAt(0) === "(") {
      tmp = curr.substr(1, curr.indexOf(")"));
      if (coursePrefix === undefined && tmp.length >= 6) {
        coursePrefix = tmp.substr(0, 3).toUpperCase();
      }
      result = parseCourse(tmp, coursePrefix);

      orList.append(result[0]);
      curr = curr.substr(curr.indexOf(")") + 1);
    } else if (curr.charAt(0) === " " || curr.charAt(0) === "/") {
      curr = curr.substr(1);
    } else {
      if (coursePrefix === undefined && curr.length >= 6) {
        coursePrefix = curr.substr(0, 3).toUpperCase();
      }
      result = parseCourse(curr, coursePrefix);
      if (curr === result[1]) {
        console.error("Parsing failed for " + s + " with curr = " + curr);
        break;
      }
      curr = result[1];
      orList.push(result[0]);
    }
  }

  if (orList.length === 1) {
    orList = orList[0];
  }

  return [orList, curr];
}

/**
 *
 * @param {string} s
 * @param {string} prefix
 * @returns {Array}
 */
function parseCourse(s, prefix) {
  "use strict";

  var start = s.search(/[,/]/);

  if (start === 3) {
    return [prefix + s.substr(0, start), s.substr(start)];
  } else if (start > 0) {
    return [s.substr(0, start).toUpperCase(), s.substr(start)];
  }

  if (s.length === 3) {
    return [prefix + s, ""];
  }

  return [s, ""];
}

export {parseAnd};

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
  svg: PropTypes.object
};
