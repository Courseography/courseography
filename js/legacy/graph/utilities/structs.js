/**
 * Data Structures for the course graph
 */

import { Node } from '../objects/node';
import Edge from '../objects/edge';

// Global variables
var nodes = []; // List of all nodes

// Data Structures

global.nodeMap = {};

/**
 * Makes a Node.
 * @param {string} type The Node's type.
 * @param {string} name The Node's name.
**/
export function makeNode(type, name) {
    'use strict';

    global.nodeMap[name] = new Node(type, name);
    nodes.push(name);
}


/**
 * Makes a Hybrid.
 * @param {string} type The Hybrid's type.
 * @param {string} id The hybrid's name.
**/
export function makeHybrid(type, name) {
    'use strict';

    makeNode(type, name);
    global.nodeMap[name].hybrid = true;
}


/**
 * Makes an Edge.
 * @param {Node} source The source Node of the Edge.
 * @param {Node} target The target Node of the Edge.
 * @param {string} id The name of the Edge.
**/
export function makeEdge(parent, child, name) {
    'use strict';

    global.nodeMap[name] = new Edge(parent, child, name);
    parent.outEdges.push(global.nodeMap[name]);
    child.inEdges.push(global.nodeMap[name]);

    parent.children.push(child);
    child.parents.push(parent);
}

