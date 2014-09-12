/**
 * Data Structures for the course graph
 */

// Global variables
var nodes = []; // List of all nodes
var edges = []; // List of all edges, Edges is never used.


/**
 *
 * @param {Array} parents
 * @param {string} type
 * @param {string} name
 */
function makeNode(parents, type, name) {
    'use strict';

    window[name] = new Node(parents, type, name);
    nodes.push(name);
}


/**
 *
 * @param {Array} parents
 * @param {string} type
 * @param {string} name
 */
function makeHybrid(parents, type, name) {
    'use strict';

    makeNode(parents, type, name);
    window[name].hybrid = true;
}


/**
 *
 * @param {Node} parent
 * @param {Node} child
 * @param {string} name
 */
function makeEdge(parent, child, name) {
    'use strict';

    window[name] = new Edge(parent, child, name);
    parent.outEdges.push(window[name]);
    child.inEdges.push(window[name]);

    parent.children.push(child);
    child.parents.push(parent);
}

