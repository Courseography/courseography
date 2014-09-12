/**
 * Data Structures for the course graph
 */

// Global variables
var nodes = []; // List of all nodes
var edges = []; // List of all edges, Edges is never used.


/**
 *
 * @param parents
 * @param type
 * @param name
 */
function makeNode(parents, type, name) {
    'use strict';

    window[name] = new Node(parents, type, name);
    nodes.push(name);
}


/**
 *
 * @param parents
 * @param type
 * @param name
 */
function makeHybrid(parents, type, name) {
    'use strict';

    makeNode(parents, type, name);
    window[name].hybrid = true;
}


/**
 *
 * @param parent
 * @param child
 * @param name
 */
function makeEdge(parent, child, name) {
    'use strict';

    window[name] = new Edge(parent, child, name);
    parent.outEdges.push(window[name]);
    child.inEdges.push(window[name]);

    parent.children.push(child);
    child.parents.push(parent);
}

