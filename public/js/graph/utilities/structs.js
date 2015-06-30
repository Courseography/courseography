/**
 * Data Structures for the course graph
 */

// Global variables
var nodes = []; // List of all nodes

// Data Structures

/**
 * Makes a Node.
 * @param {string} type The Node's type.
 * @param {string} name The Node's name.
 */
function makeNode(type, name) {
    'use strict';

    try {
        window[name] = new Node(type, name);
    } catch (err) {
        console.log("Trying to find name: " + name);
    }
    nodes.push(name);
}


/**
 * Makes a Hybrid.
 * @param {string} type The Hybrid's type.
 * @param {string} id The hybrid's name.
 */
function makeHybrid(type, id) {
    'use strict';

    makeNode(type, id);
    window[id].hybrid = true;
}


/**
 * Makes an Edge.
 * @param {Node} source The source Node of the Edge.
 * @param {Node} target The target Node of the Edge.
 * @param {string} id The id of the Edge.
 */
function makeEdge(source, target, id) {
    'use strict';

    try {
        window[id] = new Edge(source, target, name);
        parent.outEdges.push(window[id]);
        target.inEdges.push(window[id]);

        source.children.push(target);
        target.parents.push(source);
    } catch (err) {
        console.log('Encountered an error for: ' + id);
        console.log(err);
    }
}
