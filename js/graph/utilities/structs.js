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
**/
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
**/
function makeHybrid(type, name) {
    'use strict';

    makeNode(type, name);
    window[name].hybrid = true;
}


/**
 * Makes an Edge.
 * @param {Node} source The source Node of the Edge.
 * @param {Node} target The target Node of the Edge.
 * @param {string} id The name of the Edge.
**/
function makeEdge(parent, child, name) {
    'use strict';

    try {
        window[name] = new Edge(parent, child, name);
        parent.outEdges.push(window[name]);
        child.inEdges.push(window[name]);

        parent.children.push(child);
        child.parents.push(parent);
    } catch (err) {
        console.log('Encountered an error for: ' + name);
        console.log(err);
    }
}

