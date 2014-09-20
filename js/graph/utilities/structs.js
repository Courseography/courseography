/**
 * Data Structures for the course graph
 */


/**
 * Makes a Node.
 * @param {string} type The Node's type.
 * @param {string} name The Node's name.
 */
function makeNode(type, name) {
    'use strict';

    window[name] = new Node(type, name);
    nodes.push(name);
}


/**
 * Makes a Hybrid.
 * @param {string} type The hybrid's type.
 * @param {string} name The hybrid's name.
 */
function makeHybrid(type, name) {
    'use strict';

    makeNode(type, name);
    window[name].hybrid = true;
}


/**
 * Makes an Edge.
 * @param {Node} parent The source Node of the Edge.
 * @param {Node} child The target Node of the Edge.
 * @param {string} name The name of the Edge.
 */
function makeEdge(parent, child, name) {
    'use strict';

    window[name] = new Edge(parent, child, name);
    parent.outEdges.push(window[name]);
    child.inEdges.push(window[name]);

    parent.children.push(child);
    child.parents.push(parent);
}

