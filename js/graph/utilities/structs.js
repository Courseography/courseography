/**
 * Data Structures for the course graph
 */


/**
 * Makes a Node.
 * @param {Array} parents The Node's parents.
 * @param {string} type The Node's type.
 * @param {string} name The Node's name.
 */
function makeNode(parents, type, name) {
    'use strict';

    window[name] = new Node(parents, type, name);
    nodes.push(name);
}


/**
 * Makes a hybrid.
 * @param {Array} parents The hybrid's parents.
 * @param {string} type The hybrid's type.
 * @param {string} name The hybrid's name.
 */
function makeHybrid(parents, type, name) {
    'use strict';

    makeNode(parents, type, name);
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

