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
 * @param {Node} source The source Node of the Edge.
 * @param {Node} target The target Node of the Edge.
 * @param {string} id The name of the Edge.
 */
function makeEdge(source, target, id) {
    'use strict';

    window[id] = new Edge(source, target, id);
    source.outEdges.push(window[id]);
    target.inEdges.push(window[id]);

    source.children.push(target);
    target.parents.push(source);
}

