/**
 * Data Structures for the course graph
 */

// Global variables
var nodes = []; // List of all nodes
var edges = []; // List of all edges, Edges is never used.


// Data Structures
function makeNode(parents, type, name) {
    'use strict';

    window[name] = new Node(parents, type, name);
    nodes.push(name);
}


function makeHybrid(parents, type, name) {
    'use strict';

    makeNode(parents, type, name);
    window[name].hybrid = true;
}


function makeEdge(parent, child, name) {
    'use strict';

    window[name] = new Edge(parent, child, name);
    parent.outEdges.push(window[name]);
    child.inEdges.push(window[name]);

    parent.children.push(child);
    child.parents.push(parent);
}

