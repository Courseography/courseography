/**
 * Data Structures for the course graph
 */

// Global variables
var nodes = []; // List of all nodes

// Data Structures
function makeNode(type, name) {
    'use strict';

    try {
        window[name] = new Node(type, name);

    } catch (err) {
        console.log("Trying to find name: " + name);
    }
    nodes.push(name);
}


function makeHybrid(type, name) {
    'use strict';

    makeNode(type, name);
    window[name].hybrid = true;
}


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

