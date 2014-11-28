function Node(type, name) { // parents is never used.
    'use strict';

    this.name = name; // Used to identify the GUI node
    this.parents = []; // Prerequisite nodes
    this.children = []; // Nodes for which this is a prerequisite
    this.outEdges = []; // Edges leading to children
    this.inEdges = []; // Edges coming from parents
    this.logicalType = type; // 'AND' or 'OR' of prerequisites
    this.updated = false; // Used when updating active/inactive state
    this.hybrid = false; // Identifies whether node is 'hybrid'
    this.status = 'inactive';
}


// Returns true if the node has been selected
Node.prototype.isSelected = function () {
    'use strict';

    return this.status === 'active' || this.status === 'overridden';
};


// Used when entering hover
Node.prototype.focus = function () {
    'use strict';

    if (this.status !== 'active') {
        if (this.status !== 'overridden') {
            $('#' + this.name).attr('data-active', 'missing');
        }
        $.each(this.inEdges, function (i, edge) {
            if (edge.parent.status !== 'active') {
                $('#' + edge.name).attr('data-active', 'missing');
            }
        });
        $.each(this.parents, function (i, node) {
            node.focus();
        });
    }
};


// Used when leaving hover
Node.prototype.unfocus = function () {
    'use strict';

    if (!this.isSelected()) {
        if (activeFocus === '' || window[activeFocus + 'FocusList'].indexOf(this.name) > -1) {
            this.updateSVG();
        } else {
            $('#' + this.name).attr('data-active', 'unlit');
        }
    }

    $.each(this.parents, function (i, node) {
        node.unfocus();
    });
    $.each(this.outEdges, function (i, edge) {
        edge.updateStatus();
    });
};


// Check whether node's prerequisites are satisfied, update status and GUI
Node.prototype.updateStatus = function () {
    'use strict';

    if (this.arePrereqsSatisfied()) {
        if (this.isSelected() || this.hybrid) {
            this.status = 'active';
        } else {
            this.status = 'takeable';
        }
    } else {
        if (this.isSelected() && !this.hybrid) {
            this.status = 'overridden';
        } else {
            this.status = 'inactive';
        }
    }
    setCookie(this.name, this.status);

    // Always update children of hybrids
    if (this.hybrid) {
        $.each(this.children, function(i, node) {
            node.updateStatus();
        });
        $.each(this.outEdges, function (i, edge) {
            edge.updateStatus();
        });
    }

    this.updateSVG();
};


// Activate/deactivate a node; called when a node is clicked
Node.prototype.turn = function () {
    'use strict';

    if (this.isSelected()) {
        this.status = 'inactive';
    } else {
        this.status = 'active';
    }

    this.updateStatus();

    $.each(this.children, function (i, node) {
        node.updateStatus();
    });
    $.each(this.outEdges, function (i, edge) {
        edge.updateStatus();
    });
    $.each(this.inEdges, function (i, edge) {
        edge.updateStatus();
    });

    // Update interface
    this.updateSVG();
};


// Returns true if the node's prerequisites are satisfied
Node.prototype.arePrereqsSatisfied = function () {
    'use strict';

    var sat = this.checkFCEBasedPrerequisites();
    if (this.logicalType === 'AND') {
        for (var i = 0; i < this.parents.length; i++) {
            sat = sat && this.parents[i].isSelected();
        }
    } else if (this.logicalType === 'OR') {
        sat = false;
        for (var i = 0; i < this.parents.length; i++) {
            sat = sat || this.parents[i].isSelected();
        }
    } else {
        console.log('Error: invalid node logicalType ' + this.type + ' for node ' + this.name);
    }
    return sat;
};


// Checks FCE-count prerequisites
Node.prototype.checkFCEBasedPrerequisites = function() {
    'use strict';

    if (this.name === 'CSC454') {
        return FCEs200 + FCEs300 + FCEs400 >= 2.5;
    } else if (this.name === 'CSC494' || this.name === 'CSC495') {
        return FCEs300 + FCEs400 >= 1.5;
    } else if (this.name === 'CSC318') {
        return FCEs >= 0.5;
    } else {
        return true;
    }
};


// Update the visual style of the corresponding graphical node
Node.prototype.updateSVG = function() {
    'use strict';

    $('#' + this.name).attr('data-active', this.status);
};