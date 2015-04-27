'use strict';

import $ from 'jquery';

import { setCookie } from '../../common/cookie_handler';

/**
 * Constructs a Node.
 * @param {string} type The logical type of this Node's prerequisites.
 *                      Either 'OR' or 'AND'.
 * @param {string} id The ID of the SVG g element that this Node represents.
 * @constructor
 */
export function Node(type, id) {
    'use strict';

    this.id = id; // Used to identify the GUI node
    this.parents = []; // Prerequisite nodes
    this.children = []; // Nodes for which this is a prerequisite
    this.outEdges = []; // Edges leading to children
    this.inEdges = []; // Edges coming from parents
    this.logicalType = type;
    this.updated = false; // Used when updating active/inactive state
    this.hybrid = false; // Identifies whether node is 'hybrid'
    this.status = 'inactive';
}


/**
 * Returns true if this Node has been selected.
 * @returns {boolean} Whether the node has been selected.
 */
Node.prototype.isSelected = function () {
    'use strict';

    return this.status === 'active' || this.status === 'overridden';
};


/**
 * Highlights this Node and this Node's missing prerequisites when this Node
 * is hovered over.
 */
Node.prototype.focus = function () {
    'use strict';

    if (this.status !== 'active') {
        if (this.status !== 'overridden') {
            $('#' + this.id).attr('data-active', 'missing');
        }
        $.each(this.inEdges, function (i, edge) {
            if (edge.source.status !== 'active') {
                $('#' + edge.id).attr('data-active', 'missing');
            }
        });
        $.each(this.parents, function (i, node) {
            node.focus();
        });
    }
};


/**
 * Removes highlight from this Node and Node's missing prerequisites when this
 * Node is un-hovered.
 */
Node.prototype.unfocus = function () {
    'use strict';

    if (!this.isSelected()) {
        if (activeFocus === '' ||
            window[activeFocus + 'FocusList'].indexOf(this.id) > -1) {
            this.updateSVG();
        } else {
            $('#' + this.id).attr('data-active', 'unlit');
        }
    }

    $.each(this.parents, function (i, node) {
        node.unfocus();
    });
    $.each(this.outEdges, function (i, edge) {
        edge.updateStatus();
    });
};


/**
 * Updates this Node's status.
 */
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
    setCookie(this.id, this.status);

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


/**
 * Selects/deselects this Node.
 */
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


/**
 * Returns true if this Node's prerequisites are satisfied.
 * @returns {boolean} Whether this Node's prerequisites are satisfied.
 */
Node.prototype.arePrereqsSatisfied = function () {
    'use strict';

    var sat = this.checkFCEBasedPrerequisites();
    if (this.logicalType.toUpperCase() === 'AND') {
        for (var i = 0; i < this.parents.length; i++) {
            sat = sat && this.parents[i].isSelected();
        }
    } else if (this.logicalType.toUpperCase() === 'OR') {
        sat = false;
        for (var i = 0; i < this.parents.length; i++) {
            sat = sat || this.parents[i].isSelected();
        }
    } else {
        console.log('Error: invalid node logicalType ' + this.logicalType +
                    ' for node ' + this.id);
    }
    return sat;
};


/**
 * Returns true if this Node's FCE-count prerequisites have been satisfied.
 * Note: Only certain courses have FCE-count prerequisites.
 * @returns {boolean} Whether this Node's FCE-count prerequisites have
 *                    been satisfied.
 */
Node.prototype.checkFCEBasedPrerequisites = function() {
    'use strict';

    if (this.id === 'CSC454') {
        return FCEs200 + FCEs300 + FCEs400 >= 2.5;
    } else if (this.id === 'CSC494' || this.id === 'CSC495') {
        return FCEs300 + FCEs400 >= 1.5;
    } else if (this.id === 'CSC318') {
        return FCEs >= 0.5;
    } else {
        return true;
    }
};


/**
 * Updates the corresponding SVG elements (g, rect).
 */
Node.prototype.updateSVG = function() {
    'use strict';

    $('#' + this.id).attr('data-active', this.status);
};
