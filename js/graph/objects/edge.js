/**
 * Constructs an Edge.
 * @param {Node} parent This Edge's source Node.
 * @param {Node} child This Edge's child Node.
 * @param {string} id The id of the SVG path element that this Edge represents.
 * @constructor
 */
function Edge(parent, child, id) {
    'use strict';

    this.parent = parent;
    this.child = child;
    this.id = id;
    this.status = 'inactive';
}


/**
 * Updates this Edge's status.
 */
Edge.prototype.updateStatus = function () {
    'use strict';

    if (!this.parent.isSelected()) {
        this.status = 'inactive';
    } else if (!this.child.isSelected()) {
        this.status = 'takeable';
    } else {
        this.status = 'active';
    }
    this.updateSVG();
};


/**
 * Updates the corresponding SVG path element.
 */
Edge.prototype.updateSVG = function () {
    'use strict';

    $('#' + this.id).attr('data-active', this.status);
};