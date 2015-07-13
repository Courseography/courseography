/**
 * Constructs an Edge.
 * @param {Node} source This Edge's source Node.
 * @param {Node} target This Edge's target Node.
 * @param {string} id The id of the SVG path element that this Edge represents.
 * @constructor
 */
function Edge(source, target, id) {
    'use strict';

    this.source = source;
    this.target = target;
    this.id = id;
    this.status = 'inactive';
}


/**
 * Updates this Edge's status.
 */
Edge.prototype.updateStatus = function () {
    'use strict';

    if (!this.source.isSelected()) {
        this.status = 'inactive';
    } else if (!this.target.isSelected()) {
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
