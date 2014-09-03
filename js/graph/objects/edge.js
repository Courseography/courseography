function Edge(parent, child, name) {
    'use strict';

    this.parent = parent;
    this.child = child;
    this.name = name;
    this.status = 'inactive';
}


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


Edge.prototype.updateSVG = function () {
    'use strict';

    $('#' + this.name).attr('data-active', this.status);
};