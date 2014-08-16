function Edge(parent, child, name) {
    this.parent = parent;
    this.child = child;
    this.name = name;
    this.status = 'inactive';
}


Edge.prototype.updateStatus = function () {
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
    $('#' + this.name).attr('data-active', this.status);
};