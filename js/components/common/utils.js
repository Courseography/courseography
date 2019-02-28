/**
 * Search for target node in list of nodes,
 * or if node not found search through list of bools.
 * @param {React.PropTypes.node} targetNode
 * @param {React.PropTypes.element} elem
 * @returns {React.PropTypes.Node}
 */
export function refLookUp(targetNode, svg) {
    return svg.nodes.current[targetNode] ||
        svg.bools.current[targetNode];
}
