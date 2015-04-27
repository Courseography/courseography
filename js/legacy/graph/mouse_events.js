'use strict';

import $ from 'jquery';

import { removeToolTips } from './utilities/util';
import { displayTooltip } from '../common/modal';
import { updateClickedCourses, updateFCECount } from './sidebar/fce_count';

/**
 * Sets all mouse callbacks.
 */
export function setMouseCallbacks() {
    'use strict';

    var nodeObjects = $('.node');

    nodeObjects.click(turnNode)
               .hover(hoverFocus, hoverUnfocus);
}


/**
 * Performs hover function for a Node.
 * @param {object} event The hover event.
 */
function hoverFocus(event) {
    'use strict';

    if ($(".modal").length === 0 && activeFocus === '') {
        var id = event.target.parentNode.id;
        // Highlight missing prerequisites
        global.nodeMap[id].focus();

        removeToolTips();
        displayTooltip(id);
    }
}


/**
 * Performs the mouse out function for a Node.
 * @param {object} event The mouse out event.
 */
function hoverUnfocus(event) {
    'use strict';

    if ($(".modal").length === 0) {
        var id = event.target.parentNode.id;
        global.nodeMap[id].unfocus();

        var timeout = setTimeout(function () {
            $('.tooltip-group').hide('slow', function () { $(this).remove();});
        }, 100);
        timeouts.push(timeout);
    }
}


/**
 * Activates/Deactivates a clicked Node.
 * @param {object} event The click event.
 */
function turnNode(event) {
    'use strict';

    if (activeFocus === '' && $('.modal').length === 0) {
        var id = event.target.parentNode.id;
        // Update this node
        global.nodeMap[id].turn();
        updateClickedCourses(id, global.nodeMap[id].isSelected());
        updateFCECount();

        // Check the courses with FCE reqs
        global.nodeMap['csc318'].updateStatus();
        global.nodeMap['csc454'].updateStatus();

    }
}
