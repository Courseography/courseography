/**
 * Sets all mouse callbacks.
 */
function setMouseCallbacks() {
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
        window[id].focus();

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
        window[id].unfocus();

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
        window[id].turn();
        updateClickedCourses(id, window[id].isSelected());
        updateFCECount();

        // Check the courses with FCE reqs
        CSC318.updateStatus();
        CSC454.updateStatus();

        updatePOSt(id, window[id].isSelected());
        updatePostInterface();
        updateMajorPostInterface();
        updateMinorPostInterface();
    }
}
