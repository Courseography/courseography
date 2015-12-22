/**
 * Disables Tab key.
 * @param {object} event The keydown event.
 * @returns {boolean} Whether the key is the tab key.
 */
document.onkeydown = function (event) {
    'use strict';

    if (event.which === 9) { // 9 is the Tab key.
        return false;
    }
};


/**
 * Clears the FCE count variables.
 */
function clearFCECount() {
    currentFCEs = 0;
    currentFCEs100 = 0;
    currentFCEs200 = 0;
    currentFCEs300 = 0;
    currentFCEs400 = 0;
    currentFCEsMAT = 0;
}

/**
 * Clears any active focus.
 */
function clearActiveFocus() {
    if (activeFocus !== '') {
        $('.focusTabs').tabs('option', 'active', false);
        $('ellipse.spotlight').remove();
        clearFocus();
    }
}


/**
 * Initializes interface to cookie settings; blank interface if no cookies exist
 * TODO: Function too long
 */
function initializeGraphSettings() {
    'use strict';

    clearFCECount();

    // Set initial node status
    $.each(nodes, function (i, node) {

        var nodeStatus = getCookie(window[node].id);

        if (nodeStatus.length === 0) {
            nodeStatus = 'inactive';
            setCookie(node, nodeStatus);
        }

        if (initiallyTakeable.indexOf(node) > -1 && nodeStatus === 'inactive') {
            window[node].status = 'takeable';
        } else {
            window[node].status = nodeStatus;
        }

        // Check the courses with FCE reqs
        if (window[node].hybrid === false) {
            if (window[node].status === 'active' || window[node].status === 'overridden') {
                updateClickedCourses(node, true);
            }
        }
    });

    $.each(nodes, function (i, node) {
        window[node].updateSVG();
        $.each(window[node].outEdges, function (i, edge) {
            edge.updateStatus();
        });
    });

    updateFCECount();
    clearActiveFocus();

    // only run this if the CSC graph is loaded
    if (getCookie('active-graph') === '1') {
        csc318.updateStatus();
        csc454.updateStatus();
    }
}


/**
 * Resets graph to default (no Nodes selected).
 */
function reset() {
    'use strict';

    // Set initial node status
    $.each(nodes, function (i, node) {
        if (initiallyTakeable.indexOf(node) > -1) {
            window[node].status = 'takeable';
        } else {
            window[node].status = 'inactive';
        }
        setCookie(window[node].id, window[node].status);

        window[node].updateSVG();
    });

    $('path').attr('data-active', 'inactive');

    $('.region').removeAttr('data-active');

    clearActiveFocus();

    clearFCECount();
    updateFCECount();
    fillFCECount();
}


/**
 * Removes all tool-tips.
 */
function removeToolTips() {
    'use strict';

    $('.tooltip-group').remove();
}


/**
 * Clears all timeouts.
 */
function clearAllTimeouts() {
    'use strict';

    for (var i = 0; i < timeouts.length; i++) {
        clearTimeout(timeouts[i]);
    }

    timeouts = [];
}

