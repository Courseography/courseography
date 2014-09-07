function setMouseCallbacks() {
    'use strict';

    var nodeObjects = $('.node');

    nodeObjects.click(function (event) {

        // TODO: Test as click(turnNode)
        turnNode(event)
    })
        .hover(
            function (event) {
                hoverFocus(event);
            },
            function (event) {
                hoverUnfocus(event);
            });
}


// Activates missing prerequisite display and
// fetches course description on hover
function hoverFocus(event) {
    'use strict';

    if ($(".modal").length === 0 && activeFocus === '') {
        var id = event.target.parentNode.id;
        // Highlight missing prerequisites
        window[id].focus();

        removeToolTips();
        displayToolTip(id);
    }
}


// Deactivate missing prerequisites
function hoverUnfocus(event) {
    'use strict';

    if ($(".modal").length === 0) {
        var id = event.target.parentNode.id;
        window[id].unfocus();
    }

    var timeout = setTimeout(function () {
        $("." + id + "-tooltip-rect").hide('slow', function () {
            $(this).remove();
        });
        $("." + id + "-tooltip-text").hide('slow', function () {
            $(this).remove();
        });
    }, 500);
    timeouts.push(timeout);
}


// Activate/Deactivate node when clicked
function turnNode(event) {
    'use strict';

    if (activeFocus === '' && $(".modal").length === 0) {
        var id = event.target.parentNode.id;
        // Update this node
        window[id].turn();
        updateClickedCourses(id, window[id].isSelected());
        updateFCECount();

        // Check the courses with FCE reqs
        CSC318.updateStatus();
        CSC454.updateStatus();
        CSC494.updateStatus();
        CSC495.updateStatus();

        updatePOSt(id, window[id].isSelected());
        updatePostInterface();
        updateMajorPostInterface();
        updateMinorPostInterface();
    }
}
