function setMouseCallbacks() {
    'use strict';

    var nodeObjects = $('.node');

    nodeObjects.click(function (event) {
        turnNode(event);
    });
    nodeObjects.hover(
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

    var id = event.target.parentNode.id;
    // Highlight missing prerequisites
    window[id].focus();
    // Fetch course description
    fetchCourseDescription(id);
}


// Deactivate missing prerequisites
function hoverUnfocus(event) {
    'use strict';

    var id = event.target.parentNode.id;
    window[id].unfocus();
}

// Activate/Deactivate node when clicked
function turnNode(event) {
    'use strict';

    if (activeFocus === '') {
        var id = event.target.parentNode.id;
        // Update this node
        window[id].turn();

        updateClickedCourses(id, window[id].isSelected());
        updateMyCoursesTab();
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