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
        updateFCECount();

        // Check the courses with FCE reqs
        $.each(FCEPrerequisiteCourses, function (i, course) {
            course.updateStatus();
        });

        updatePOSt(id, window[id].isSelected());
        updatePostInterface();
        updateMajorPostInterface();
        updateMinorPostInterface();
    }
}