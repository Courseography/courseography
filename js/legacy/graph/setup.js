'use strict';

import $ from 'jquery';

import { getCookie } from '../common/cookie_handler';
import { loadGraph } from './sidebar/sidebar_events';

// Globals
global.timetable = 'static/res/timetable.html';
global.clickedCourses = [];
global.FCEs = 0;
global.FCEs100 = 0;
global.FCEs200 = 0;
global.FCEs300 = 0;
global.FCEs400 = 0;
global.FCEsMAT = 0;

global.activeFocus = '';         // The active focus from the 'Focuses' tab.
global.timeouts = [];            // All timeouts. Used to remove timeouts later on
global.FCEPrerequisiteCourses;   // Courses that have FCE based prerequisites,
                              // initialised at runtime
global.courseCache = [];         // Cached Courses. Courses are cached to
                              // minimise AJAX requests

global.nodes = [];               // List of all nodes

$(document).ready(function () {
    'use strict';

    var active = getCookie('active-graph');

    if (active !== '') {
        loadGraph(active);
    } else {
        loadGraph('1');
    }

    $('#fcecount').hide();
});

/**
 * Retrieves an SVG file.
 * @param {string} The relative filepath of the graph
 * @returns {string} An SVG string representing a graph.
 */
export function getRemote(filepath) {
    'use strict';

    var SVG = $.ajax({
        type: 'GET',
        url: filepath,
        async: false
    }).responseText;
    $('#graph').append(SVG);
}
