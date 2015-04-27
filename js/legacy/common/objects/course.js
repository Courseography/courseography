'use strict';

import $ from 'jquery';

import { getCourse, convertTimes, cleanUpTimes } from '../utilities/util';
import { makeLecture } from './section';

/**
 * Constructs a Course.
 * @param {string} name The course code.
 * @constructor
 */
export function Course(name) {
    'use strict';
    var course = getCourse(name);
    // Copy attributes
    this.F = course.F;
    this.S = course.S;
    this.Y = course.Y;
    this.name = course.name;
    this.title = course.title;
    this.prereqs = course.prereqs;
    this.prereqString = course.prereqString;
    this.breadth = course.breadth;
    this.prep = course.prep;
    this.description = course.description;
    this.exclusions = course.exclusions;
    this.distribution = course.distribution;
    this.manualTutorialEnrolment = course.manualTutorialEnrolment;
    this.manualPracticalEnrolment = course.manualPracticalEnrolment;

    // Create sections
    this.parseSessions(course);

    this.selected = {'L': undefined, 'T': undefined, 'P': undefined};

    this.status = 'inactive';

    this.manual = {'T': this.manualTutorialEnrolment, 'P': this.manualPracticalEnrolment};
}


/* Section initialization */
/**
 * Parses this Course's sessions.
 * @param {JSON} course The object from which the sessions are retrieved.
 */
Course.prototype.parseSessions = function (course) {
    'use strict';

    // In the long run, maybe initialize to []
    this.sections = {'F': undefined, 'S': undefined, 'Y': undefined};
    var tmp = this;
    $.each(['F', 'S', 'Y'], function (i, s) {
        if (course[s] !== undefined) {
            tmp.sections[s] = tmp.parseSections(course[s], s);
        }
    });
};


/**
 * Parses this Course's sections.
 * @param {string} session The session of the section.
 * @param {string} timeSuffix The suffix of the section ID.
 * @returns {Section[]} This Course's Sections.
 */
Course.prototype.parseSections = function (session, timeSuffix) {
    'use strict';

    return this.parseLectures(session, timeSuffix)
           .concat(this.parseTutorials(session, timeSuffix));
};


/**
 * Parses this Course's lectures.
 * @param {string} session The session of the lecture.
 * @param {string} timeSuffix The suffix of the section ID.
 * @returns {Section[]} The lecture's Sections.
 */
Course.prototype.parseLectures = function (session, timeSuffix) {
    'use strict';

    var tmp = this;

    var sectionTimes = [];
    var sections = [];

    session.lectures.forEach(function (lecture, i, arr) {
        if (lecture.section.charAt(1) === '2' ||
            lecture.time === 'Online Web Version') {
            return;
        }

        sectionTimes = [];

        var id = tmp.name + '-' + lecture.section + '-' + timeSuffix;
        sectionTimes = sectionTimes.concat(convertTimes(lecture.time));

        if (timeSuffix === 'Y') {
            sectionTimes = sectionTimes.map(function (t) {
                                              return '#' + t + 'F';
                                       })
                                       .concat(sectionTimes.map(
                                        function (t) {
                                              return '#' + t + 'S';
                                       }));
        } else {
            sectionTimes = sectionTimes.map(function (time) {
                return '#' + time + timeSuffix;
            });
        }

        sectionTimes = cleanUpTimes(sectionTimes);

        sections.push(makeLecture(lecture, tmp, id, sectionTimes));
    });

    return sections;
};


/**
 * Parses this Course's tutorials.
 * @param {string} session The session of the tutorial.
 * @param {string} timeSuffix The suffix of the section ID.
 * @returns {Section[]} The tutorial's Sections.
 */
Course.prototype.parseTutorials = function (session, timeSuffix) {
    'use strict';

    if (this.manualTutorialEnrolment === false && this.manualPractialEnrolment === false) {
        return [];
    }

    var tmp = this;
    var tutorials = [];
    var i;
    for (i = 0; i < session.tutorials.length; i++) {
        if (!inArray(session.tutorials[i], tutorials)) {
            tutorials.push(session.tutorials[i]);
        }
    }

    return tutorials.map(function (tutorial) {
        var sectionTimes = convertTimes(tutorial[1]);
        if (timeSuffix === 'Y') {
            sectionTimes = sectionTimes.map(function (t) {
                                              return '#' + t + 'F';
                                       })
                                       .concat(sectionTimes.map(
                                       function (t) {
                                              return "#" + t + "S";
                                       }));
        } else {
            sectionTimes = sectionTimes.map(function (time) {
                return '#' + time + timeSuffix;
            });
        }

        var id = tmp.name + '-' + tutorial[0] + '-' + timeSuffix;
        sectionTimes = cleanUpTimes(sectionTimes);
        return makeTutorial(tutorial, tmp, id, sectionTimes);
    });
};


/* Manipulate course sections */
/**
 * Activates a section of this Course.
 * @param {Section} section The Section being activated.
 */
Course.prototype.activateSection = function (section) {
    'use strict';

    // Check if section was already selected
    var curr = this.selected[section.type];
    if (curr !== undefined) {
        this.removeSection(curr);
        if (curr.id !== section.id) {
            this.addSection(section);
        }
    } else {
        this.addSection(section);
    }
};


/**
 * Adds a section to this Course.
 * @param {Section} section The section being added.
 */
Course.prototype.addSection = function (section) {
    'use strict';

    this.selected[section.type] = section;

    section.clicked = true;
    this.selectTimes(section);
    updateSelectedLectures(section.id);
};


/**
 * Selects the times of section for this Course.
 * @param {Section} section The section whose times are being selected.
 */
Course.prototype.selectTimes = function (section) {
    'use strict';

    $.each(section.times, function (i, time) {

        var n = time.charAt(time.length - 1);

        if (n === 'H') {
            extendCell(parseInt(time.slice(2)), time.charAt(1), time.charAt(time.length - 2));
            ptime = previousCell(time);
            if ($(ptime).attr('clicked') === 'true') {
                section.setConflictTime(time);
            }
        }

        if (n === 'E') {
            extendCell(parseInt(time.slice(2)), time.charAt(1), time.charAt(time.length - 2));
            time = time.slice(0, time.length - 1);
        }

        if ($(time).attr('rowspan') !== '2' && n !== 'H' && n !== 'E') {
            section.setConflictTime(time);
        }

        if ($(time).attr('clicked') !== 'true') {
            section.setTime(time);
        } else {
            section.setConflictTime(time);
        }

        var ptime = previousCell(time);
        if ($(ptime).html() !== '') {
            $(time).removeClass('timetable-edge')
                   .addClass('timetable-middle');
        }

    });
};


/**
 * Removes section from the grid.
 * @param {Section} section The Section being removed.
 */
Course.prototype.removeSection = function (section) {
    'use strict';

    section.removeTimes();
    removeFromArray(section.id, selectedSections);
    this.selected[section.type] = undefined;
    section.clicked = false;
};


/**
 * Returns and updates whether this course is satisfied.
 * @returns {boolean} Whether this course is satisfied.
 */
Course.prototype.updateSatisfaction = function () {
    'use strict';

    if (!this.manual.T && !this.manual.P) {
        this.satisfied = true;
    } else if (!this.manual.P &&
               sameSession(this.selected.L, this.selected.T)) {
        this.satisfied = true;
    } else if (!this.manual.T &&
               sameSession(this.selected.L, this.selected.P)) {
        this.satisfied = true;
    } else if (sameSession(this.selected.L, this.selected.T) &&
               sameSession(this.selected.L, this.selected.P)) {
        this.satisfied = true;
    } else {
        this.satisfied = false;
    }

    var sat = this.satisfied;
    var sections = [];
    var tmp = this;
    $.each(['F', 'S', 'Y'], function (i, session) {
        if (tmp.sections[session] !== undefined) {
            sections = sections.concat(tmp.sections[session]);
        }
    });
    $.each(sections,
           function (i, section) {
                if (section !== undefined) {
                    section.satisfied = sat;
                }
           });

    return this.satisfied;
};


// Rendering methods (manipulate/return DOM elements)
/**
 * Renders this Course.
 * @returns {HTMLElement} The list element representing this Course.
 */
Course.prototype.render = function () {
    'use strict';

    var entry = document.createElement('li');
    entry.id = this.name + '-li';

    var header = this.renderHeader();
    entry.appendChild(header);
    this.header = header; // Still necessary...?

    var sections = this.renderSessions();

    entry.appendChild(sections);
    $(entry).accordion({
        heightStyle: 'content',
        collapsible: true,
        active: false
    });

    $(entry).attr('satisfied', '' + this.satisfied);

    return entry;
};


/**
 * Updates this Course in the grid.
 */
Course.prototype.renderUpdate = function () {
    'use strict';

    $('#' + this.id + '-li').attr('satisfied', '' + this.satisfied);
    var tmp = this;
    $.each(['F', 'S', 'Y'], function (i, session) {
        if (tmp.sections[session] !== undefined) {
            $.each(tmp.sections[session], function (ind, section) {
                if (section !== undefined) {
                    section.renderUpdate();
                }
            });
        }
    });
};


/**
 * Returns and renders this Course's header element.
 * @returns {HTMLElement} This course's header element.
 */
Course.prototype.renderHeader = function () {
    'use strict';

    var headerDiv = document.createElement('div');
    var header = document.createElement('h3');
    header.appendChild(document.createTextNode(this.name));

    var tmp = this;
    $(header).mouseover(function () {
                renderDisplayCourseTitle(tmp);
             })
             .mouseout(function () {
                 renderClearCourseInformation();
             });


    var iconDiv = document.createElement('div');
    $(iconDiv).addClass('icon-div');
    var courseImg = document.createElement('img');
    $(courseImg).attr('src', 'static/res/ico/delete.ico')
                .addClass('close-icon')
                .click(function () {
                    removeCourseFromList(tmp.name);
                });
    var aboutImg = document.createElement('img');
    $(aboutImg).attr('src', 'static/res/ico/about.png')
               .addClass('close-icon')
               .click(function () {
                   openModal(getCourseTitle(tmp.name), createModalDiv(tmp.name));
               });
    iconDiv.appendChild(courseImg);
    iconDiv.appendChild(aboutImg);
    headerDiv.appendChild(iconDiv);
    headerDiv.appendChild(header);

    return headerDiv;
};


/**
 * Renders this Course's sessions in the grid.
 * @returns {HTMLElement} This Course's sessions.
 */
Course.prototype.renderSessions = function () {
    'use strict';

    var sessionDiv = document.createElement('div');
    $(sessionDiv).addClass('sections');
    var course = this;
    $.each(['Y', 'F', 'S'], function (i, session) {
        if (course.sections[session] !== undefined) {
            sessionDiv.appendChild(course.renderSections(session));
        }

    });
    return sessionDiv;
};


/**
 * Renders this Course's Sections from session in the grid.
 * @param {string} session The session of the sections.
 * @returns {undefined|HTMLElement} The updated list of sections.
 */
Course.prototype.renderSections = function (session) {
    'use strict';

    var sections = this.sections[session];
    if (sections === undefined) {
        return undefined;
    } else {
        var sectionList = document.createElement('ul');
        $(sectionList).addClass('sectionList-' + session);
        $.each(sections, function(i, section) {
            sectionList.appendChild(section.render());
        });
        return sectionList;
    }
};


/**
 * Renders this Course's satisfaction attribute.
 */
Course.prototype.renderSatisfaction = function () {
    'use strict';

    var tmp = this;
    $.each(['L', 'T', 'P'], function (i, session) {
        var section = tmp.selected[session];
        if (section !== undefined) {
            $.each(section.times, function (i, time) {
                $(time).attr('satisfied', tmp.satisfied);
            });
        }
    });
};


/**
 * Updates the rendering of this Course's header.
 */
Course.prototype.renderUpdatedHeader = function () {
    'use strict';

    $('#' + this.name + '-li' + ' h3').attr('taken',
                                            this.selected.L !== undefined ||
                                            this.selected.T !== undefined ||
                                            this.selected.P !== undefined)
                                      .attr('satisfied', this.satisfied);
};


/**
 * Returns this Course's selected section times.
 * @returns {string[]} This Course's selected section times.
 */
Course.prototype.getSectionTimes = function () {
    'use strict';

    var sectionTimes = [];
    var course = this;
    $.each(['L', 'T', 'P'], function (i, type) {
        if (course.selected[type] !== undefined) {
            sectionTimes = sectionTimes.concat(course.selected[type].times);
        }
    });
    return sectionTimes;
};
