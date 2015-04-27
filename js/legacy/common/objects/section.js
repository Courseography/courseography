/**
 * Constructs a Section.
 * @param {string[]} times The times that this Section is offered at.
 * @param {Course} course The Course of this Section.
 * @param {string} id The ID of this Section.
 * @constructor
 */
function Section(times, course, id) {
    'use strict';

    this.id = id;
    this.name = this.id.substring(9, 14);
    this.type = this.name.charAt(0);
    this.courseName = this.id.substring(0, 8);
    this.session = this.id.substring(15, 16);
    this.course = course;
    this.times = times;
    this.clicked = false;
    this.satisfied = true;
}


// Mouse events
/**
 * Sets this Section's mouse events.
 */
Section.prototype.setMouseEvents = function (li) {
    'use strict';

    var tmp = this;
    $(li).mouseout(function () {
             tmp.mouseout();
             //tmp.course.renderUpdate();
         })
         .mouseover(function () {
             tmp.mouseover();
             //tmp.course.renderUpdate();
         })
         .click(function () {
             tmp.onclick();
             tmp.course.renderUpdate();
         });
};


/**
 * Sets this Section's mouseout event.
 */
Section.prototype.mouseout = function () {
    'use strict';

    $.each(this.times, function (i, time) {
        renderClearHover(time);
    });
    renderClearCourseInformation();
};


/**
 * Sets this Section's mouseover event.
 */
Section.prototype.mouseover = function () {
    'use strict';

    var tmp = this;
    $.each(this.times, function (i, time) {
        renderAddHover(time, tmp);

    });
    renderDisplayCourseInformation(this.course);
    renderDisplaySectionInformation(this);
};


/**
 * Sets this Section's click event.
 */
Section.prototype.onclick = function () {
    'use strict';

    $.each(this.times, function (i, time) {
        renderClearHover(time);
    });

    var course = this.course;

    course.activateSection(this);
    course.updateSatisfaction();
    course.renderSatisfaction();
    course.renderUpdatedHeader();

    saveCookies(selectedCourses, selectedSections);
    alertUserOfConflict();
};


/**
 * Sets one of this Section's times.
 * @param {string} time The time's cell ID.
 * TODO: Rename parameter.
 */
Section.prototype.setTime = function (time) {
    'use strict';

    $(time).html(this.courseName.substring(0, 6) + ' (' + this.type + ')')
           .data('courseName', this.courseName)
           .attr("clicked", "true")
           .attr("type", this.type)
           .addClass('timetable-edge');
};


/**
 * Sets one of this Section's times as in conflict.
 * @param {string} time The time's cell ID.
 * TODO: Rename parameter.
 */
Section.prototype.setConflictTime = function (time) {
    'use strict';

    var conflicts = $(time).data("conflicts");
    conflicts.push(this);
    renderConflicts(time, conflicts);
};


/**
 * Removes this Section's times.
 */
Section.prototype.removeTimes = function () {
    'use strict';

    var tmp = this;
    $.each(this.times, function (i, time) {
        var n = time.charAt(time.length - 1);

        if (n === 'E') {
            compressCell(parseInt(time.slice(2)), time.charAt(1), time.charAt(time.length - 2));
            time = time.slice(0, time.length - 1);
        }

        $(time).removeClass('timetable-edge')
               .removeClass('timetable-middle');

        if (n === 'H') {
            compressCell(parseInt(time.slice(2)), time.charAt(1), time.charAt(time.length - 2));
        }

        if ($(time).data("conflicts").length > 0) {
            tmp.removeConflict(time);
        } else {
            renderClearTime(time);
        }
    });
};


/**
 * Removes one of this Section's times in conflict status.
 * @param {string} time The time's cell ID.
 * TODO: Rename parameter.
 */
Section.prototype.removeConflict = function (time) {
    'use strict';

    var conflicts = $(time).data("conflicts");
    var index = $.inArray(this, conflicts);

    if (index === -1) {
        $(time).html(conflicts[0].courseName.substring(0,6) +
                     ' (' + conflicts[0].type + ')')
               .attr("type", conflicts[0].type);
        conflicts.splice(0, 1);
    } else {
        conflicts.splice(index, 1);
    }

    renderConflicts(time, conflicts);
};


// Rendering
/**
 * Renders this Section, returns this Section's list item representation.
 * @returns {HTMLElement} This Section's list item representation.
 */
Section.prototype.render = function () {
    'use strict';

    var li = document.createElement("li");
    $(li).attr("id", this.id)
         .data("instructor", this.instructor)
         .data("cap", this.cap)
         .data("enrol", this.enrol)
         .data("wait", this.wait)
         .attr("clicked", String(this.clicked))
         .attr("satisfied", String(this.satisfied));
    li.appendChild(document.createTextNode(this.name));
    this.setMouseEvents(li);
    return li;
};


/**
 * Updates the rendering of this Section in the grid.
 */
Section.prototype.renderUpdate = function () {
    'use strict';

    $("#" + this.id).attr("clicked", String(this.clicked))
                    .attr("satisfied", String(this.satisfied));
};


// Other constructors
/**
 * Makes a lecture Section.
 * @param {object} lecture The lecture.
 * @param {object} course The course.
 * @param {string} id The Section ID.
 * @param {string[]} sectionTimes The Section's times.
 * @returns {Section} The lecture Section.
 */
export function makeLecture(lecture, course, id, sectionTimes) {
    'use strict';

    var section = new Section(sectionTimes, course, id);
    section.instructor = lecture.instructor;
    section.cap = lecture.cap;
    section.enrol = lecture.enrol;
    section.wait = lecture.wait;
    return section;
}


/**
 * Makes a tutorial Section.
 * @param {Array} tutorial The tutorial.
 * @param course The Course.
 * @param {string} id The Section's ID.
 * @param {string[]} sectionTimes The Section's times.
 * @returns {Section} The tutorial Section.
 */
function makeTutorial(tutorial, course, id, sectionTimes) {
    'use strict';

    var section = new Section(sectionTimes, course, id);
    section.cap = tutorial[3];
    section.enrol = tutorial[4];
    section.wait = tutorial[5];
    return section;
}

/**
 * Returns whether two Sections are in the same session.
 * @param {Section} section1 The first section.
 * @param {Section} section2 The second section.
 * @returns {boolean} Whether two sections are in the same session.
 */
function sameSession(section1, section2) {
    'use strict';

    return section1 !== undefined &&
        section2 !== undefined &&
        section1.session === section2.session;
}
