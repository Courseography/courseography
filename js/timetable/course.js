/* Course class */

function Course(name) {
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
    // Create sections
    this.parseSessions(course);

    this.selected = {"L": undefined, "T": undefined, "P": undefined};

    this.status = "inactive";

    if (course.manualTutorialEnrolment) {
        if (course.Y !== undefined) {
            this.practicalEnrolment = course.Y.tutorials.some(hasManualPractical);
            this.tutorialEnrolment = course.Y.tutorials.some(hasManualTutorial);
        } else if (course.F !== undefined) {
            this.practicalEnrolment = course.F.tutorials.some(hasManualPractical);
            this.tutorialEnrolment = course.F.tutorials.some(hasManualTutorial);
        } else {
            this.practicalEnrolment = course.S.tutorials.some(hasManualPractical);
            this.tutorialEnrolment = course.S.tutorials.some(hasManualTutorial);
        }
    } else {
        this.practicalEnrolment = false;
        this.tutorialEnrolment = false;
    }

    this.manual = {"T": this.tutorialEnrolment, "P": this.practicalEnrolment};
}


/* Section initialization */
Course.prototype.parseSessions = function (course) {
    // In the long run, maybe initialize to []
    this.sections = {"F": undefined, "S": undefined, "Y": undefined};
    var tmp = this;
    $.each(["F", "S", "Y"], function (i, s) {
        if (course[s] !== undefined) {
            tmp.sections[s] = tmp.parseSections(course[s], s);
        }
    })
}


Course.prototype.parseSections = function(session, timeSuffix) {
    return this.parseLectures(session, timeSuffix)
           .concat(this.parseTutorials(session, timeSuffix));
}


Course.prototype.parseLectures = function (session, timeSuffix) {
    var tmp = this;

    return session.lectures.filter(function (lecture) {
                return lecture.section.charAt(1) !== "2" &&
                       lecture.time !== "Online Web Version";
            }).map(function (lecture, i) {
                var id = tmp.name + "-" + lecture.section + "-" + timeSuffix;
                var sectionTimes = convertTimes(lecture.time);
                if (!tmp.manualTutorialEnrolment && session.tutorials.length > 0) {
                    sectionTimes = sectionTimes.concat(
                        convertTimes(session.tutorials[i][0]));
                }
                if (timeSuffix === "Y") {
                    sectionTimes = sectionTimes.map(function (t) {
                                                      return "#" + t + "F";
                                               })
                                               .concat(sectionTimes.map(
                                                function (t) {
                                                      return "#" + t + "S";
                                               }));
                } else {
                    sectionTimes = sectionTimes.map(function (time) {
                        return "#" + time + timeSuffix;
                    });
                }
                return makeLecture(lecture, tmp, id, sectionTimes);
            });
}


Course.prototype.parseTutorials = function (session, timeSuffix) {
    if (!this.manualTutorialEnrolment) {
        return [];
    } else {
        var tmp = this;
        return session.tutorials.map(function (tutorial) {
            var sectionTimes = convertTimes(tutorial[1]);
            if (timeSuffix === "Y") {
                sectionTimes = sectionTimes.map(function (t) {
                                                  return "#" + t + "F";
                                           })
                                           .concat(function (t) {
                                                  return "#" + t + "S";
                                           });
            } else {
                sectionTimes = sectionTimes.map(function (time) {
                    return "#" + time + timeSuffix;
                });
            }

            var id = tmp.name + "-" + tutorial[0] + "-" + timeSuffix;
            return makeTutorial(tutorial, tmp, id, sectionTimes);
        });
    }
}


/* Manipulate course sections */
Course.prototype.activateSection = function (section) {

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
}


Course.prototype.addSection = function (section) {
    var type = section.type;
    this.selected[section.type] = section;

    section.clicked = true;
    this.selectTimes(section);
}


Course.prototype.selectTimes = function (section) {
    $.each(section.times, function (i, time) {
        if ($(time).attr("clicked") !== "true") {
            section.setTime(time);
        } else {
            section.setConflictTime(time);
        }
    });
}


Course.prototype.removeSection = function (section) {
    section.removeTimes();
    removeFromArray(section, selectedLectures);
    this.selected[section.type] = undefined;
    section.clicked = false;
}


Course.prototype.updateSatisfaction = function () {
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
    $.each(["F", "S", "Y"], function (i, session) {
        if (tmp.sections[session] !== undefined) {
            sections = sections.concat(tmp.sections[session]);
        }
    })
    $.each(sections,
           function (i, section) {
                if (section !== undefined) {
                    section.satisfied = sat;
                }
           });

    return this.satisfied;
}


// Rendering methods (manipulate/return DOM elements)
Course.prototype.render = function () {
    var entry = document.createElement("li");
    entry.id = this.name + "-li";

    var header = this.renderHeader();
    entry.appendChild(header);
    this.header = header; // Still necessary...?

    var sections = this.renderSessions();

    entry.appendChild(sections);
    $(entry).accordion({
        heightStyle: "content",
        collapsible: true,
        active: false
    });

    $(entry).attr("satisfied", "" + this.satisfied);

    return entry;
}


Course.prototype.renderUpdate = function () {
    $("#" + this.id + "-li").attr("satisfied", "" + this.satisfied);
    var tmp = this;
    $.each(["F", "S", "Y"], function (i, session) {
        if (tmp.sections[session] !== undefined) {
            $.each(tmp.sections[session], function (ind, section) {
                if (section !== undefined) {
                    section.renderUpdate();
                }
            });
        }
    });
}


Course.prototype.renderHeader = function () {
    var header = document.createElement("h3");
    header.appendChild(document.createTextNode(this.name));

    var tmp = this;
    $(header).mouseover(function () {
                renderDisplayCourseTitle(tmp);
             })
             .mouseout(function () {
                 renderClearCourseInformation();
             });


    var courseImg = document.createElement("img");
    $(courseImg).attr("src", "res/ico/delete.ico")
                .addClass("close-icon")
                .click(function () {
                    removeCourseFromList(tmp.name);
                });
    header.appendChild(courseImg);

    return header;
}


Course.prototype.renderSessions = function () {
    var sessionDiv = document.createElement("div");
    $(sessionDiv).addClass("sections");
    var course = this;
    $.each(["Y", "F", "S"], function (i, session) {
        if (course.sections[session] !== undefined) {
            sessionDiv.appendChild(course.renderSections(session));
        }

    });
    return sessionDiv;
}


Course.prototype.renderSections = function (session) {
    var sections = this.sections[session];
    if (sections === undefined) {
        return undefined;
    } else {
        var sectionList = document.createElement("ul");
        $(sectionList).addClass("sectionList-" + session);
        $.each(sections, function(i, section) {
            sectionList.appendChild(section.render());
        });
        return sectionList;
    }
}


Course.prototype.renderSatisfaction = function () {
    var tmp = this;
    $.each(["L", "T", "P"], function (i, session) {
        var section = tmp.selected[session];
        if (section !== undefined) {
            $.each(section.times, function (i, time) {
                $(time).attr("satisfied", tmp.satisfied);
            });
        }
    });
}


Course.prototype.renderUpdatedHeader = function () {
    $("#" + this.name + "-li" + " h3").attr("taken",
                                            this.selected.L !== undefined ||
                                            this.selected.T !== undefined ||
                                            this.selected.P !== undefined)
                                      .attr("satisfied", this.satisfied);
}


Course.prototype.sectionTimes = function () {
    var sectionTimes = [];
    var course = this;
    $.each(["L", "T", "P"], function (i, type) {
        if (course.selected[type] !== undefined) {
            sectionTimes = sectionTimes.concat(course.selected[type].times);
        }
    });
    return sectionTimes;
}

function sameSession(section1, section2) {
    return section1 !== undefined &&
           section2 !== undefined &&
           section1.session == section2.session;
}
