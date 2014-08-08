/* Section class */
function Section(section, times, course, id) {
    if (section !== undefined) {
        this.id = $(section).attr("id");
    } else {
        this.id = id;
    }

    this.name = this.id.substring(9, 14);
    this.session = this.id.substring(15, 16);
    this.course = course; // Not sure if this is necessary...
    this.type = this.name.charAt(0);
    this.times = times;
    this.courseName = this.id.substring(0, 8);
}


// Rendering
Section.prototype.render = function () {
    var li = document.createElement("li");
    $(li).data("instructor", this.instructor)
              .data("cap", this.cap)
              .data("enrol", this.enrol)
              .data("wait", this.wait);
    li.appendChild(document.createTextNode(this.name));
    setSectionMouseEvents(li, this.times, this.course);
    $(li).attr("id", this.id);
    return li;
}


// TODO: remove getType and getCourseName
function getType(section) {
    return $(section).html().charAt(0);
}


function getCourseName(section) {
    return section.id.substring(0, 8);
}


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
    this.isLectureSelected = false;
    this.isTutorialSelected = false;
    this.isPracticalSelected = false;

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
    }

    this.manual = {"T": this.tutorialEnrolment, "P": this.practicalEnrolment};
}


/* Section initialization */
Course.prototype.parseSessions = function (course) {
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

    var t = session.lectures.filter(function (lecture) {
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
                                       .concat(function (t) {
                                              return "#" + t + "S";
                                       });
        } else {
            sectionTimes = sectionTimes.map(function (time) {
                return "#" + time + timeSuffix;
            });
        }
        return makeLecture(lecture, tmp, id, sectionTimes);
    });
    return t;
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


function makeLecture(lecture, course, id, sectionTimes) {
    var section = new Section(undefined, sectionTimes, course, id);
    section.instructor = lecture.instructor;
    section.cap = lecture.cap;
    section.enrol = lecture.enrol;
    section.wait = lecture.wait;
    return section;
}


function makeTutorial(tutorial, course, id, sectionTimes) {
    var section = new Section(undefined, sectionTimes, course, id);
    section.cap = tutorial[3];
    section.enrol = tutorial[4];
    section.wait = tutorial[5];
    return section;
}

/* Manipulate course sections */
Course.prototype.clickSection = function (section, sectionTimes) {
    var type = getType(section);
    var id = $(section).attr("id");

    // Check if section was already selected
    var curr = this.selected[type];
    if (curr !== undefined) {
        this.removeSection(document.getElementById(curr.id));
        if (curr.id !== id) {
            this.addSection(section, sectionTimes);
        }
    } else {
        this.addSection(section, sectionTimes);
    }
}

Course.prototype.addSection = function (section, sectionTimes) {
    var type = getType(section);
    if (type === "L") {
        this.selectedLecture = section;
        this.selected.L = new Section(section, sectionTimes, this);
    } else if (type === "T") {
        this.selectedTutorial = section;
        this.selected.T = new Section(section, sectionTimes, this);
    } else if (type === "P") {
        this.selectedPractical = section;
        this.selected.P = new Section(section, sectionTimes, this);
    }

    this.setSession();
    $(section).attr("clicked", "true");
    selectUnselectedTimes(this, sectionTimes, section);
}


Course.prototype.removeSection = function (section) {
    var name = $(section).html();
    var type = name.charAt(0);
    removeSectionTimes(section, this.selected[type].times);

    $(section).attr("clicked", "false");

    if (type === "L") {
        removeFromArray(this.selected.L, selectedLectures);
        this.selected.L = undefined;
        this.selectedLecture = undefined;
    } else if (type === "T") {
        removeFromArray(this.selected.T, selectedLectures);
        this.selected.T = undefined;
        this.selectedTutorial = undefined;
    } else if (type === "P") {
        removeFromArray($(this.selectedPractical).attr("id"), selectedLectures);
        this.selected.P = undefined;
        this.selectedPractical = undefined;
    }
    this.setSession(section);
}


// TODO: Remove this.
Course.prototype.setSession = function (section) {
    var lec = this.selected.L;
    var tut = this.selected.T;
    var prac = this.selected.P;
    this.selectedLectureSession = lec === undefined? undefined : lec.session;
    this.selectedTutorialSession = tut === undefined? undefined : tut.session;
    this.selectedPracticalSession = prac === undefined? undefined : prac.session;

    this.selectedLectureTimes = lec === undefined? undefined : lec.times;
    this.selectedTutorialTimes = tut === undefined? undefined : tut.times;
    this.selectedPracticalTimes = prac === undefined? undefined : prac.times;

    this.isLectureSelected = lec !== undefined;
    this.isTutorialSelected = tut !== undefined;
    this.isPracticalSelected = prac !== undefined;
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
    return this.satisfied;
}


/* Rendering methods (manipulate/return DOM elements) */
// DOM Elements/manipulation
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

    return entry;
}


Course.prototype.renderHeader = function () {
    var header = document.createElement("h3");
    header.appendChild(document.createTextNode(this.name));

    $(header).mouseover(function () {
                displayCourseTitle(this);
             })
             .mouseout(function () {
                 clearCourseInformation();
             });


    var courseImg = document.createElement("img");
    $(courseImg).attr("src", "res/ico/delete.ico")
                .addClass("close-icon")
                .click(function () {
                    removeCourseFromList(this.name);
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
        $.each(sections, function(i, section) {
            sectionList.appendChild(section.render());
        })
        $(sectionList).addClass("sectionList-" + session);
        return sectionList;
    }
}


Course.prototype.renderSatisfaction = function () {
    if (this.selectedLectureTimes !== undefined) {
        $.each(this.selectedLectureTimes, function (i, time) {
            $(time).attr("satisfied", this.satisfied);
        });
        $(this.selectedLecture).attr("satisfied", this.satisfied);
    }

    if (this.selectedTutorialTimes !== undefined) {
        $.each(this.selectedTutorialTimes, function (i, time) {
            $(time).attr("satisfied", this.satisfied);
        });
        $(this.selectedTutorial).attr("satisfied", this.satisfied);
    }

    if (this.selectedPracticalTimes !== undefined) {
        $.each(this.selectedPracticalTimes, function (i, time) {
            $(time).attr("satisfied", this.satisfied);
        });
        $(this.selectedPractical).attr("satisfied", this.satisfied);
    }

    if (this.satisfied) {
        $("#" + this.name + "-li" + " li").attr("satisfied", true);
    }
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
    })
    return sectionTimes;
}

function sameSession(section1, section2) {
    return section1 !== undefined &&
           section2 !== undefined &&
           section1.session == section2.session;
}
