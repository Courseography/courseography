/* Section class */
function Section(section, times, course) {
    this.course = course;
    this.name = $(section).html();
    this.id = $(section).attr("id");
    this.session = getSession(section);
    this.type = this.name.charAt(0);
    this.times = times;
    this.courseName = this.id.substring(0, 8);
}


function getSession(section) {
    if (isYearSection(section)) {
        return "Y";
    } else if (isFallSection(section)) {
        return "F";
    } else if (isSpringSection(section)) {
        return "S";
    }
}


function isYearSection(section) {
    return $(section.parentNode).hasClass("sectionList-Y");
}


function isFallSection(section) {
    return $(section.parentNode).hasClass("sectionList-F");
}


function isSpringSection(section) {
    return $(section.parentNode).hasClass("sectionList-S");
}


function getType(section) {
    return $(section).html().charAt(0);
}




function Course(name) {
    var course = getCourse(name);
    // Copy attributes
    this.F = course.F;
    this.S = course.S;
    this.Y = course.Y
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

function sameSession(section1, section2) {
    return section1 !== undefined &&
           section2 !== undefined &&
           section1.session == section2.session;
}
