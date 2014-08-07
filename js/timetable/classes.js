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
}
