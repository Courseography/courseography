function Course(title) {
	this.title           = title;
	this.lectures        = [];
	this.tutorials       = [];
	this.fall            = false;
	this.spring          = false;
	this.year            = false;
}

function Lecture(title, time, assignedTutorial) {
	this.title           = title;
	this.time            = time;
	this.lectureTutorial = assignedTutorial;
	this.hasTutorial     = (assignedTutorial !== null);
}

function Tutorial(title, time) {
	this.title = title;
	this.time  = time;
}

Course.prototype.addLectureSection = function(title, time, assignedTutorial) {
	var lecture = new Lecture(title, time, assignedTutorial);
	this.lectures.push(lecture);//error sometimes, but not others?
}

Course.prototype.addTutorialSection = function(title, time) {
	var tutorial = new Tutorial(title, time);
	this.tutorials.push(tutorial);
}