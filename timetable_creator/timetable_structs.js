function Course(title) {
	var title           = title;
	var lectures        = [];
	var tutorials       = [];
	var fall            = false;
	var spring          = false;
	var year            = false;
}
function Lecture(title, time, assignedTutorial) {
	var title           = title;
	var time            = time;
	var lectureTutorial = assignedTutorial;
	var hasTutorial     = (assignedTutorial !== null);
}

function Tutorial(title, time) {
	var title = title;
	var time  = time;
}

Course.prototype.addLectureSection = function(title, time, assignedTutorial) {
	var lecture = new Lecture(title, time, assignedTutorial);
	//this.lectures.push(lecture);
}

Course.prototype.addTutorialSection = function(title, time) {
	var tutorial = new Tutorial(title, time);
	this.tutorials.push(tutorial);
}
alert("structs loaded");