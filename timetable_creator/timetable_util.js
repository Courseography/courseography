
var day           = /M|T|W|R|F/;
var time          = /1|2|3|4|5|6|7|8|9/;
var timeSecondDig = /0|1|2/;
var hyphen        = /-/;
var result;
var i;
var contentString = "";

// In the future, it would be nice to pull the course values either from all json files, or the course nodes.
if (window.XMLHttpRequest) {
  xmlhttp=new XMLHttpRequest();
}
else {
  xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
}

xmlhttp.open("GET","../res/timetable2014.csv",false);
xmlhttp.send();
xmlDoc=xmlhttp.responseXML;

var httpResponse = xmlhttp.responseText;
var csvSplitNewline = httpResponse.split('\n');
var courseSelect = document.getElementById("course-select");

function addCourseToList(course) {
	var courseNode = getCourse(course);
	if(typeof(courseNode.F) !== "undefined" || typeof(courseNode.Y) !== "undefined") {
		var entry    = document.createElement('li');
		$(entry).css("border","2px solid black");
		var header   = document.createElement('h3');
		var sections = document.createElement('div');
		header.appendChild(document.createTextNode(course));
		entry.setAttribute("class", "course");
		sections.id = course + "Sections";
		var sectionList = document.createElement('ul');
		var fall = document.createElement("li");
		if(typeof(courseNode.F) !== "undefined") {
			$.each(courseNode.F.lectures, function(i, lecture){
				var section = document.createElement("li");
				section.setAttribute("class", "section");
				section.appendChild(document.createTextNode(lecture.section));
				var sectionTimes = getSectionTimeSlot(lecture, "lecture");
				if(courseNode.manualTutorialEnrolment === false) {
					sectionTimes = sectionTimes.concat(getSectionTimeSlot(courseNode.F.tutorials[i], "tutorial"));
				}
				$.each(sectionTimes, function(i, time) {
					$(section).click(function(){
						if(document.getElementById(time).getAttribute("clicked") === "true" && document.getElementById(time).innerHTML === courseNode.name) {
							document.getElementById(time).innerHTML = "";
							document.getElementById(time).setAttribute("clicked","false");
							$("#"+time).css("background-color", "black");
						} else if(document.getElementById(time).getAttribute("clicked") !== "true") {
							document.getElementById(time).innerHTML = courseNode.name;
							document.getElementById(time).setAttribute("clicked","true");
							$("#"+time).css("background-color", "blue");
						}
					});

					$(section).mouseover(function(){
						if(document.getElementById(time).getAttribute("clicked") === "true") {
							$("#"+time).css("background-color", "red");
						} else {
							document.getElementById(time).innerHTML = courseNode.name;
						}
					});

					$(section).mouseout(function(){
						if(document.getElementById(time).getAttribute("clicked") !== "true") {
							document.getElementById(time).innerHTML = "";
							$("#"+time).css("background-color", "black");	
						} else {
							$("#"+time).css("background-color", "blue");

						}
					});

				});

				sectionList.appendChild(section);
			});

		} else {

			$.each(courseNode.Y.lectures, function(i, lecture){
				var section = document.createElement("li");
				section.setAttribute("class", "section");
				section.appendChild(document.createTextNode(lecture.section));
				sectionList.appendChild(section);
			});

		}

		sections.appendChild(sectionList);
		entry.appendChild(header);
		entry.appendChild(sections);
		courseSelect.appendChild(entry);
		return entry;
	}
}


function getSectionTimeSlot(section, type) {
	var firstTimeSlot;
	var secondTimeSlot;
	var thirdTimeSlot;
	var timeSlots  = [];
	var times = [];
	if(type === "lecture") {
		console.log("input: " + section.time);
		times = section.time.split(',');
		console.log("times after: " + times);
	} else if (type === "tutorial" && typeof(section) !== "undefined") {
		times.push(section);
	}

	$.each(times, function(i, lectureTime) {
		console.log("iterating on: " +lectureTime);
		var timeLength = lectureTime.length;

		if(lectureTime.charAt(0).match(day) !== null) {
			firstTimeSlot = lectureTime.charAt(0);
		}

		if(lectureTime.charAt(1).match(day) !== null) {
			secondTimeSlot = lectureTime.charAt(1);

			if(lectureTime.charAt(2).match(day) !== null) {
				thirdTimeSlot  = lectureTime.charAt(2).match(day);
				firstTimeSlot  = firstTimeSlot  + lectureTime.charAt(3);
				secondTimeSlot = secondTimeSlot + lectureTime.charAt(3);
				thirdTimeSlot  = thirdTimeSlot  + lectureTime.charAt(3);

				if(lectureTime.charAt(3) === "1" && lectureTime.charAt(4).match(timeSecondDig)) {
					firstTimeSlot  = firstTimeSlot  + lectureTime.charAt(4);
					secondTimeSlot = secondTimeSlot + lectureTime.charAt(4);
					thirdTimeSlot  = thirdTimeSlot  + lectureTime.charAt(4);
				}

				timeSlots.push(firstTimeSlot);
				timeSlots.push(secondTimeSlot);
				timeSlots.push(thirdTimeSlot);
			} else {
				firstTimeSlot  = firstTimeSlot  + lectureTime.charAt(2);
				secondTimeSlot = secondTimeSlot + lectureTime.charAt(2);

				if(lectureTime.charAt(2) === "1"  && lectureTime.charAt(3).match(timeSecondDig)) {
					firstTimeSlot  = firstTimeSlot  + lectureTime.charAt(3);
					secondTimeSlot = secondTimeSlot + lectureTime.charAt(3);
				}

				timeSlots.push(firstTimeSlot);
				timeSlots.push(secondTimeSlot);
			}

		} else if(lectureTime.charAt(1).match(time)) {
			firstTimeSlot = firstTimeSlot + lectureTime.charAt(1);

			if(lectureTime.charAt(1) === "1" && lectureTime.charAt(2).match(timeSecondDig)) {
				firstTimeSlot = firstTimeSlot + lectureTime.charAt(2);

				if(timeLength > 2 && lectureTime.charAt(3).match(hyphen)) {
					console.log("parsed!:" + lectureTime.substring(1,3));
					console.log(timeLength + " " + firstTimeSlot.charAt(4) + " " + lectureTime);
					console.log(lectureTime.substring(1,3));
					console.log(lectureTime.substring(4,6));
					if(timeLength > 5) {
						var difference = lectureTime.substring(4,6) - lectureTime.substring(1,3);
						console.log(lectureTime + " " + difference);

					} else {
						var difference = lectureTime.charAt(1) - (lectureTime.substring(1,3) - 12);
					}

					for(var i = 0; i < difference; i++) {
						var newTime = parseInt(firstTimeSlot.substring(1,3)) + 1;
						if(newTime > 12) {
							newTime = newTime - 12;
						}
						firstTimeSlot = firstTimeSlot.charAt(0) + newTime;
						timeSlots.push(firstTimeSlot);
					}
				} else {
					timeSlots.push(firstTimeSlot);
			}
			} else if(timeLength === 2) {
				timeSlots.push(firstTimeSlot);
			}
		}


		if(timeLength > 2 && lectureTime.charAt(2).match(hyphen)) {
			timeSlots.push(firstTimeSlot);
			var difference = lectureTime.charAt(3) - lectureTime.charAt(1);
			for(var i = 1; i < difference; i++) {
				var newTime = parseInt(firstTimeSlot.charAt(1)) + 1;
				firstTimeSlot = firstTimeSlot.charAt(0) + newTime;
				timeSlots.push(firstTimeSlot);
			}
		}
	});
	console.log(timeSlots);
	return timeSlots;
}

function getCourse(courseCode) {
	$.ajax({
				url: '../res/courses/timetable/' + courseCode + 'TimeTable.txt',
				dataType: 'json',
				async: false,
				success: function(data) {
					result = data;
				}
			});
	return result;
}

function getCourseTitle(course) {
	$.ajax({
				url: '../res/courses/timetable/' + course + 'H1TimeTable.txt',
				dataType: 'json',
				async: false,
				success: function(data) {
					result = data.title;
				}
			});
	return result;
}

function addSectionsToList(course) {
	//console.log(course);
	//console.log(course.innerHTML);
	//var courseName = course.innerHTML + "Sections";
	//console.log(course.id);
	//document.getElementById(courseName).appendChild("hello!");
}

function linkCourseToLI() {
	$(".course h3").each(function() {
		$(this).data(this.innerHTML, getCourse(this.innerHTML));
		addSectionsToList(this);
	});
}

function setupList() {
	for(i = 0; i < csvSplitNewline.length; i++) {
	var splitLine     = csvSplitNewline[i].split(',');
	var course        = splitLine[0];
	var isACourse     = course.indexOf("CSC")>-1;
	if(course.indexOf("/") > -1) {
		course = course.substring(0, course.indexOf("/"));
	}
	var notYetLogged  = contentString.indexOf(course) <= -1;
	if(isACourse && notYetLogged) {
		addCourseToList(course);
		contentString = contentString + course;
	}
}
}

$( document ).ready(function() {
    setupList();
	linkCourseToLI();
	$("#course-select").accordion({heightStyle: "content"});
});