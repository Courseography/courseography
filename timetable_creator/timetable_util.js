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
} else {
  xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
}

xmlhttp.open("GET","../res/timetable2014.csv",false);
xmlhttp.send();
xmlDoc=xmlhttp.responseXML;

var httpResponse = xmlhttp.responseText;
var csvSplitNewline = httpResponse.split('\n');
var courseSelect = document.getElementById("course-select");
var backgroundTdColor = "#003366";

function addCourseToList(course) {
	var courseNode = getCourse(course);
	setupEntry(courseNode);
}

function setupEntry(courseNode) {
	if(typeof(courseNode.F) !== "undefined" || typeof(courseNode.Y) !== "undefined") {
		var entry    = document.createElement('li');
		$(entry).css("border","2px solid black");
		var header   = document.createElement('h3');
		var sections = document.createElement('div');
		header.appendChild(document.createTextNode(courseNode.name));
		entry.setAttribute("class", "course");
		sections.id = courseNode.name + "Sections";
		var sectionList = document.createElement('ul');

		if(typeof(courseNode.F) !== "undefined") {
			sectionList = processSession(courseNode.F, courseNode, header, sectionList);
						
		} else if(typeof(courseNode.Y) !== "undefined") {
			sectionList = processSession(courseNode.Y, courseNode, header, sectionList);
		}

		sections.appendChild(sectionList);
		entry.appendChild(header);
		entry.appendChild(sections);
		courseSelect.appendChild(entry);
	}
}

function processSession(session, courseNode, header, sectionList) {
	$.each(session.lectures, function(i, lecture){
		if(lecture.section.charAt(1) !== "2") {
			var section = document.createElement("li");
			section.setAttribute("class", "section");
			section.appendChild(document.createTextNode(lecture.section));
			var sectionTimes = getSectionTimeSlot(lecture, "lecture");
			if(courseNode.manualTutorialEnrolment === false) {
				sectionTimes = sectionTimes.concat(getSectionTimeSlot(session.tutorials[i], "tutorial"));
			}
			processSectionTimes(section, sectionTimes, header, courseNode);
			sectionList.appendChild(section);
		}
	});

	$.each(session.tutorials, function(i, tutorial){
		if(courseNode.manualTutorialEnrolment) {
			var section = document.createElement("li");
			section.setAttribute("class", "section");
			section.appendChild(document.createTextNode(tutorial[0]));
			console.log(courseNode + " " + tutorial[0] + " " + tutorial[1]);
			var sectionTimes = getSectionTimeSlot(tutorial[1], "tutorial");
			processSectionTimes(section, sectionTimes, header, courseNode);
			sectionList.appendChild(section);
		}
	});

	return sectionList;
}

function processSectionTimes(section, sectionTimes, header, courseNode) {
	$.each(sectionTimes, function(i, time) {
		setSectionMouseEvents(section, time, header, courseNode);
	});
}

function setSectionMouseEvents(section, time, header, courseNode) {
	setSectionOnClick(section, time, header, courseNode);
	setSectionMouseOver(section, time, courseNode);
	setSectionMouseOut(section, time);
}

function setSectionOnClick(section, time, header, courseNode) {
	$(section).click(function(){
		if(document.getElementById(time).getAttribute("clicked") === "true" && document.getElementById(time).innerHTML === courseNode.name) {
			document.getElementById(time).innerHTML = "";
			document.getElementById(time).setAttribute("clicked","false");
			$("#"+time).css("background-color", backgroundTdColor);
			$(section).css("background-color", backgroundTdColor);
			$(section).css("color", "white");
			$(header).css("background-color", backgroundTdColor);
			$(header).css("color", "white");
		} else if(document.getElementById(time).getAttribute("clicked") !== "true") {
			document.getElementById(time).innerHTML = courseNode.name;
			document.getElementById(time).setAttribute("clicked","true");
			console.log(courseNode.name);
			$("#"+time).css("background-color", "blue");
			$(section).css("background-color", "blue");
			$(header).css("background-color", "blue");
			$(section).css("color", "white");
			$(header).css("color", "white");
		}
	});
}

function setSectionMouseOver(section, time, courseNode) {
	$(section).mouseover(function(){
		if(document.getElementById(time).getAttribute("clicked") === "true") {
			$("#"+time).css("background-color", "#FF6666");
		} else {
			document.getElementById(time).innerHTML = courseNode.name;
		}
	});
}

function setSectionMouseOut(section, time) {
	$(section).mouseout(function(){
		if(document.getElementById(time).getAttribute("clicked") !== "true") {
			document.getElementById(time).innerHTML = "";
			$("#"+time).css("background-color", backgroundTdColor);	
		} else {
			$("#"+time).css("background-color", "blue");

		}
	});
}

function getTimesArray(section, type) {
	var times = [];
	if(type === "lecture") {
		times = section.time.split(',');
	} else if (type === "tutorial" && typeof(section) !== "undefined") {
		times.push(section);
	}
	return times;
}


function constructTimesArray(times) {
	var firstTimeSlot;
	var secondTimeSlot;
	var thirdTimeSlot;
	var timeSlots = [];
	var numDays;
	$.each(times, function(i, lectureTime) {
		var timeLength = lectureTime.length;
		var difference;
		numDays = 0;
		for(var k = 0; k < timeLength; k++) {

			if(lectureTime.charAt(k).match(day) !== null) {
				numDays++;
			}

			switch (k) {
				case 0: // M,T,W,R,F - Always a day
					firstTimeSlot = lectureTime.charAt(0);
					break;
				case 1: //M1, MW
					switch(numDays) {
						case 1: // M1
							console.log("M1 ish");
							firstTimeSlot = firstTimeSlot + lectureTime.charAt(1);
							break;
						case 2: //MW
							secondTimeSlot = lectureTime.charAt(1);
							console.log("second time slot: " + secondTimeSlot);
							break;
					}
					break;
				case 2: //MWF. MW1. M1-
					switch(numDays) {
						case 1: // M10, M1- !!!!!
							if(!lectureTime.charAt(2).match(hyphen)) {
								firstTimeSlot = firstTimeSlot + lectureTime.charAt(2);		
							}
							break;
						case 2: //MW1
							firstTimeSlot  = firstTimeSlot   + lectureTime.charAt(2);
							secondTimeSlot = secondTimeSlot  + lectureTime.charAt(2);
							break;
						case 3: // MWF
							thirdTimeSlot = lectureTime.charAt(2);
							break;
					}
					break;
				case 3: //MWF1
					switch(numDays) {
						case 1: //M10-, M1-2 W6-9

							var newTime;
							if(timeLength > 5) {
								difference = lectureTime.substring(4,6) - lectureTime.substring(1,3);
								newTime = parseInt(firstTimeSlot.substring(1,3));
							} else if (timeLength === 5) {
								if(lectureTime.charAt(2).match(hyphen)) { //M9-11
									difference = lectureTime.substring(3,5) - lectureTime.charAt(1);
									newTime = parseInt(firstTimeSlot.charAt(1));
								} else { //M11-1
									difference = lectureTime.charAt(4) - lectureTime.substring(1,3) + 12;
									newTime = parseInt(firstTimeSlot.substring(1,3));
								}
							} else { // M1-2
								difference = lectureTime.charAt(3) - (lectureTime.charAt(1));
								newTime = parseInt(firstTimeSlot.charAt(1));
							}

							for(var l = 0; l < difference; l++) {
								if(newTime > 12) {
									newTime = newTime - 12;
								}
								firstTimeSlot = firstTimeSlot.charAt(0) + newTime;
								timeSlots.push(firstTimeSlot);
								newTime = newTime + 1;
							}

							return timeSlots;

							break;
						case 2: //MW1-, MW10-12, WF11
							if(lectureTime.search("-") <= -1) {

								firstTimeSlot  = firstTimeSlot   + lectureTime.charAt(3);
								secondTimeSlot = secondTimeSlot  + lectureTime.charAt(3);

							} else {
								var newTime;
								if(timeLength > 5) {
									difference = lectureTime.substring(5,7) - lectureTime.substring(2,4);
									newTime = parseInt(firstTimeSlot.substring(2,4));
								} else if (timeLength === 6) {
									difference = lectureTime.substring(1,3) - lectureTime.charAt(4);
									newTime = parseInt(firstTimeSlot.substring(1,3));
								} else {
									difference = lectureTime.charAt(1) - (lectureTime.substring(1,3) - 12);
									newTime = parseInt(firstTimeSlot.charAt(1));
								}

								for(var o = 0; o < difference; o++) {
									if(newTime > 12) {
										newTime = newTime - 12;
									}
									firstTimeSlot = firstTimeSlot.charAt(0) + newTime;
									//timeSlots.push(firstTimeSlot);
									newTime = newTime + 1;
								}
							}
							break;
						case 3: // MWF1
								firstTimeSlot  = firstTimeSlot  + lectureTime.charAt(3);
								secondTimeSlot = secondTimeSlot + lectureTime.charAt(3);
								thirdTimeSlot  = thirdTimeSlot  + lectureTime.charAt(3);
							break;
						case 4: // MWTF, not yet covered by CS, but german has sections like this.

							break;
					}
					break;
				case 4:
					switch(numDays) {
						case 1: // M10-1, case previously covered

							break;
						case 2: // MW10-1, MW1-10, cases previously covered

							break;
						case 3: // MWF12, MWF1-2 <-- need to cover this case, for future.
							firstTimeSlot  = firstTimeSlot  + lectureTime.charAt(4);
							secondTimeSlot = secondTimeSlot + lectureTime.charAt(4);
							thirdTimeSlot  = thirdTimeSlot  + lectureTime.charAt(4);
							break;
						case 4: // MTWF1, todo

							break;
					}
					break;
				case 5:
					switch(numDays) {
						case 1: // No strings reach this state.

							break;
						case 2: // MT10-12 <-- already covered

							break;
						case 3: //

							break;
						case 3: //

							break;
					}
					break;
				case 6: //MW11-12
					switch(numDays) {
						case 1: // No strings reach this state.

							break;
						case 2: //

							break;
						case 3: // 

							break;
						case 3: // 

							break;
					}
					break;
			}
		}

		if(typeof(firstTimeSlot) !== "undefined") {
			timeSlots.push(firstTimeSlot);

		} 
		if (typeof(secondTimeSlot) !== "undefined") {
			timeSlots.push(secondTimeSlot);

		}
		if (typeof(thirdTimeSlot) !== "undefined") {
			timeSlots.push(thirdTimeSlot);

		}
	});
	
	console.log(timeSlots);

	return timeSlots;

}

function getSectionTimeSlot(section, type) {
	var times = getTimesArray(section, type);
	var timeSlots = constructTimesArray(times);
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

function linkCourseToLI() {
	$(".course h3").each(function() {
		$(this).data(this.innerHTML, getCourse(this.innerHTML));
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

$(document).ready(function() {
    setupList();
	linkCourseToLI();
	$("#course-select").accordion({heightStyle: "content", collapsible: true});
});