var day = /M|T|W|R|F/;
var time = /1|2|3|4|5|6|7|8|9/;
var timeSecondDig = /0|1|2/;
var hyphen = /-/;
var result;
var i;
var contentString = "";
var courseSelect = document.getElementById("course-select");
var backgroundTdColor = "#003366";

// In the future, it would be nice to pull the course values either from all json files, or the course nodes.

if (window.XMLHttpRequest) {
 xmlhttp=new XMLHttpRequest();
} else {
 xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
}

xmlhttp.open("GET", "../res/timetable2014.csv",false);
xmlhttp.send();

var httpResponse = xmlhttp.responseText;
var csvSplitNewline = httpResponse.split('\n');

/**
 * Adds the course to the course-select list navigation panel.
 *
 * @param course The course code of the course being added to the list. For example "CSC108H1"
 * TODO: Make sure limiting the course node name doesn't affect anything.
 */
function addCourseToList(course) {
	var courseNode = getCourse(course);
	courseNode.selected = "false";

	// Convert CSC***H1 -> CSC***
	courseNode.name = courseNode.name.substring(0,6);
	setupEntry(courseNode);
}

/**
 * Sets up the list item entry, adding the element to the course-select list.
 *
 * @param courseNode The course that is being added to the list.
 */
function setupEntry(courseNode) {
	if(typeof(courseNode.F) !== "undefined" || typeof(courseNode.Y) !== "undefined") {
		var entry = document.createElement('li');
		$(entry).css("border","2px solid black");
		var header = document.createElement('h3');
		var sections = document.createElement('div');
		header.appendChild(document.createTextNode(courseNode.name));
		entry.setAttribute("class", "course");
		sections.id = courseNode.name + "Sections";
		var sectionList;

		if(typeof(courseNode.F) !== "undefined") {
			sectionList = processSession(courseNode.F, courseNode, header);
						
		} else if(typeof(courseNode.Y) !== "undefined") {
			sectionList = processSession(courseNode.Y, courseNode, header);
		}

		sections.appendChild(sectionList);
		entry.appendChild(header);
		entry.appendChild(sections);
		courseSelect.appendChild(entry);
	}
}

/**
 * Processes courseNodes session. A session is the semester construct within the timetable.
 *
 * @param session     This courseNode's session. Either courseNode.Y, courseNode.F, courseNode.S.
 * @param courseNode  The courseNode that is currently being processed.
 * @param header      The corresponding header to the courseNode object.
 * @param sectionList Recently taken out.
 */
function processSession(session, courseNode, header) {
	var sectionList = document.createElement('ul');
	$.each(session.lectures, function(i, lecture) {
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
			var sectionTimes = getSectionTimeSlot(tutorial[1], "tutorial");
			processSectionTimes(section, sectionTimes, header, courseNode);
			sectionList.appendChild(section);
		}
	});

	return sectionList;
}

/**
 * Processes this sections times. 
 * TODO: Decide whether to shave this function from the project. There are bigger plans for this function,
 * other than to just call setSectionMouseEvents.
 * @param sections
 * @param sectionTimes
 * @param header
 * @param courseNode
 */
function processSectionTimes(section, sectionTimes, header, courseNode) {

	setSectionMouseEvents(section, sectionTimes, header, courseNode);
}


/**
 * Set's all section's mouse events: onclick, mouseover and mouseout. header is generally affected cosmetically.
 *
 * @param section
 * @param sectionTimes
 * @param header
 * @param courseNode
 */
function setSectionMouseEvents(section, sectionTimes, header, courseNode) {
	setSectionOnClick(section, sectionTimes, header, courseNode);
	setSectionMouseOver(section, sectionTimes, courseNode);
	setSectionMouseOut(section, sectionTimes);
}


/**
 * Sets section's onclick function. header is also modified in section's onclick method.
 *
 * TODO: use css class attrribute instead of dynamically changing style.
 * TODO: break up function to increase readability.
 * @param section      The lecture section that is course's mousover function is being applied to.
 * @param sectionTimes
 * @param header
 * @param courseNode
 */
function setSectionOnClick(section, sectionTimes, header, courseNode) {
	$(section).click(function() {
		var isLecture = section.innerHTML.charAt(0) === "L";
		var cancelSection;
		if(courseNode.selected === "true" && isLecture && courseNode.isLectureSelected === "true") {
			$(courseNode.selectedLecture).css("background-color", backgroundTdColor);
			$(courseNode.selectedLecture).css("color", "white");
			$(courseNode.selectedLectureHeader).css("background-color", backgroundTdColor);
			$(courseNode.selectedLectureHeader).css("color", "white");
			$.each(courseNode.selectedTimes, function(i, time) {
				document.getElementById(time).innerHTML = "";
				document.getElementById(time).setAttribute("clicked","false");
				$("#"+time).css("background-color", backgroundTdColor);
			});
			if(courseNode.selectedLecture.innerHTML !== section.innerHTML) {

				courseNode.selectedLecture = section;
				courseNode.selectedLectureHeader = header;
				courseNode.selectedTimes = sectionTimes;

				if(section.innerHTML.charAt(0) === "L") {
					$(section).css("background-color", "blue");
					$(header).css("background-color", "blue");
				} else {
					$(section).css("background-color", "orange");
					$(header).css("background-color", "orange");
				}

				$.each(sectionTimes, function(i, time) {
					
						document.getElementById(time).innerHTML = courseNode.name;
						document.getElementById(time).setAttribute("clicked","true");
						$("#"+time).css("background-color", "blue");
						$(section).css("color", "white");
						$(header).css("color", "white");
						$(section).attr("clicked", "true");	
				});
			} else {
				courseNode.selected = "false";
				courseNode.selectedLecture = null;
			}
		} else if (!isLecture) {
			$.each(sectionTimes, function(i, time) {
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
					$("#"+time).css("background-color", "blue");
					if(section.innerHTML.charAt(0) === "L") {

						$(section).css("background-color", "blue");
						$(header).css("background-color", "blue");
					} else {

						$(section).css("background-color", "orange");
						$(header).css("background-color", "orange");
					}
					$(section).css("color", "white");
					$(header).css("color", "white");
					$(section).attr("clicked", "true");
				}
			});
		} else if (courseNode.selected === "false") {
			courseNode.selected = "true";
			courseNode.selectedLecture = section;

			if(courseNode.selectedLecture.innerHTML.charAt(0) === "L") {
				courseNode.isLectureSelected = "true";
			} else {
				courseNode.isTutorialSelected = "true";
			}

			courseNode.selectedLectureHeader = header;
			courseNode.selectedTimes = sectionTimes;

			$.each(sectionTimes, function(i, time) {
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
					$("#"+time).css("background-color", "blue");
					if(section.innerHTML.charAt(0) === "L") {

						$(section).css("background-color", "blue");
						$(header).css("background-color", "blue");
					} else {

						$(section).css("background-color", "orange");
						$(header).css("background-color", "orange");
					}
					$(section).css("color", "white");
					$(header).css("color", "white");
					$(section).attr("clicked", "true");
				}
			});
		}
		
	});
}

/**
 * Sets section's mouseover function.
 *
 * @param section      The lecture section that is course's mousover function is being applied to.
 * @param sectionTimes
 * @param courseNode   The courseNode that owns the lecture section.
 */
function setSectionMouseOver(section, sectionTimes, courseNode) {
	$(section).mouseover(function(){
		$.each(sectionTimes, function(i, time) {
			if(document.getElementById(time).getAttribute("clicked") === "true") {
				$("#"+time).css("background-color", "#FF6666");
			} else {
				document.getElementById(time).innerHTML = courseNode.name;
			}
			if(courseNode.name === "CSC309H1") {
				alert("Bug alert! 309 has a mislabeled section in the CSC timesheet!");
			}
		});
	});
}

/**
 * Sets section's mouseout function. 
 *
 * @param section      The lecture section that is course's mousover function is being applied to.
 * @param sectionTimes
 */
function setSectionMouseOut(section, sectionTimes) {
	$(section).mouseout(function(){
		$.each(sectionTimes, function(i, time) {
			if(document.getElementById(time).getAttribute("clicked") !== "true") {
				document.getElementById(time).innerHTML = "";
				$("#"+time).css("background-color", backgroundTdColor);	
			} else {
				$("#"+time).css("background-color", "blue");
			}
		});
	});
}


/**
 * Returns an array of strings of separate lecture times for the specified section. 
 * Sections of type "lecture" are generally separated with commas, whereas tutorials are generally not.
 * TODO: Experiment with split method to shave down code.
 * @param   section
 * @param   type
 * @returns An array of this sections listed time sections.
 */
function getTimesArray(section, type) {
	var times = [];
	if(type === "lecture") {
		times = section.time.split(',');
	} else if (type === "tutorial" && typeof(section) !== "undefined") {
		times.push(section);
	}
	return times;
}


/**
 * Parses the individual course time string.
 * Example strings: "MWF10-12" | "M10" | "MT10-1".
 * TODO: Redo entire function. It works for now, but could be made simpler. As mentioned in the below TODO, it needs to be shaved.
 * @param An array of strings representing this course sections allocated times. ["F10","MT10-1"]
 * @return An array of strings representing each individual time. For instance, "M10-12" would be split into
 * ["M10","M11","M12"].
 * TODO: Break up function to increase readability.
 * @param times
 */
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
							firstTimeSlot = firstTimeSlot + lectureTime.charAt(1);
							break;
						case 2: //MW
							secondTimeSlot = lectureTime.charAt(1);
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
							firstTimeSlot = firstTimeSlot + lectureTime.charAt(2);
							secondTimeSlot = secondTimeSlot + lectureTime.charAt(2);
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

								firstTimeSlot = firstTimeSlot + lectureTime.charAt(3);
								secondTimeSlot = secondTimeSlot + lectureTime.charAt(3);

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
								firstTimeSlot = firstTimeSlot + lectureTime.charAt(3);
								secondTimeSlot = secondTimeSlot + lectureTime.charAt(3);
								thirdTimeSlot = thirdTimeSlot + lectureTime.charAt(3);
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
							firstTimeSlot = firstTimeSlot + lectureTime.charAt(4);
							secondTimeSlot = secondTimeSlot + lectureTime.charAt(4);
							thirdTimeSlot = thirdTimeSlot + lectureTime.charAt(4);
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

	return timeSlots;
}


/**
 * Gets an Array of section's time slots. For example, ["M10","M11","M12","F10"].
 *
 * @param section
 * @param type
 */
function getSectionTimeSlot(section, type) {
	var times = getTimesArray(section, type);
	var timeSlots = constructTimesArray(times);
	return timeSlots;
}

/**
 * Requests the JSON object stored within the res folder of the project.
 *
 * @param courseCode
 */
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

/**
 * Links each JSON course object to its list item counterpart.
 *
 */
function linkCourseToLI() {
	$(".course h3").each(function() {
		$(this).data(this.innerHTML, getCourse(this.innerHTML));
	});
}

/**
 * Sets up the course-select list (unordered list HTML element).
 * 
 */
function setupList() {

	// Iterates through the courses grabbed with the XMLHTTP request.
	// TODO: Rely on better way to grab all course node name in the future.
	for(i = 0; i < csvSplitNewline.length; i++) {
		var splitLine = csvSplitNewline[i].split(',');
		var course = splitLine[0];
		var isACourse = course.indexOf("CSC")>-1;

		// Filters out graduate/undergraduate hybrid courses and makes them purely undergraduate courses.
		if(course.indexOf("/") > -1) {
			course = course.substring(0, course.indexOf("/"));
		}

		// Many courses have duplicate listings due to the timetable holding both F and S sections.
		var notYetLogged = contentString.indexOf(course) <= -1;

		if(isACourse && notYetLogged) {
			addCourseToList(course);
			contentString = contentString + course;
		}
	}
}

/**
 * Called when the document is ready. This links all courses that have been grabbed from the XMLHTTP request
 * with their corresponding JSON object, and adds them to the course-select HTML component, including all
 * onclick, mouseover, mouseout methods. 
 */
$(document).ready(function() {
 setupList();
	linkCourseToLI();
	$("#course-select").accordion({heightStyle: "content", collapsible: true});
});