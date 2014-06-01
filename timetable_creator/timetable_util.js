// Get csv file
if (window.XMLHttpRequest) {
  xmlhttp=new XMLHttpRequest();
}
else {
  xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
}
xmlhttp.open("GET","../res/timetable2014.csv",false);
xmlhttp.send();
xmlDoc=xmlhttp.responseXML;
var result;
var httpResponse = xmlhttp.responseText;
console.log(result);
document.getElementById("content").innerHTML = result; //Temp
var csvSplitNewline = httpResponse.split('\n');
var i;
var courseSelect = document.getElementById("course-select");
contentString = "";

function addCourseToList(course) {
		var entry    = document.createElement('li');
		var header   = document.createElement('h3');
		var sections = document.createElement('div');
		var courseNode = getCourse(course);
		header.appendChild(document.createTextNode(course));
		header.setAttribute("class", "course");
		sections.id = course + "Sections";
		var sectionList = document.createElement('ul');
		var fall = document.createElement("li");
		if(typeof(courseNode.F) !== "undefined") {
		$.each(courseNode.F.lectures, function(i, lecture){
			var section = document.createElement("li");
			section.appendChild(document.createTextNode(lecture.section));
			sectionList.appendChild(section);
			console.log(lecture.section);
		});

		}
		sections.appendChild(sectionList);
		entry.appendChild(header);
		entry.appendChild(sections);
		courseSelect.appendChild(entry);
		return entry;
}

function addCourse() {
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
	$(".course").each(function() {
		$(this).data(this.innerHTML, getCourse(this.innerHTML));
		$(this).mouseover(function(){document.getElementById("M9").innerHTML = $(this).data(this.innerHTML).F.lectures});
		$(this).mouseout(function(){document.getElementById("M9").innerHTML = "mouse out!"});
		getCourseTime(this);
		addSectionsToList(this);
	});
}
var time;
var day = /M|T|W|R|F/;
function getCourseTime(course) {
	if(typeof($(course).data(course.innerHTML).F) === "undefined"){

	} else {
		time = $(course).data(course.innerHTML).F.lectures[0].time;
		// console.log(time.length);
		// console.log(time.charAt(0).match(day));
		if(time.charAt(0).match(day) === null) {
			console.log("WE FOUND A NULL");
		} else {

		}
		console.log($(course).data(course.innerHTML).F.lectures[0].time);
	}
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
		console.log(getCourse(course).name);
		contentString = contentString + course;
	}
}
}

$( document ).ready(function() {
    setupList();
	linkCourseToLI();
	$("#course-select").accordion({autoHeight:false});
});