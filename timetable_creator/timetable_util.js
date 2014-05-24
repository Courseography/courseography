// Get csv file
if (window.XMLHttpRequest) {
  xmlhttp=new XMLHttpRequest();
}
else {
  xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
}
xmlhttp.open("GET","../res/timetable2014.txt",false);
xmlhttp.send();
xmlDoc=xmlhttp.responseXML;
var httpResponse = xmlhttp.responseText;

// Add output now for debugging purposes
document.getElementById("content").innerHTML = httpResponse; //Temp
var csvSplitNewline = httpResponse.split('\n');
var i;
contentString = "";
for(i=0;i<csvSplitNewline.length;i++) {
	var splitLine  = csvSplitNewline[i].split(',');
	var course     = splitLine[0];
	var semester   = splitLine[1];
	var title      = splitLine[2];
	var section    = splitLine[3];
	var time       = splitLine[4];
	var type       = splitLine[5];
	var instructor = splitLine[6];
	var capacity   = splitLine[7];
	if(course.indexOf("CSC")>-1 && contentString.indexOf(course) <= -1) {
		contentString = contentString + "<option value="+course+">" + course + "</option>";
	} else if(type === 'L') { //Lecture

	} else if(type === 'T') { //Tutorial

	}
}
console.log(contentString);

document.getElementById("course-select").innerHTML = contentString;