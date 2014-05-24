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
	if(csvSplitNewline[i].indexOf("CSC")>-1) {
		var course = csvSplitNewline[i].substring(0,csvSplitNewline[i].indexOf("CSC")+6);
		contentString = contentString + "<option value="+course+">" + course + "</option>";
	}
}
console.log(contentString);

document.getElementById("course-select").innerHTML = contentString;