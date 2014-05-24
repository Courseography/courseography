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
document.getElementById("content").innerHtml = httpResponse; //Temp
var csvSplitNewline = httpResponse.split('\n');
var i;
contentString = "";
for(i=0;i<csvSplitNewline.length;i++) {
	if(csvSplitNewline[i].indexOf("CSC")>-1) {
		contentString = contentString + "<option>" + csvSplitNewline.substring(0,csvSplitNewline.indexOf("CSC")+6) + "</option>";
	}
}
console.log(contentString);

document.getElementById("course-select").innerHtml = contentString;