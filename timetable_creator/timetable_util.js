var xmlhttp;
var csvText;
if (window.XMLHttpRequest) {
  xmlhttp=new XMLHttpRequest();
}
else {
  xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
}
xmlhttp.open("GET","../res/timetable2014.csv",false);
xmlhttp.send();
csvText=xmlhttp.responseXML;

//var csvNewline = csvText.split('\n');
console.log(csvText);
document.getElementById("content").innerHtml = csvText;