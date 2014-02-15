$(function() {
    $("ul.ianTabs").tabs("div.ianTab > div");
});

function create(htmlStr) {
    var frag = document.createDocumentFragment(),
        temp = document.createElement('div');
    temp.innerHTML = htmlStr;
    while (temp.firstChild) {
        frag.appendChild(temp.firstChild);
      }
        return frag;
    }
    var courseList = ["CSC108", "CSC148", "CSC207"];
    var htmlClickedString = "";
    for (var i = 0; i < courseList.length; i++) {
        htmlClickedString += "<div><p>" + courseList[i] + "</p></div>";
	var fragment = create(htmlClickedString); 
	
     }
     parent.document.getElementById("t1").appendChild(fragment);

    
