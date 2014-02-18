var nodes = [];
var edges = [];
var clickedCourses = [];
var clickedNode = parent.document.getElementById("courseGrid");

var fragment2 = createTimeTable();
timeNode = parent.document.getElementById("timetable");
timeNode.appendChild(fragment2);

var FCEs = 0;
var FCEs100 = 0;
var FCEs200 = 0;
var FCEs300 = 0;
var FCEs400 = 0;
var FCEsMAT = 0;
var clickedCourses = [];

var sciFocusList = ["CSC336", "CSC446", "CSC456", "CSC320", "CSC418", "CSC321", "CSC411", "CSC343", "CSC384", "CSC358", "CSC458"];
var AIFocusList = ["CSC310", "CSC330", "CSC438", "CSC448", "CSC463", "CSC401", "CSC485", "CSC320", "CSC420", "CSC321", "CSC411", "CSC412", "CSC384", "CSC486"];
var NLPFocusList = ["CSC318", "CSC401", "CSC485", "CSC309", "CSC321", "CSC330", "CSC411", "CSC428", "CSC486"];
var visionFocusList = ["CSC320", "CSC336", "CSC411", "CSC420", "CSC418", "CSC412"];
var systemsFocusList = ["CSC324", "CSC343", "CSC443", "CSC469", "CSC488", "CSC372", "ECE385", "CSC358", "CSC458", "CSC301", "CSC309", "CSC410", "ECE489"];
var gameFocusList = ["CSC300", "CSC301", "CSC318", "CSC324", "CSC384", "CSC418", "CSC404"];
var HCIFocusList = ["CSC300", "CSC301", "CSC318", "CSC428", "CSC309", "CSC320", "CSC321", "CSC343", "CSC384", "CSC401", "CSC404", "CSC418", "CSC485", "CSC490", "CSC491"];
var theoryFocusList = ["CSC336", "CSC463", "CSC310", "CSC438", "CSC448", "Sta2"];
var webFocusList = ["Sta2", "CSC309", "CSC343", "CSC358", "CSC458", "CSC411", "CSC310", "CSC443", "CSC469"];

var activeFocus = "";

function showtime(id) {
    var focus = window[id + "FocusList"];
    $("ellipse.spotlight").remove();
    if (activeFocus == id) {
        // Deactivate the focuses.
        $("#graph").css("background", "white");
        activeFocus = "";
        for (var i = 0; i < nodes.length; i++) {
            window[nodes[i]].updateSVG();
        }
    } else {
        $("#graph").css("background", "rgb(40,40,40)");
        $(".node, .hybrid").attr('data-active', 'unlit');
        for (var i = 0; i < focus.length; i++) {
            spotlight(focus[i]);
            window[focus[i]].updateSVG();
        }
        $("#graph").html($("#graph").html());
        $(".node").click(function() {
            turnNode();
        });
        $(".node").hover(function() {
            hoverFocus();
        }, function() {
            hoverUnfocus();
        });
        activeFocus = id;
    }
}

function spotlight(id) {
    var x = parseFloat($("#".concat(id, " > rect")).attr("x")) + 20;
    var y = parseFloat($("#".concat(id, " > rect")).attr("y")) + 15;
    var el = '<ellipse class="spotlight" cx="'.concat(x, '" cy = "', y, '" rx="30" ry = "27" style="fill:white; opacity:0.8; stroke: none" />');
    $("#" + id).before(el);
    $("#" + id).attr('data-active', 'lit');
}

function reset() {
    
    for (var i = 0; i < nodes.length; i++) {
        window[nodes[i]].turnOff();
    }
}

function makeNode(parents, type, name) {
    window[name] = new Node(parents, type, name);
    nodes.push(name);
}

function makeHybrid(parents, type, name) {
    makeNode(parents, type, name);
    window[name].hybrid = true;
}

function makeEdge(parent, child, name) {
    window[name] = new Edge(parent, child, name);
    parent.outEdges.push(window[name]);
    child.inEdges.push(window[name]);
}

function turnNode() {
    window[$(event.target).parent().attr("id")].turn();
}

function Edge(parent, child, name) {
    this.parent = parent;
    this.child = child;
    this.name = name;
}

function Node(parents, type, name) {
    this.name = name;
    this.parents = parents;
    this.logicalType = type;
    this.children = [];
    this.active = false;
    this.takeable = false;
    this.updated = false;
    this.hybrid = false;
    this.outEdges = [];
    this.inEdges = [];

    for (var i = 0; i < this.parents.length; i++) {
        this.parents[i].children.push(this);
    }

    this.turnOff = function() {
        this.active = false;
        if (this.name == "CSC454" || this.name == "CSC494" || this.name == "CSC495") {
            this.takeable = false;
        } else {
            this.takeable = this.parents.length == 0;
        }

        // Edges
        for (var i = 0; i < this.outEdges.length; i++) {
            var id = this.outEdges[i].name;
            console.log("edge " + id);
            $("#" + id).attr("data-active", "inactive")
        }

        this.updateSVG();
        this.updateClickedCourses();
    }


    this.turn = function() {
        if (this.active || this.takeable) {
            for (var i = 0; i < nodes.length; i++) {
                window[nodes[i]].updated = false;
            }

            this.updated = true;
            this.active = !this.active;

            this.updateClickedCourses();
            updatePOSt(this.name, this.active);

            if (this.name == "CSC454") {
                this.takeable = FCEs300 + FCEs400 >= 2.5;
            } else if (this.name == "CSC494" || this.name == "CSC495") {
                this.takeable = FCEs300 + FCEs400 >= 1.5;
            } else {
                this.takeable = true;
            }

            for (var i = 0; i < this.parents.length; i++) {
                this.takeable = this.takeable && this.parents[i].active;
            }
            this.updateSVG();
            for (var i = 0; i < this.children.length; i++) {
                this.children[i].isActive();
            }

            // Edges
            for (var i = 0; i < this.outEdges.length; i++) {
                var id = this.outEdges[i].name;
                console.log("edge " + id);
                if (!this.active) {
                    $("#" + id).attr("data-active", "inactive");
                } else if (this.outEdges[i].child.active) {
                    $("#" + id).attr("data-active", "active");
                } else {
                    $("#" + id).attr("data-active", "takeable");
                }
            }

            for (var i = 0; i < this.inEdges.length; i++) {
                var id = this.inEdges[i].name;
                $("#" + id).attr("data-active", "active");
                
            }

            // Check CSC454
            CSC454.isActive();
            CSC494.isActive();
            CSC495.isActive();
        }

    }

    this.isActive = function() {
        if (this.updated) {
            return this.active;
        } else {
            this.updated = true;
            console.log("\t\t\t", "Running 'isActive' on: ", this.name);

            if (this.name == "CSC454") {
                this.takeable = FCEs300 + FCEs400 >= 2.5;
            } else if (this.name == "CSC494" || this.name == "CSC495") {
                this.takeable = FCEs300 + FCEs400 >= 1.5;
            } else if (this.logicalType == "AND") {
                this.takeable = true;
            } else {
                this.takeable = false;
            }

            if (this.logicalType == "AND") {
                for (var i = 0; i < this.parents.length; i++) {
                    this.takeable = this.takeable && this.parents[i].isActive();
                }
            } else if (this.logicalType == "OR") {
                for (var i = 0; i < this.parents.length; i++) {
                    this.takeable = this.takeable || this.parents[i].isActive();
                }
            }

            if (this.active && !this.takeable) {
                this.active = false;

                this.updateClickedCourses();

                if (this.name.charAt(3) >= '3' && this.name.length == 6 && !this.hybrid) {
                    num300Plus--;
                }
            } else if (this.hybrid && this.takeable) {
                this.active = true;
                this.updateClickedCourses();
            }

            for (var i = 0; i < this.children.length; i++) {
                this.children[i].isActive();
            }

            this.updateSVG();

            // Edges
            for (var i = 0; i < this.outEdges.length; i++) {
                var id = this.outEdges[i].name;
                if (!this.active) {
                    $("#" + id).attr("data-active", "inactive");
                } else if (this.outEdges[i].child.active) {
                    $("#" + id).attr("data-active", "active");
                } else {
                    $("#" + id).attr("data-active", "takeable");
                }
            }

            for (var i = 0; i < this.inEdges.length; i++) {
                var id = this.inEdges[i].name;
                if (this.active) {
                    $("#" + id).attr("data-active", "active");
                } else if (this.inEdges[i].child.active) {
                    $("#" + id).attr("data-active", "takeable");
                } else {
                    $("#" + id).attr("data-active", "inactive");
                }
            }

            updatePOSt(this.name, this.active);

            return this.active;

        }

    }

    this.updateSVG = function() {
        if (this.active) {
            $("#" + this.name).attr('data-active', 'active');
        } else if (this.takeable) {
            $("#" + this.name).attr('data-active', 'takeable');
        } else {
            $("#" + this.name).attr('data-active', 'inactive');
        }
    }


    this.focus = function() {
        if (!this.active) {
            $("#" + this.name).attr('data-active', 'missing');
            for (var i = 0; i < this.parents.length; i++) {
                this.parents[i].focus();
            }
        }
    }

    this.unfocus = function() {
        if (!this.active) {
            this.updateSVG();
            console.log(this.takeable);
            for (var i = 0; i < this.parents.length; i++) {
                this.parents[i].unfocus();
            }
        }
    }

    this.updateClickedCourses = function() {
        while (clickedNode.firstChild) {
            parent.document.getElementById("courseGrid").removeChild(clickedNode.firstChild);
        }
        if (this.active && !this.hybrid) {
            if (this.name == "CSC200") {
                FCEs200 += 1
            } else if (this.name.substr(0, 4) == "CSC1") {
                FCEs100 += 0.5;
            } else if (this.name.substr(0, 4) == "CSC2") {
                FCEs200 += 0.5;
            } else if (this.name.substr(0, 4) == "CSC3" 
                || this.name.substr(0,4) == "ECE3") {
                FCEs300 += 0.5;
            } else if (this.name.substr(0, 4) == "CSC4"
                || this.name.substr(0,4) == "ECE4") {
                FCEs400 += 0.5;
            } else if (this.name == "Calc1") {
                FCEsMAT += 1;
            } else if (this.name == "Lin1" || this.name == "Sta1" || this.name == "Sta2") {
                FCEsMAT += 0.5;
            } else {
                console.log("Unexpected course: " + this.name)
            }
            clickedCourses.push(this.name);
            console.log(clickedCourses);
        } else {
            var index = clickedCourses.indexOf(this.name);
            if (index > -1) {
                clickedCourses.splice(index, 1);
                if (this.name == "CSC200") {
                    FCEs200 -= 1
                } else if (this.name.substr(0, 4) == "CSC1") {
                    FCEs100 -= 0.5;
                } else if (this.name.substr(0, 4) == "CSC2") {
                    FCEs200 -= 0.5;
                } else if (this.name.substr(0, 4) == "CSC3" 
                    || this.name.substr(0,4) == "ECE3") {
                    FCEs300 -= 0.5;
                } else if (this.name.substr(0, 4) == "CSC4"
                    || this.name.substr(0,4) == "ECE4") {
                    FCEs400 -= 0.5;
                } else if (this.name == "Calc1") {
                    FCEsMAT -= 1;
                } else if (this.name == "Lin1" || this.name == "Sta1" || this.name == "Sta2") {
                    FCEsMAT -= 0.5;
                } else {
                    console.log("Unexpected course: " + this.name)
                }
            }
        }
        FCEs = FCEs100 + FCEs200 + FCEs300 + FCEs400 + FCEsMAT;

        $('#FCEcount').html(FCEs);


        var htmlClickedString = "";
        for (var i = 0; i < clickedCourses.length; i++) {
            htmlClickedString += "<td class='courseCell' style='background: " + $("#" + clickedCourses[i] + "> rect").css('fill') + "'><div id='" + clickedCourses[i] + "cell'><p class='courseName'>" + clickedCourses[i] + "</p><p>Course information about this.. course!</p></div></td>";
        }

        $('#courseGrid').html(htmlClickedString);

    }

}

// Auxillary courses for CS requirements
makeNode([], "AND", "Calc1");
makeNode([Calc1], "AND", "Sta1");
makeNode([Sta1], "AND", "Sta2");
makeNode([], "AND", "Lin1");

// First year courses

makeNode([], "AND", "CSC104");
makeNode([], "AND", "CSC108");
makeNode([], "AND", "CSC120");
makeNode([CSC108], "AND", "CSC148");
makeNode([], "AND", "CSC165");
makeHybrid([CSC165, Calc1], "OR", "hybrid16");
makeHybrid([Calc1, Lin1], "OR", "bool5");

// Second year courses

makeNode([], "AND", "CSC200");
makeNode([CSC148], "AND", "CSC207");
makeNode([CSC207], "AND", "CSC209");
makeNode([CSC148, CSC165], "AND", "CSC236");
makeNode([CSC236], "AND", "hybrid11");
makeNode([CSC148, CSC165], "AND", "CSC258");
makeHybrid([CSC209, CSC258], "AND", "bool2");
makeHybrid([Sta1], "AND", "hybrid6");
makeHybrid([CSC236, CSC207], "AND", "bool1");
makeNode([bool1, hybrid6], "AND", "CSC263");
makeHybrid([CSC263], "AND", "hybrid2");
makeHybrid([CSC263], "AND", "hybrid10");

// Third year courses

makeNode([], "AND", "CSC300");
makeNode([CSC263], "AND", "hybrid15");
makeNode([CSC209, hybrid10], "AND", "CSC301");
makeNode([CSC301], "AND", "CSC302");
makeHybrid([Lin1], "AND", "hybrid7");
makeHybrid([hybrid6, hybrid7], "AND", "bool4");
makeHybrid([CSC148], "AND", "hybrid13")
makeNode([hybrid13, bool4], "AND", "CSC310");
makeNode([], "AND", "CSC318");
makeHybrid([Lin1, Calc1], "AND", "hybrid9");
makeHybrid([CSC209], "AND", "hybrid12");
makeNode([hybrid12, bool5], "AND", "CSC320");
makeNode([bool4], "AND", "CSC321");
makeNode([bool1], "AND", "CSC324");
makeHybrid([CSC324], "AND", "hybrid4");
makeNode([CSC236], "AND", "CSC330");
makeHybrid([Lin1, Calc1], "AND", "hybrid11");
makeNode([CSC148, bool5], "AND", "CSC336");
makeHybrid([CSC207], "AND", "hybrid14");
makeNode([hybrid16, hybrid14], "AND", "CSC343");
makeNode([CSC209, CSC343], "AND", "CSC309");
makeHybrid([CSC209, CSC263], "AND", "hybrid2");
makeNode([bool2, hybrid2], "AND", "CSC358");
makeHybrid([CSC258, CSC209], "AND", "bool3");
makeNode([bool2], "AND", "CSC369");
makeNode([bool2], "AND", "CSC372");
makeNode([CSC263], "AND", "CSC373");
makeHybrid([Sta1], "AND", "hybrid8");
makeNode([CSC324, hybrid8], "AND", "CSC384");
makeNode([bool2], "AND", "ECE385");

// Fourth year courses

makeNode([CSC207, hybrid8], "AND", "CSC401");
makeNode([bool1], "AND", "CSC410");
makeHybrid([Sta2], "AND", "hybrid9");
makeNode([CSC263, Calc1, hybrid9], "AND", "CSC411");
makeNode([CSC411], "AND", "CSC412");
makeNode([CSC236], "AND", "CSC463");
makeHybrid([CSC373, CSC463], "OR", "bool3");
makeNode([CSC336, CSC373, CSC463], "OR", "CSC336orCSC373orCSC463");
makeHybrid([CSC336orCSC373orCSC463, Calc1], "AND", "hybrid3");
makeNode([hybrid3, hybrid12], "AND", "CSC418");
makeHybrid([CSC263], "AND", "hybrid15");
makeNode([hybrid15, bool5], "AND", "CSC420");
makeNode([CSC318, Sta2, hybrid12], "AND", "CSC428"); // Exception "Sta2 may be replaced by PSY201..."
makeNode([CSC318, CSC418, CSC301, CSC384], "OR", "hybrid1");
makeHybrid([hybrid1], "AND", "CSC404");
makeNode([CSC336], "AND", "CSC436");
makeNode([bool3], "OR", "CSC438");
makeHybrid([CSC373], "AND", "hybrid5");
makeNode([CSC343, CSC369, hybrid5], "AND", "CSC443");
makeNode([CSC336], "AND", "CSC446");
makeNode([CSC463], "AND", "CSC448");
makeNode([], "AND", "CSC454");
makeNode([CSC209, CSC336], "AND", "CSC456");
makeNode([bool2, hybrid2], "AND", "CSC458");
makeNode([CSC236], "AND", "CSC465");
makeNode([CSC369], "AND", "CSC469");
makeNode([hybrid8, CSC209], "AND", "CSC485");
makeNode([CSC384, bool3], "AND", "CSC486");
makeNode([CSC258, hybrid4, hybrid2], "AND", "CSC488");
makeNode([hybrid11], "AND", "ECE489");

makeNode([], "AND", "CSC490");
makeNode([], "AND", "CSC491");
makeNode([], "AND", "CSC494");
makeNode([], "AND", "CSC495");

makeEdge(CSC463, CSC448, "p1");
makeEdge(CSC165, CSC236, "p2");
makeEdge(CSC236, CSC463, "p3");
makeEdge(CSC263, CSC373, "p4");
makeEdge(CSC108, CSC148, "p5");
makeEdge(CSC148, CSC236, "p6");
makeEdge(CSC148, CSC207, "p7");
makeEdge(CSC207, CSC209, "p8");
makeEdge(CSC148, CSC258, "p9");
makeEdge(CSC165, CSC258, "p10");
makeEdge(CSC318, CSC428, "p80");
makeEdge(CSC148, CSC336, "p11");
makeEdge(CSC336, CSC446, "p12");
makeEdge(CSC336, CSC456, "p13");
makeEdge(CSC336, CSC436, "p14");
makeEdge(CSC384, CSC486, "p15");
makeEdge(CSC463, CSC448, "p16");
makeEdge(CSC209, CSC485, "p17");
makeEdge(CSC263, CSC411, "p18");
makeEdge(CSC209, CSC301, "p19");
makeEdge(CSC301, CSC302, "p20");
makeEdge(CSC324, CSC384, "p21");
makeEdge(CSC369, CSC469, "p22");
makeEdge(CSC209, CSC309, "p23");
makeEdge(CSC343, CSC309, "p24");
makeEdge(CSC343, CSC443, "p25");
makeEdge(CSC369, CSC443, "p26");
makeEdge(hybrid1, CSC404, "p27");
makeEdge(hybrid2, CSC448, "p28");
makeEdge(hybrid2, CSC358, "p29");
makeEdge(hybrid2, CSC458, "p30");
makeEdge(hybrid3, CSC418, "p31");
makeEdge(CSC236, bool1, "p32");
makeEdge(CSC207, bool1, "p33");
makeEdge(bool1, CSC263, "p34");
makeEdge(bool1, CSC410, "p35");
makeEdge(bool1, CSC324, "p36");
makeEdge(hybrid4, CSC488, "p37");
makeEdge(hybrid5, CSC443, "p38");
makeEdge(CSC209, bool2, "p39");
makeEdge(CSC258, bool2, "p40");
makeEdge(bool2, CSC369, "p41");
makeEdge(bool2, ECE385, "p42");
makeEdge(bool2, CSC358, "p43");
makeEdge(bool2, CSC458, "p44");
makeEdge(CSC463, bool3, "p45");
makeEdge(CSC373, bool3, "p46");
makeEdge(bool3, CSC438, "p47");
makeEdge(bool3, CSC486, "p48");
makeEdge(hybrid6, CSC263, "p49");
makeEdge(hybrid7, CSC384, "p50");
makeEdge(hybrid7, CSC401, "p51");
makeEdge(hybrid7, CSC485, "p52");
makeEdge(hybrid8, CSC411, "p54");
makeEdge(Calc1, Sta1, "p55");
makeEdge(Sta2, CSC428, "p56");
makeEdge(Sta1, Sta2, "p57");
makeEdge(CSC411, CSC412, "p58");
makeEdge(hybrid9, CSC301, "p59");
makeEdge(hybrid10, ECE489, "p60");
makeEdge(bool2, CSC372, "p61");
makeEdge(hybrid11, CSC456, "p62");
makeEdge(hybrid11, CSC428, "p63");
makeEdge(hybrid11, CSC320, "p64");
makeEdge(hybrid11, CSC418, "p65");
makeEdge(hybrid12, bool4, "p66");
makeEdge(hybrid13, bool4, "p67");
makeEdge(bool4, CSC321, "p68");
makeEdge(bool4, CSC310, "p69");
makeEdge(hybrid14, CSC310, "p70");
makeEdge(hybrid15, CSC343, "p71");
makeEdge(hybrid16, CSC420, "p72");
makeEdge(hybrid16, CSC343, "p73");
makeEdge(Lin1, bool5, "p74");
makeEdge(Calc1, bool5, "p75");
makeEdge(bool5, CSC336, "p76");
makeEdge(bool5, CSC320, "p77");
makeEdge(bool5, CSC420, "p78");
makeEdge(CSC258, CSC488, "p79");


function hoverFocus() {
    var id = $(event.target).parent().attr("id");
    window[id].focus();

    window.mytimeout = setTimeout(function() {
        // Get data from course calendar
        var xmlreq = new XMLHttpRequest();
        xmlreq.open("GET", "res/calendar.txt", false);
        xmlreq.send();

        var patt1 = new RegExp("\n" + id + ".*\n.*\n", "im");
        var courseString = xmlreq.responseText.match(patt1)[0].split("\n");
        console.log(courseString);
        var htmlCourseString = "";
        for (var i = 0; i < courseString.length; i++) {
            htmlCourseString += "<p>" + courseString[i] + "</p>";
            $("#calendar").html(htmlCourseString);
        }
    }, 500);
};

function createHtml(htmlStr) {
    var frag = document.createDocumentFragment();
    temp = document.createElement('div');
    temp.innerHTML = "<div float='right'>" + FCEs + "</div>" +
        "<table><tr id='tabTable'>" +
        htmlStr + "</tr></table>";
    while (temp.firstChild) {
        frag.appendChild(temp.firstChild);
    }

    return frag;
}

function createTimeTable() {
     var frag = document.createDocumentFragment();
     temp = document.createElement('div');
     var xmlreq = new XMLHttpRequest();
     xmlreq.open("GET", "res/timeTable", false);
     xmlreq.send();
     var timeTableString = xmlreq.responseText;
     temp.innerHTML = timeTableString;
     console.log(timeTableString);
     while (temp.firstChild) {
        frag.appendChild(temp.firstChild);
    }
    return frag;
}

function hoverUnfocus() {
    window[$(event.target).parent().attr("id")].unfocus();
    clearTimeout(window.mytimeout);
};

var initiallyTakeable = ["CSC104", "CSC120", "CSC108", "CSC165", "Calc1", "Lin1", "CSC200", "CSC300", "CSC490", "CSC491", "CSC494", "CSC495"];

$(document).ready(function() {
    $(".node").click(function() {
        turnNode();
    });
    $(".node").hover(function() {
        hoverFocus();
    }, function() {
        hoverUnfocus();
    });
    $(".node").attr("data-active", "inactive");
    $(".hybrid").attr("data-active", "inactive");
    $(".bool").attr("data-active", "inactive");
    $("path").attr("data-active", "inactive");

    for (var i = 0; i < initiallyTakeable.length; i++) {
        var id = initiallyTakeable[i];
        window[id].takeable = true;
        window[id].updateSVG();
    }
});


var CSCreqs = ["CSC108", "CSC148", "CSC165", "CSC207", "CSC209", "CSC236", "CSC258", "CSC263", "CSC369", "CSC373", "Calc1", "Lin1", "Sta1"];
var CSCinq =  ["CSC301", "CSC318", "CSC404", "CSC411", "CSC418", "CSC420", "CSC428", "CSC454", "CSC485", "CSC490", "CSC491", "CSC494", "CSC495"];
var active400s = [];
var active300s = [];

var reqtotal = 0;
var electotal = 0;
var posttotal = 0;

function updatePOSt(course, active) {
    if (CSCreqs.indexOf(course) > -1) {
        $('#' + course + 'check').prop('checked', active);
    } else if (course.substr(0,4) == "CSC4" || course.substr(0,4) == "ECE4") {
        if (active && active400s.indexOf(course) == -1) {
            active400s.push(course);
            if (active400s.length <= 3) {
                $('#4xx' + active400s.length).attr('value', course);
                electotal += 1;
            }
        } else if (!active) {
            var ind = active400s.indexOf(course);
            if (ind > -1) {
                active400s.splice(ind, 1);
                if (ind < 3) {
                    
                }
                if (active400s.length < 3) {
                    electotal -= 1;
                }
            }
        }


    } else if (course.substr(0,4) == "CSC3" || course.substr(0,4) == "ECE3") {
        if (active && active300s.indexOf(course) == -1) {
            active300s.push(course);
        } else if (!active) {
            var ind = active300s.indexOf(course);
            if (ind > -1) {
                active300s.splice(ind, 1);
            }
        }
    }

    for (var i = 0; i < active300s.length && i < 7; i++) {
        $('#CSC' + (i+1)).attr('value', active300s[i]);
    }

    for (var i = active300s.length; 
        i - active300s.length + 3 < active400s.length 
        && i < 7; i++) {
        $('#CSC' + (i+1)).attr('value', active400s[i+3]);
    }

    for (var i = active300s.length + Math.max(active400s.length - 3, 0);
        i < 7; i++) {
        $('#CSC' + (i+1)).attr('value', '');
    }

    console.log(active300s);

    for (var i = ind; i < 3; i++) {
        if (i < active400s.length) {
            $('#4xx' + (i+1)).attr('value', active400s[i]);    
        } else {
            $('#4xx' + (i+1)).attr('value', '');
        }
    }

    reqtotal = $('#reqPost input:checkbox:checked').length;

    $('#reqTotal').html(reqtotal);
    $('#elecTotal').html(electotal);
    $('#postTotal').html((reqtotal + electotal));

    if (CSCinq.indexOf(course) > -1) {
        $('#' + course + 'check').prop('checked', active);
    }
};
