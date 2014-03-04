// List of all nodes and edges
var nodes = [];
var edges = [];

// Track active courses
var clickedCourses = [];
var active400s = [];
var active300s = [];
var projectCourses = []; // CSC49x

// Numbers of active courses
var FCEs = 0;
var FCEs100 = 0;
var FCEs200 = 0;
var FCEs300 = 0;
var FCEs400 = 0;
var FCEsMAT = 0;

// Specialist requirements
var numBCB = 0;
var cscReqTotal = 0;
var matReqTotal = 0;
var elecTotal = 0;
var postTotal = 0;
var cscReqSat = false;
var matReqSat = false;
var elec400sSat = false;
var elecSat = false;
var peySat = false;

// Major requirements
var cscReqSatMajor = false;
var matReqSatMajor = false;
var elecSatMajor = false;
var peySatMajor = false;
var numBCBMajor = 0;
var cscReqTotalMajor = 0;
var matReqTotalMajor = 0;
var elec200sTotalMajor = 0;
var elec300sTotalMajor = 0;
var elecTotalMajor = 0;
var postTotalMajor = 0;


var activeFocus = "";


// Data Structures
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

function Edge(parent, child, name) {
  this.parent = parent;
  this.child = child;
  this.name = name;
  this.status = 'inactive';

  this.isActive = function() {
    if (!parent.active) {
      this.status = 'inactive';
    } else if (!child.active) {
      this.status = 'takeable';
    } else {
      this.status = 'active';
    }
    $('#' + this.name).attr('data-active', this.status);
  }
}


function Node(parents, type, name) {
  this.name = name;
  this.parents = parents;
  this.children = [];
  this.outEdges = [];
  this.inEdges = [];
  this.logicalType = type;
  this.active = false;
  this.takeable = false;
  this.updated = false;
  this.hybrid = false;

  for (var i = 0; i < this.parents.length; i++) {
    this.parents[i].children.push(this);
  }

  // Activate/deactivate a node; called when a node is clicked.
  this.turn = function() {
    if (this.active || this.takeable) {
      $.each(nodes, function(i, node) {
        window[node].updated = false;
      });

      this.updated = true;
      this.active = !this.active;


      // We seem to be assuming here that this is an 'AND' node
      this.takeable = true;
      this.checkFCEBasedPrerequisites();
      for (var i = 0; i < this.parents.length; i++) {
        // Note that we don't need to call isActive here
        // Parents don't change
        this.takeable = this.takeable && this.parents[i].active;
      }

      $.each(this.children, function(i, node) {
        node.isActive();
      });
      $.each(this.outEdges, function(i, edge) {
        edge.isActive();
      });
      $.each(this.inEdges, function(i, edge) {
        edge.isActive();
      });


      // Update interface
      this.updateSVG();
      updateClickedCourses(this.name, this.active);
      updatePOSt(this.name, this.active);
    }
  }

  
  this.isActive = function() {
    if (this.updated) {
      return this.active;
    } else {
      var changed = false;
      this.updated = true;

      if (this.logicalType == "AND") {
        this.takeable = true;
        this.checkFCEBasedPrerequisites();
        for (var i = 0; i < this.parents.length; i++) {
          this.takeable = this.takeable && this.parents[i].isActive();
        }
      } else { // this.logicalType == "OR"
        this.takeable = false;
        this.checkFCEBasedPrerequisites();
        for (var i = 0; i < this.parents.length; i++) {
          this.takeable = this.takeable || this.parents[i].isActive();
        }
      }

      if (this.active && !this.takeable) {
        this.active = false;
        changed = true;
      } else if (!this.active && this.takeable && this.hybrid) {
        this.active = true;
        changed = true;
      }

      if (changed) {
        $.each(this.children, function(i, node) {
          node.isActive();
        });
        $.each(this.outEdges, function(i, edge) {
          edge.isActive();
        });
        $.each(this.inEdges, function(i, edge) {
          edge.isActive();
        });
        if (!this.hybrid) {
          updateClickedCourses(this.name, this.active);
          updatePOSt(this.name, this.active);
        }
      }

      // Takeable status might be changed, even if active isn't
      this.updateSVG();

      return this.active;
    }
  }


  this.checkFCEBasedPrerequisites = function() {
    if (this.name == "CSC454") {
      this.takeable = FCEs300 + FCEs400 >= 2.5;
    } else if (this.name == "CSC494" || this.name == "CSC495") {
      this.takeable = FCEs300 + FCEs400 >= 1.5;
    } else if (this.name == 'CSC318') {
      this.takeable = FCEs >= 0.5;
    }
  }

  // Used when entering "spotlight mode" (upon hover)
  this.focus = function() {
    if (!this.active) {
      $("#" + this.name).attr('data-active', 'missing');
      $.each(this.inEdges, function(index, edge) {
        if (!edge.parent.active) {
          $("#" + edge.name).attr('data-active', 'missing');
        }
      })

      $.each(this.parents, function(i, node) {
        node.focus();
      });
    }
  }


  // Used when leaving "spotlight mode" (upon leaving hover)
  this.unfocus = function() {
    if (!this.active) {
      if (activeFocus == '' || window[activeFocus + "FocusList"].indexOf(this.name) > -1) {
        this.updateSVG();
      } else {
        $("#" + this.name).attr('data-active', 'unlit');
      }

      $.each(this.parents, function(i, node) {
        node.unfocus();
      });
      $.each(this.outEdges, function(i, edge) {
        edge.isActive();
      });
    }
  }


  // Update the style of this node
  this.updateSVG = function() {
    if (this.active) {
      $("#" + this.name).attr('data-active', 'active');
    } else if (this.takeable) {
      $("#" + this.name).attr('data-active', 'takeable');
    } else {
      $("#" + this.name).attr('data-active', 'inactive');
    }
  }
}


/*
 * Mouse Event Callbacks
 */

// Set mouse callbacks on all graph nodes
function setMouseCallbacks() {
  $(".node").click(function(event) {
    turnNode(event);
  });
  $(".node").hover(
    function(event) {
      hoverFocus(event);
    },
    function(event) {
      hoverUnfocus(event);
    });
}


// Activates missing prerequisite display and 
// fetches course description on hover
function hoverFocus(event) {
  var id = event.target.parentNode.id;
  // Highlight missing prerequisites
  window[id].focus();

  // Fetch course description
  fetchCourseDescription(id);
};


// Deactivate missing prerequisites
function hoverUnfocus(event) {
  var id = event.target.parentNode.id;
  window[id].unfocus();
};


// Callback when a node is clicked
function turnNode(event) {
  if (activeFocus == '') {
    var id = event.target.parentNode.id;
    window[id].turn();
    
    updatePostInterface();
    updateMajorPostInterface();

    updateMyCoursesTab();
    updateFCECount();

    // Check the courses with FCE reqs
    CSC318.isActive();
    CSC454.isActive();
    CSC494.isActive();
    CSC495.isActive();
  }
};


// Resets interface to default (nothing selected)
function reset() {
  // Deactivate focus, if necessary
  if (activeFocus != '') {
    $(".focusTabs").tabs("option", "active", false);
    $("ellipse.spotlight").remove();
    curtains();
  }

  // Deactivate nodes
  $.each(nodes, function(i, node) {
    window[node].active = false;
    window[node].takeable = initiallyTakeable.indexOf(node) > -1;
    window[node].updateSVG();
  });

  // Deactivate edges
  $('path').attr('data-active', 'inactive');

  // Remove nodes from table
  $('#courseGrid').empty();

  // Reset FCE counts
  FCEs = 0;
  FCEs100 = 0;
  FCEs200 = 0;
  FCEs300 = 0;
  FCEs400 = 0;
  FCEsMAT = 0;
  clickedCourses = [];
  $('#FCEcount').html('0.0');

  active300s = [];
  active400s = [];
  projectCourses = [];
  $('input:checkbox').attr('checked', false);
  $('input:text').attr('value', '');

  updatePostInterface();
  updateMajorPostInterface();
};


/*
 * Tab functions
 */

// My Courses Tab
// Updates the "My Courses" tab
// Note: not called on hybrids
function updateClickedCourses(name, active) {
  var i = clickedCourses.indexOf(name);
  var diff = 0;
  if (active && i == -1) {
    if (name == 'CSC200' || name == 'Calc1') {
      diff = 1;
    } else {
      diff = 0.5;
    }
    clickedCourses.push(name);
  } else if (!active && i > -1) {
    if (name == 'CSC200' || name == 'Calc1') {
      diff = -1;
    } else {
      diff = -0.5;
    }
    clickedCourses.splice(i, 1);
  }

  if (name == 'Calc1' || name == "Lin1" || name == "Sta1" || name == "Sta2") {
    FCEsMAT += diff;
  } else if (name.charAt(3) == '1') {
    FCEs100 += diff;
  } else if (name.charAt(3) == '2') {
    FCEs200 += diff;
  } else if (name.charAt(3) == '3') {
    FCEs300 += diff;
  } else if (name.charAt(3) == '4') {
    FCEs400 += diff;
  } 

}

function updateMyCoursesTab() {
  $('#courseGrid').empty();

  // Get data from course calendar        
  var htmlClickedString = $.map(clickedCourses, function(course) {
    var courseStringText = '';
    if (course == 'Calc1') {
      courseStringText = 'First-year calculus: MAT135/136, MAT137, or MAT157';
    } else if (course == 'Lin1') {
      courseStringText = 'One term in linear algebra: MAT221, MAT223, or MAT240';
    } else if (course == 'Sta1') {
      courseStringText = 'One term in probability theory: STA247, STA255, or STA257';
    } else if (course == 'Sta2') {
      courseStringText = 'One term in statistics: STA248 or STA261';
    } else {
      var xmlreq = new XMLHttpRequest();
      xmlreq.open("GET", "res/calendar.txt", false);
      xmlreq.send();
      var patt1 = new RegExp("\n" + course + ".*", "im");
      courseStringText = xmlreq.responseText.match(patt1)[0].split(course)[1].split("1")[1].split("[")[0];
    }
    return "<td class='courseCell' style='background: " + $("#" + course + "> rect").css('fill') + "'><div id='" + course + "cell'><p class='courseName'>" + course + "</p><p class=" + course + "text>" + courseStringText + "</p></div></td>";
  }).join("");

  $('#courseGrid').html(htmlClickedString);
}

function updateFCECount() {
  FCEs = FCEs100 + FCEs200 + FCEs300 + FCEs400 + FCEsMAT;
  $('#FCEcount').html(FCEs.toFixed(1));
}


// Course Description Tab

// Read course description from resource files
function fetchCourseDescription(id) {
  // Get data from course calendar
  if (id == "Calc1" || id == "Lin1" || id == "Sta1" || id == "Sta2") {
    var calendarUrl = 'res/' + id + '_calendar.txt';
    var pattern = new RegExp('[.\n]*', 'm');
  } else {
    var calendarUrl = 'res/calendar.txt';
    var calendarParser = new RegExp("\n" + id + "(.|\n)*?Breadth Requirement.*\n", "im");
  }

  $.ajax({
    url: calendarUrl,
    type: 'GET',
    dataType: 'text',
    success: function(response) {
      if (id == "Calc1" || id == "Lin1" || id == "Sta1" || id == "Sta2") {
        var courseString = response.split('\n');
      } else {
        var courseString = response.match(calendarParser)[0].split('\n');
        courseString.shift();

        // Add extra description for enriched courses
        if (id == "CSC165" || id == "CSC236") {
          var calendarParserEnriched = new RegExp("\nCSC240(.|\n)*?Breadth Requirement.*\n", "im");
          var courseString2 = response.match(calendarParserEnriched)[0].split('\n');
          courseString = courseString.concat("<hr/>", courseString2);
        } else if (id == "CSC263") {
          var calendarParserEnriched = new RegExp("\nCSC265(.|\n)*?Breadth Requirement.*\n", "im");
          var courseString2 = response.match(calendarParserEnriched)[0].split('\n');
          courseString = courseString.concat("<hr/>", courseString2);
        }
      }

      $('#calendar').html($.map(courseString, function(s) {
        return '<p>' + s + '</p>';
      }).join(''));
    }
  })
};


// Focus Tab

// Activate a focus
function showtime(id) {
  var focus = window[id + "FocusList"];
  $("ellipse.spotlight").remove();
  if (activeFocus == id) {
    curtains();
  } else {
    $("#graph").css("background", "rgb(40,40,40)");
    $(".node, .hybrid").attr('data-active', 'unlit');
    $.each(focus, function(index, elem) {
      spotlight(elem);
    });
    $("#graph").html($("#graph").html());
    setMouseCallbacks();
    activeFocus = id;
    $(".closeIcon").remove(); // Remove old icon.
    $(".focusList a[href='#" + id + "Details']").append(
      "<img class='closeIcon' src='res/close.ico' alt='Click to close!'/>"); // Put in new icon.
  }
}

// Removes spotlight on active focus
function curtains() {
  $("#graph").css("background", "white");
  $(".closeIcon").remove();
  activeFocus = "";
  $.each(nodes, function(index, elem) {
    window[elem].updateSVG();
  });

}

function spotlight(id) {
  var node = $("#".concat(id, " > rect"));

  var width = parseFloat(node.attr("width")) / 2;
  var height = parseFloat(node.attr("height")) / 2;
  var x = parseFloat(node.attr("x")) + width;
  var y = parseFloat(node.attr("y")) + height;

  var el = '<ellipse class="spotlight" cx="'.concat(x, '" cy = "', y, '" rx="', width + 9, '" ry="', height + 8.5, '"/>');
  $("#" + id).before(el);
  $("#" + id).attr('data-active', 'lit');

  window[id].updateSVG();
}


// Timetable Tab

// Generate html for timetable 
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

// Fetch timetable information
function createTimeTable() {
  var frag = document.createDocumentFragment();
  temp = document.createElement('div');
  var xmlreq = new XMLHttpRequest();
  xmlreq.open("GET", "res/timeTable.txt", false);
  xmlreq.send();
  var timeTableString = xmlreq.responseText;
  $.each(nodes, function(index, node) {
    timeTableString = timeTableString.replace(
      new RegExp("backgroundInstertion\">" + node, 'g'),
      $("#" + node + "> rect").css('fill') + "\">" + node);
  });

  // Hardcoded values for enriched theory courses
  timeTableString = timeTableString.replace(
      new RegExp("backgroundInstertion\">CSC240", 'g'),
      $("#CSC165 > rect").css('fill') + "\">CSC240");

  timeTableString = timeTableString.replace(
      new RegExp("backgroundInstertion\">CSC263", 'g'),
      $("#CSC165 > rect").css('fill') + "\">CSC263");

  temp.innerHTML = timeTableString;
  while (temp.firstChild) {
    frag.appendChild(temp.firstChild);
  }
  return frag;
}



// POSt Tab
function updatePOSt(course, active) {
  if (reqs.indexOf(course) > -1) { // Required course
    $('#' + course + 'check').prop('checked', active);
    $('#' + course + 'checkMajor').prop('checked', active);
  } else {
    if (course.substr(0, 5) == "CSC49") {
      var ind = projectCourses.indexOf(course);
      if (active && ind == -1) {
        projectCourses.push(course);
      } else if (!active && ind > -1) {
        projectCourses.splice(ind, 1);
      }
    } else if (course.substr(0, 4) == "CSC4" || course.substr(0, 4) == "ECE4") { // 4th year course
      var ind = active400s.indexOf(course);
      if (active && ind == -1) {
        active400s.push(course);
      } else if (!active && ind > -1) {
        active400s.splice(ind, 1);
      }
    } else if (course.substr(0, 4) == "CSC3" || course.substr(0, 4) == "ECE3") { // 3rd year course
      var ind = active300s.indexOf(course);
      if (active && ind == -1) {
        active300s.push(course);
      } else if (!active && ind > -1) {
        active300s.splice(ind, 1);
      }
    }

    if (CSCinq.indexOf(course) > -1) {
      $('#' + course + 'check').prop('checked', active);
      $('#' + course + 'checkMajor').prop('checked', active);
    }
  }

  $('#' + course + 'check').prop('checked', active);
  $('#' + course + 'checkMajor').prop('checked', active);
};


function updatePostInterface() {
  updateCSCReqs();
  updateMATReqs();
  updateCSC400s();
  updateElecs();
  updatePEY();
  updatePOStTotal();

  setIcon('specCheck', 
    cscReqSat && matReqSat && elec400sSat && elecSat && peySat);
}

function updateMajorPostInterface() {
  updateCSCReqsMajor();
  updateMATReqsMajor();
  updateElecsMajor();
  updatePEYMajor();
  updatePOStTotalMajor();

  setIcon('majorCheck', 
    cscReqSatMajor && matReqSatMajor && elecSatMajor && peySatMajor);
}

function setIcon(id, sat) {
  if (sat) {
    $('a[href="#' + id + '"] img').attr('src', 'res/check.ico');
  } else {
    $('a[href="#' + id + '"] img').attr('src', 'res/delete.ico');
  }
}

function updateCSCReqs() {
  cscReqTotal = $('#cscReqs input:checkbox:checked').length / 2;
  $('#cscReqTotal').html(cscReqTotal.toFixed(1));
  cscReqSat = cscReqTotal >= 5;
  setIcon('cscReqs', cscReqSat);
}

function updateCSCReqsMajor() {
  cscReqTotalMajor = $('#cscReqsMajor input:checkbox:checked').length / 2;
  $('#cscReqTotalMajor').html(cscReqTotal.toFixed(1));
  cscReqSatMajor = cscReqTotalMajor >= 3.5;
  setIcon('cscReqsMajor', cscReqSatMajor);
}


function updateMATReqs() {
  matReqTotal = $('#matReqs input:checkbox:checked').length / 2;
  if ($('#Calc1check').prop('checked')) {
    matReqTotal += 0.5;
  }
  $('#matReqTotal').html(matReqTotal.toFixed(1));
  matReqSat = matReqTotal >= 2;
  setIcon('matReqs', matReqSat);
}

function updateMATReqsMajor() {
  matReqTotalMajor = $('#matReqsMajor input:checkbox:checked').length / 2;
  if ($('#Calc1checkMajor').prop('checked')) {
    matReqTotalMajor += 0.5;
  }
  $('#matReqTotalMajor').html(matReqTotalMajor.toFixed(1));
  matReqSatMajor = matReqTotalMajor >= 1.5;
  setIcon('matReqsMajor', matReqSatMajor);
}


function update200sElecsMajor() {
  elec200sTotalMajor = 0;
  if ($('#Calc2checkMajor').prop('checked')) {
    elec200sTotalMajor += 1;
  }
  if ($('#CSC200checkMajor').prop('checked')) {
    elec200sTotalMajor += 1;
  }
  if ($('#CSC209checkMajor').prop('checked')) {
    elec200sTotalMajor += 0.5;
  }
  if ($('#Lin1checkMajor').prop('checked')) {
    elec200sTotalMajor += 0.5;
  }

  if (elec200sTotalMajor > 1) {
    elec200sTotalMajor = 1;
  }
}

var extraMajor = 0;

function update300sElecsMajor() {
  numBCBMajor = $('#300sElecsMajor input:checkbox:checked').length;
  var numProjects = 2;
  if ($('#BCB430checkMajor').prop('checked')) {
    numBCBMajor += 1;
    numProjects = 0;
  }

  var tmp = active300s.concat(active400s, projectCourses.slice(0, numProjects));
  // Manually add active 3rd year courses required by specialist
  if (CSC373.active) {
    tmp.push('CSC373');
    extraMajor += 0.5;
  }

  if (CSC369.active) {
    tmp.push('CSC369');
    extraMajor += 0.5
  }

  for (var i = 1; i <= 6; i++) {
    if (i <= tmp.length) {
      $('#3xx' + i + 'Major').attr('value', tmp[i-1]);
    } else {
      $('#3xx' + i + 'Major').attr('value', '');
    }
  }


  elec300sTotalMajor = tmp.length + numBCBMajor;

  for (var i = 1; i <= 3; i++) {
    if ($('#MAT' + i + "Major").prop('value').substr(0,3) == 'MAT') {
      elec300sTotalMajor += 1;
    }
  }

  elec300sTotalMajor /= 2;
}


function updateElecsMajor() {
  update200sElecsMajor();
  update300sElecsMajor();

  elecTotalMajor = elec200sTotalMajor + elec300sTotalMajor;
  if (elecTotalMajor >= 3) {
    elecTotalMajor = 3;
  }

  var numProjects = 2;
  if ($('#BCB430check').prop('checked')) {
    numProjects = 0;
  }

  elecSatMajor = elecTotalMajor >= 3 
    && (active400s.length > 0 || numBCBMajor > 0)
    && (active300s.concat(active400s, projectCourses.slice(0, numProjects)).length + extraMajor >= 3);
  
  $('#elecTotalMajor').html(elecTotalMajor.toFixed(1));

  setIcon('200sElecsMajor', elecSatMajor);
  setIcon('300sElecsMajor', elecSatMajor);
}



function updateCSC400s() {
  numBCB = $('#csc400s input:checkbox:checked').length;

  var numProjects = 2;
  if ($('#BCB430check').prop('checked')) {
    numBCB += 1;
    numProjects = 0;
  }

  var tmp = active400s.concat(projectCourses.slice(0, numProjects));

  for (var i = 1; i <= 3; i++) {
    if (i <= 3 - numBCB && i <= tmp.length) {
      $('input#4xx' + i).attr('value', tmp[i - 1]);
    } else {
      $('input#4xx' + i).attr('value', '');
    }
  }

  elec400sSat = numBCB + tmp.length >= 3;
  setIcon('csc400s', elec400sSat);
}

// Right now, it must be called after updateCSC400s (because of numBCB)
function updateElecs() {
  var numProjects = 2;
  if ($('#BCB430check').prop('checked')) {
    numProjects = 0;
  }
  var tmp = active300s.concat(active400s.slice(3 - numBCB), projectCourses.slice(Math.max(3 - numBCB - active400s.length, 0), numProjects));
  for (var i = 1; i <= 7; i++) {
    if (i <= tmp.length) {
      $('#inputCSC' + i).attr('value', tmp[i - 1]);
    } else {
      $('#inputCSC' + i).attr('value', '');
    }
  }

  var matElecs = 0;

  $('#matElecs input:text').each(function(index) {
    if (this.value == 'MAT235' || this.value == 'MAT237' || this.value == 'MAT257' || this.value == 'MAT235Y1' || this.value == 'MAT237Y1' || this.value == 'MAT257Y1') {
      matElecs += 1;
    } else if (this.value.substr(0, 3) == 'MAT' || this.value.substr(0, 3) == 'STA') {
      matElecs += 0.5;
    }
  });

  elecTotal = (active300s.length + active400s.length + numBCB) / 2 + matElecs;
  if (!$('#BCB430check').prop('checked')) {
    elecTotal += Math.min(projectCourses.length / 2, 1);
  }
  if (elecTotal >= 5) {
    elecTotal = 5;
  }

  $('#elecTotal').html(elecTotal.toFixed(1));
  elecSat = elecTotal == 5;
  setIcon('cscElecs', elecSat);
  setIcon('matElecs', elecSat);
}

function updatePEYMajor() {
  peySatMajor = $('#peycheckMajor').prop('checked') || $('#peyReqMajor input:checkbox:checked').length > 0;
  setIcon('peyReqMajor', peySatMajor);
}

function updatePEY() {
  peySat = $('#peycheck').prop('checked') || $('#peyReq input:checkbox:checked').length > 0;
  setIcon('peyReq', peySat);
}

function updatePOStTotalMajor() {
  $('#postTotalMajor').html((cscReqTotalMajor + matReqTotalMajor + elecTotalMajor).toFixed(1));
}

function updatePOStTotal() {
  $('#postTotal').html((cscReqTotal + matReqTotal + elecTotal).toFixed(1));
}

function setGraphSize() {
    // Set height of tabs
  //var graphHeight = Math.min(Math.round($(window).height() * 0.7), $(window).width() / 2);
  //var height = $(window).height() - graphHeight - 46;
  console.log("Resizing graph");
  var height = $(window).height() - $('#graph').height() - 46;
  //$(".infoTabs").height(height + "px");
  //$("#graph").height(graphHeight + "px");
  //$('#graph').css('margin-left', 'auto');
  //$('#graph').css('margin-right', 'auto');
  //$("#graph").width('100%');
  //$(".infoTabs").height("100%");
  //$(".infoTabs").width($(window).width() + "px");
  if ($(window).width() < 1400) {
    $(".infoTabs").width("1400px");
  } else {
    $(".infoTabs").width("100%");
  }
}

$(document).ready(function() {

  // Set height of tabs
  //var graphHeight = Math.min(Math.round($(window).height() * 0.7), $(window).width() / 2);
  //var height = $(window).height() - graphHeight - 46;
  var height = $(window).height() - $('#graph').height() - 46;
  //$(".infoTabs").height(height + "px");
  //$("#graph").height(graphHeight + "px");
  //$('#graph').css('margin-left', 'auto');
  //$('#graph').css('margin-right', 'auto');
  //$("#graph").width('100%');
  //$(".infoTabs").height("100%");
  //$(".infoTabs").width($(window).width() + "px");
  //$("#graph").width(screen.availWidth + "px");
  //$(".infoTabs").width(screen.availWidth + "px");
  if ($(window).width() < 1200) {
    $(".infoTabs").width("1200px");
  } else {
    $(".infoTabs").width("100%");
  }

  // Enable tabs
  $('.infoTabs').tabs({
    activate: function(e, ui) {
      e.currentTarget.blur();
    }
  });
  $('.focusTabs').tabs({
    active: false,
    collapsible: true,
    activate: function(e, ui) {
      e.currentTarget.blur();
    }
  });

  $('.postTypeTabs').tabs({
    active: 0,
    activate: function(e, ui) {
      e.currentTarget.blur();
    }
  });

  $('.postTabs').tabs({
    active: 0,
    activate: function(e, ui) {
      e.currentTarget.blur();
    }
  });
  setMouseCallbacks();

  $(".node").attr("data-active", "inactive");
  $(".hybrid").attr("data-active", "inactive");
  $(".bool").attr("data-active", "inactive");
  $("path").attr("data-active", "inactive");

  $("input:checkbox").prop('checked', false);

  for (var i = 0; i < initiallyTakeable.length; i++) {
    var id = initiallyTakeable[i];
    window[id].takeable = true;
    window[id].updateSVG();
  }

  var fragment2 = createTimeTable();
  timeNode = parent.document.getElementById("timetable");
  timeNode.appendChild(fragment2);

  // Enable email
  $("#submit_btn").click(function() { 
        var user_name       = $('input[name=name]').val(); 
        var user_message    = $('textarea[name=message]').val();
        var proceed = true;

        if(proceed) 
        {
            post_data = {'userName':user_name, 'userMessage':user_message};
            $.post('email.php', post_data, function(data){       
                $("#result").hide().html('<div class="success">'+data+'</div>').slideDown();
                $('#contact_form input').val(''); 
                $('#contact_form textarea').val(''); 
                
            }).fail(function(err) {
                $("#result").hide().html('<div class="error">'+err.statusText+'</div>').slideDown();
            });
        }
                
    });
    
    $("#contact_form input, #contact_form textarea").keyup(function() { 
        $("#contact_form input, #contact_form textarea").css('border-color', ''); 
        $("#result").slideUp();
    });
    
    /*Search function for TimeTable*/
    $("#filter").keyup(function() {
        var filter = $(this).val();
        $(".searchClass").each(function() {
            if ($(this).text().search(new RegExp(filter, "i")) < 0) {
                $(this).fadeOut();
            } else {
                $(this).show();
            }
        });
    });


    /* Draggable function for map*/
    /* Extending the jQuery draggable option to be fitted with right click for either graph or graphRootSVG. 
    This also disables the context menu for graphRootSVG, but not for the tab.
    $.extend($.ui.draggable.prototype, {
        _mouseInit: function () {
            var context = this;
            if (!this.options.mouseButton) {
                this.options.mouseButton = 1;
            }

            $.ui.mouse.prototype._mouseInit.apply(this, arguments);
            this.started = false;
        },
        _mouseDown: function (event) {

            (this._mouseStarted && this._mouseUp(event));

            this._mouseDownEvent = event;

            var that = this,
                btnIsLeft = (event.which === this.options.mouseButton),

                elIsCancel = (typeof this.options.cancel === "string" && event.target.nodeName ? $(event.target).closest(this.options.cancel).length : false);
            if (!btnIsLeft || elIsCancel || !this._mouseCapture(event)) {
                return true;
            }

            this.mouseDelayMet = !this.options.delay;
            if (!this.mouseDelayMet) {
                this._mouseDelayTimer = setTimeout(function () {
                    that.mouseDelayMet = true;
                }, this.options.delay);
            }

            if (this._mouseDistanceMet(event) && this._mouseDelayMet(event)) {
                this._mouseStarted = (this._mouseStart(event) !== false);
                if (!this._mouseStarted) {
                    event.preventDefault();
                    return true;
                }
            }

            // This is apparently for Gecko and Opera, but I haven't tested it out yet.
            if (true === $.data(event.target, this.widgetName + ".preventClickEvent")) {
                $.removeData(event.target, this.widgetName + ".preventClickEvent");
            }

            this._mouseMoveDelegate = function (event) {
                return that._mouseMove(event);
            };
            this._mouseUpDelegate = function (event) {
                return that._mouseUp(event);
            };
            
            $(document)
                .bind("mousemove." + this.widgetName, this._mouseMoveDelegate)
                .bind("mouseup." + this.widgetName, this._mouseUpDelegate);

            event.preventDefault();

            mouseHandled = true;
            return true;
        }
    });

    $("#graphRootSVG").draggable({
        mouseButton: 3
    });
    */
});

// Disables Tab key
document.onkeydown = function (e) {
        if(e.which == 9){
                return false;
        }
}


