// List of all nodes and edges
var nodes = [];
var edges = [];

var clickedCourses = [];
var active400s = [];
var active300s = [];
var projectCourses = [];

var FCEs = 0;
var FCEs100 = 0;
var FCEs200 = 0;
var FCEs300 = 0;
var FCEs400 = 0;
var FCEsMAT = 0;

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

var reqs = ["CSC108", "CSC148", "CSC165", "CSC207", "CSC209", "CSC236", "CSC258", "CSC263", "CSC369", "CSC373", "Calc1", "Lin1", "Sta1"];
var CSCinq = ["CSC301", "CSC318", "CSC404", "CSC411", "CSC418", "CSC420", "CSC428", "CSC454", "CSC485", "CSC490", "CSC491", "CSC494", "CSC495"];
var sciFocusList = ["CSC336", "CSC446", "CSC456", "CSC320", "CSC418", "CSC321", "CSC411", "CSC343", "CSC384", "CSC358", "CSC458"];
var AIFocusList = ["CSC310", "CSC330", "CSC438", "CSC448", "CSC463", "CSC401", "CSC485", "CSC320", "CSC420", "CSC321", "CSC411", "CSC412", "CSC384", "CSC486"];
var NLPFocusList = ["CSC318", "CSC401", "CSC485", "CSC309", "CSC321", "CSC330", "CSC411", "CSC428", "CSC486"];
var visionFocusList = ["CSC320", "CSC336", "CSC411", "CSC420", "CSC418", "CSC412"];
var systemsFocusList = ["CSC324", "CSC343", "CSC443", "CSC469", "CSC488", "CSC372", "ECE385", "CSC358", "CSC458", "CSC301", "CSC309", "CSC410", "ECE489"];
var gameFocusList = ["CSC300", "CSC301", "CSC318", "CSC324", "CSC384", "CSC418", "CSC404"];
var HCIFocusList = ["CSC300", "CSC301", "CSC318", "CSC428", "CSC309", "CSC320", "CSC321", "CSC343", "CSC384", "CSC401", "CSC404", "CSC418", "CSC485", "CSC490", "CSC491"];
var theoryFocusList = ["CSC336", "CSC463", "CSC310", "CSC438", "CSC448", "Sta2"];
var webFocusList = ["Sta2", "CSC309", "CSC343", "CSC358", "CSC458", "CSC411", "CSC310", "CSC443", "CSC469"];

var initiallyTakeable = ["CSC104", "CSC120", "CSC108", "CSC165", "Calc1", "Lin1", "CSC200", "CSC300", "CSC490", "CSC491"];

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
  clearTimeout(window.mytimeout);
};


// Callback when a node is clicked
function turnNode(event) {
  if (activeFocus == '') {
    var id = event.target.parentNode.id;
    window[id].turn();
    // Check the courses with FCE reqs
    CSC318.isActive();
    CSC454.isActive();
    CSC494.isActive();
    CSC495.isActive();

    updateCSCReqs();
    updateMATReqs();
    updateCSC400s();
    updateElecs();
    updatePEY();

    updateMyCoursesTab();
    updateFCECount();

    $('#postTotal').html((cscReqTotal + matReqTotal + elecTotal));
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
  $('#FCEcount').html(FCEs.toFixed(1));

  active300s = [];
  active400s = [];
  $('input:checkbox').attr('checked', false);
  $('input:text').attr('value', '');

  updatePostInterface();
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
          courseString = courseString.concat(courseString2);
        } else if (id == "CSC263") {
          var calendarParserEnriched = new RegExp("\nCSC265(.|\n)*?Breadth Requirement.*\n", "im");
          var courseString2 = response.match(calendarParserEnriched)[0].split('\n');
          courseString = courseString.concat(courseString2);
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
    }
  }
};


function updatePostInterface() {
  updateCSCReqs();
  updateMATReqs();
  updateCSC400s();
  updateElecs();
  updatePEY();
  updatePOStTotal();
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

function updateMATReqs() {
  matReqTotal = $('#matReqs input:checkbox:checked').length / 2;
  if ($('#Calc1check').prop('checked')) {
    matReqTotal += 0.5;
  }
  $('#matReqTotal').html(matReqTotal.toFixed(1));
  matReqSat = matReqTotal >= 2;
  setIcon('matReqs', matReqSat);
}

function updateCSC400s() {
  numBCB = $('#csc400s input:checkbox:checked').length;

  var tmp = active400s.concat(projectCourses.slice(0, 2));

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
  var tmp = active300s.concat(active400s.slice(3 - numBCB), projectCourses.slice(0, 2));
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

  elecTotal = (active300s.length + active400s.length + numBCB) / 2 + matElecs + Math.min(projectCourses.length / 2, 1);
  if (elecTotal >= 5) {
    elecTotal = 5;
  }

  $('#elecTotal').html(elecTotal.toFixed(1));
  elecSat = elecTotal == 5;
  setIcon('cscElecs', elecSat);
  setIcon('matElecs', elecSat);
}

function updatePEY() {
  peySat = $('#peycheck').prop('checked') || $('#peyReq input:checkbox:checked').length > 0;
  setIcon('peyReq', peySat);
}

function updatePOStTotal() {
  $('#postTotal').html((cscReqTotal + matReqTotal + elecTotal).toFixed(1));
}

$(document).ready(function() {

  // Set height of tabs
  var height = $(window).height() - $("#graph").height() - 46;
  $(".infoTabs").height(height + "px");

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
    var user_message = $('textarea[name=message]').val();
    var proceed = true;

    if (proceed) {
      post_data = {
        'userMessage': user_message
      };
      $.post('email.php', post_data, function(data) {
        $("#result").hide().html('<div class="success">' + data + '</div>').slideDown();
        $('#contact_form input').val('');
        $('#contact_form textarea').val('');

      }).fail(function(err) {
        $("#result").hide().html('<div class="error">' + err.statusText + '</div>').slideDown();
      });
    }

  });

  // Might be a bit useless.
  $("#contact_form input, #contact_form textarea").keyup(function() {
    $("#contact_form input, #contact_form textarea").css('border-color', '');
    $("#result").slideUp();
  });

});



/*
 * The actual dependencies.
 */

// Math courses
makeNode([], "AND", "Calc1");
makeNode([Calc1], "AND", "Sta1");
makeNode([Sta1], "AND", "Sta2");
makeNode([], "AND", "Lin1");

// Math Hybrids
makeHybrid([Sta1], "AND", "hybrid6");
makeHybrid([Lin1], "AND", "hybrid7");
makeHybrid([Sta1], "AND", "hybrid8");
makeHybrid([Sta2], "AND", "hybrid9");
makeHybrid([hybrid6, hybrid7], "AND", "bool4");
makeHybrid([Calc1, Lin1], "AND", "bool5");

// First year courses
makeNode([], "AND", "CSC104");
makeNode([], "AND", "CSC108");
makeNode([], "AND", "CSC120");
makeNode([CSC108], "AND", "CSC148");
makeNode([], "AND", "CSC165");

// First year hybrids
makeHybrid([CSC148], "AND", "hybrid13")
makeHybrid([CSC165, Calc1], "OR", "hybrid16");

// Second year courses
makeNode([], "AND", "CSC200");
makeNode([CSC148], "AND", "CSC207");
makeNode([CSC207], "AND", "CSC209");
makeNode([CSC148, CSC165], "AND", "CSC236");
makeNode([CSC148, CSC165], "AND", "CSC258");

// Second year hybrids and CSC263
makeHybrid([CSC236, CSC207], "AND", "bool1");
makeHybrid([CSC209, CSC258], "AND", "bool2");
makeNode([bool1, hybrid6], "AND", "CSC263"); // CSC263
makeHybrid([CSC263], "AND", "hybrid2");
makeHybrid([CSC209], "AND", "hybrid12");
makeHybrid([CSC263], "AND", "hybrid10");
makeHybrid([CSC236], "AND", "hybrid11");
makeHybrid([CSC207], "AND", "hybrid14");
makeHybrid([CSC263], "AND", "hybrid15");

// Third year courses
makeNode([], "AND", "CSC300");
makeNode([CSC209, hybrid10], "AND", "CSC301");
makeNode([CSC301], "AND", "CSC302");
makeNode([hybrid13, bool4], "AND", "CSC310");
makeNode([], "AND", "CSC318");
makeNode([hybrid12, bool5], "AND", "CSC320");
makeNode([bool4], "AND", "CSC321");
makeNode([CSC263], "AND", "CSC324"); // CHANGED
makeNode([CSC236], "AND", "CSC330");
makeNode([CSC148, bool5], "AND", "CSC336");
makeNode([hybrid16, hybrid14], "AND", "CSC343");
makeNode([CSC209, CSC343], "AND", "CSC309");
makeNode([bool2, hybrid2], "AND", "CSC358");
makeNode([bool2], "AND", "CSC369");
makeNode([bool2], "AND", "CSC372");
makeNode([CSC263], "AND", "CSC373");
//makeNode([CSC263, hybrid8], "AND", "CSC384"); // CHANGED
makeNode([CSC263, hybrid8], "AND", "CSC384");
makeNode([bool2], "AND", "ECE385");

// Third year hybrids
makeHybrid([CSC324], "AND", "hybrid4");
makeHybrid([CSC373], "AND", "hybrid5");

// Fourth year courses
makeNode([CSC207, hybrid8], "AND", "CSC401");
makeNode([bool1], "AND", "CSC410");
makeNode([CSC263, Calc1, hybrid9], "AND", "CSC411");
makeNode([CSC411], "AND", "CSC412");
makeNode([hybrid15, bool5], "AND", "CSC420");
makeNode([CSC318, Sta2, hybrid12], "AND", "CSC428");
makeNode([CSC336], "AND", "CSC436");
makeNode([CSC343, CSC369, hybrid5], "AND", "CSC443");
makeNode([CSC336], "AND", "CSC446");
makeNode([], "AND", "CSC454");
makeNode([hybrid12, CSC336], "AND", "CSC456");
makeNode([bool2, hybrid2], "AND", "CSC458");
makeNode([CSC236], "AND", "CSC463");
makeNode([CSC463], "AND", "CSC448"); // CSC448
makeNode([CSC236], "AND", "CSC465");
makeNode([CSC369], "AND", "CSC469");
makeNode([hybrid8, CSC209], "AND", "CSC485");
makeNode([CSC258, hybrid4, hybrid2], "AND", "CSC488");
makeNode([hybrid11], "AND", "ECE489");
makeNode([], "AND", "CSC490");
makeNode([], "AND", "CSC491");
makeNode([], "AND", "CSC494");
makeNode([], "AND", "CSC495");

// Fourth year hybrids and CSC418, CSC438, and CSC486
makeHybrid([CSC373, CSC463], "OR", "bool3");
makeHybrid([CSC336, CSC373, CSC463], "OR", "CSC336orCSC373orCSC463");
makeHybrid([CSC336orCSC373orCSC463, Calc1], "AND", "hybrid3");
makeNode([hybrid3, hybrid12], "AND", "CSC418"); // CSC418
makeNode([bool3], "OR", "CSC438"); // CSC438
makeNode([CSC384, bool3], "AND", "CSC486"); // CSC486
makeHybrid([CSC318, CSC418, CSC301, CSC384], "OR", "hybrid1");
makeNode([hybrid1], "AND", "CSC404"); // CSC404


// Edges
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
makeEdge(CSC148, CSC336, "p11");
makeEdge(CSC336, CSC446, "p12");
makeEdge(CSC336, CSC456, "p13");
makeEdge(CSC336, CSC436, "p14");
makeEdge(CSC384, CSC486, "p15");
makeEdge(CSC207, CSC401, "p16");
makeEdge(CSC209, CSC485, "p17");
makeEdge(CSC263, CSC411, "p18");
makeEdge(CSC209, CSC301, "p19");
makeEdge(CSC301, CSC302, "p20");
makeEdge(CSC263, CSC384, "p21");
makeEdge(CSC369, CSC469, "p22");
makeEdge(CSC209, CSC309, "p23");
makeEdge(CSC343, CSC309, "p24");
makeEdge(CSC343, CSC443, "p25");
makeEdge(CSC369, CSC443, "p26");
makeEdge(hybrid1, CSC404, "p27");
makeEdge(hybrid2, CSC488, "p28");
makeEdge(hybrid2, CSC358, "p29");
makeEdge(hybrid2, CSC458, "p30");
makeEdge(hybrid3, CSC418, "p31");
makeEdge(CSC236, bool1, "p32");
makeEdge(CSC207, bool1, "p33");
makeEdge(bool1, CSC263, "p34");
makeEdge(bool1, CSC410, "p35");
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
makeEdge(Sta1, CSC263, "p49");
makeEdge(Sta1, CSC384, "p50");
makeEdge(Sta1, CSC401, "p51");
makeEdge(Sta1, CSC485, "p52");
makeEdge(Sta2, CSC411, "p54");
makeEdge(Calc1, Sta1, "p55");
makeEdge(Sta2, CSC428, "p56");
makeEdge(Sta1, Sta2, "p57");
makeEdge(CSC411, CSC412, "p58");
makeEdge(hybrid10, CSC301, "p59");
makeEdge(hybrid11, ECE489, "p60");
makeEdge(bool2, CSC372, "p61");
makeEdge(hybrid12, CSC456, "p62");
makeEdge(hybrid12, CSC428, "p63");
makeEdge(hybrid12, CSC320, "p64");
makeEdge(hybrid12, CSC418, "p65");
makeEdge(hybrid7, bool4, "p66");
makeEdge(Sta1, bool4, "p67");
makeEdge(bool4, CSC321, "p68");
makeEdge(bool4, CSC310, "p69");
makeEdge(CSC148, CSC310, "p70");
makeEdge(hybrid14, CSC343, "p71");
makeEdge(CSC263, CSC420, "p72");
makeEdge(hybrid16, CSC343, "p73");
makeEdge(Lin1, bool5, "p74");
makeEdge(Calc1, bool5, "p75");
makeEdge(bool5, CSC336, "p76");
makeEdge(bool5, CSC320, "p77");
makeEdge(bool5, CSC420, "p78");
makeEdge(CSC258, CSC488, "p79");
makeEdge(CSC318, CSC428, "p80");
makeEdge(CSC263, CSC324, "p81");