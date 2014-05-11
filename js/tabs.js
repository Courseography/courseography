/**
 * Javscript functions for all the tabs except 'Check My POSt!'
 */

//////////////////////////////////////////
// My Courses Tab
//////////////////////////////////////////

// Globals
var clickedCourses = [];
var FCEs = 0;
var FCEs100 = 0;
var FCEs200 = 0;
var FCEs300 = 0;
var FCEs400 = 0;
var FCEsMAT = 0;

// Updates the "My Courses" tab
// Note: not called on hybrids
function updateClickedCourses(name, active) {
	var i = clickedCourses.indexOf(name);
	var diff = (name === 'CSC200' || name === 'Calc1') ? 1 : 0.5; // Full-year
	if (active && i == -1) {
		clickedCourses.push(name);
	} else if (!active && i > -1) {
		diff *= -1;
		clickedCourses.splice(i, 1);
	}

	if (math.indexOf(name) > -1) {
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

// Update the total FCE count, and display total
function updateFCECount() {
	FCEs = FCEs100 + FCEs200 + FCEs300 + FCEs400 + FCEsMAT;
	$('#FCEcount').html(FCEs.toFixed(1));
}

// Generate table of clicked courses
function updateMyCoursesTab() {
	$('#courseGrid').empty();

	// Get data from course calendar        
	var htmlClickedString = $.map(clickedCourses, function(course) {
		var title = '';
		if (course === 'Calc1') {
			title = 'First-year calculus: MAT135-136, MAT137, or MAT157';
		} else if (course === 'Lin1') {
			title = 'One term in linear algebra: MAT221, MAT223, or MAT240';
		} else if (course === 'Sta1') {
			title = 'One term in probability theory: STA247, STA255, or STA257';
		} else if (course === 'Sta2') {
			title = 'One term in statistics: STA248 or STA261';
		} else {
			$.ajax({
				url: 'res/courses/' + course + 'H1.txt',
				dataType: 'json',
				async: false,
				success: function(data) {
					result = data.title;
				}
			});
			title = result;
		}
		return "<td class='courseCell' style='background: " + $("#" + course + "> rect").css('fill') + "'><div id='" + course + "cell'><p class='courseName'>" + course + "</p><p class=" + course + "text>" + title + "</p></div></td>";
	}).join('');

	$('#courseGrid').html(htmlClickedString);
}


//////////////////////////////////////////
// Course Descriptions Tab
//////////////////////////////////////////

// Read course description from resource files
function fetchCourseDescription(id) {
	var result;
	if (id === 'CSC200') {
		result = readCalendarEntry(id + 'Y1');
	} else if (id === 'Calc1') {
		result = readCalendarEntry('MAT135H1') + readCalendarEntry('MAT136H1') + readCalendarEntry('MAT137Y1') + readCalendarEntry('MAT157Y1');
	} else if (id === 'Lin1') {
		result = readCalendarEntry('MAT221H1') + readCalendarEntry('MAT223H1') + readCalendarEntry('MAT240H1');
	} else if (id === 'Sta1') {
		result = readCalendarEntry('STA247H1') + readCalendarEntry('STA255H1');
	} else if (id === 'Sta2') {
		result = readCalendarEntry('STA248H1') + readCalendarEntry('STA261H1');
	} else {
		result = readCalendarEntry(id + 'H1');
	}

	$('#calendar').html(result);
};

function readCalendarEntry(name) {
	var result = '';
	$.ajax({
		url: 'res/courses/' + name + '.txt',
		dataType: 'json',
		async: false,
		success: function(data) {
			result += '<h3>' + data.code + ': ' + data.title + '</h3>';
			result += '<p>' + data.description + '</p>';
			if (typeof data.prereqString !== 'undefined') {
				result += '<p><strong>Prerequisite:</strong> ' + data.prereqString + '</p>';
			}
			if (typeof data.prep !== 'undefined') {
				result += '<p><strong>Recommended Preparation:</strong> ' + data.prep + '</p>';
			}
			if (typeof data.exclusions != 'undefined') {
				result += '<p><strong>Exclusions:</strong> ' + data.exclusions + '</p>';
			}

			result += '<p><strong>Distribution Requirement Status:</strong> ' + data.distribution + '</p>';
			result += '<p><strong>Breadth Requirement:</strong> ' + data.breadth + '</p>';
		}
	});

	return result;
};



//////////////////////////////////////////
// Focus Tab
//////////////////////////////////////////

// Globals
var activeFocus = '';


// Activate a focus
function updateActiveFocus(id) {
	$('ellipse.spotlight').remove();
	// Remove old icon
	$(".focusList .statusIcon").remove();

	if (id === '') {
		clearFocus();
	} else {
		var focus = window[id + 'FocusList'];
		$('body').css('background', 'rgb(40,40,40)');
		$('.node, .hybrid').attr('data-active', 'unlit');
		$.each(focus, function(index, elem) {
			spotlight(elem);
		});
		$("#graph").html($("#graph").html()); // Hack to make spotlights appear
		setMouseCallbacks();
		activeFocus = id;

		// Put in new icon.
		$(".focusList a[href='#" + id + "Details']").append(
			"<img class='statusIcon' src='res/ico/close.ico' alt='Click to close!'/>");
	}
}

// Removes spotlight on active focus
function clearFocus() {
	$('body').css('background', 'white');
	activeFocus = '';
	$.each(nodes, function(index, elem) {
		window[elem].updateSVG();
	});
}

// Put a spotlight on a node
function spotlight(id) {
	var node = $('#' + id + ' > rect');
	var width = parseFloat(node.attr('width')) / 2;
	var height = parseFloat(node.attr('height')) / 2;
	var x = parseFloat(node.attr('x')) + width;
	var y = parseFloat(node.attr('y')) + height;

	var ellipse = '<ellipse class="spotlight" cx="'.concat(x, '" cy = "', y, '" rx="', width + 9, '" ry="', height + 8.5, '"/>');
	$('#' + id).before(ellipse);
	$('#' + id).attr('data-active', 'lit');

	window[id].updateSVG();
}



//////////////////////////////////////////
// Timetable Tab
//////////////////////////////////////////

// Globals
// 2013-2014 timetable
var timetable = 'res/timetableHTML.html';
// 2014-2015 timetable
//var timetable = 'res/timetableHTML2014.html';


// Fetch timetable information
function createTimetable() {
	$.ajax(timetable)
		.done(function(response) {
			$('#timetableContainer').html(response);
		});
}

// Search function for timetable
function createTimetableSearch() {
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
}



//////////////////////////////////////////
// Feedback Tab
//////////////////////////////////////////

// A form for feedback; currently disabled
function activateFeedbackForm() {
	$("#submit_btn").click(function() {
		var user_name = $('input[name=name]').val();
		var user_message = $('textarea[name=message]').val();
		var proceed = true;

		if (proceed) {
			post_data = {
				'userName': user_name,
				'userMessage': user_message
			};
			$.post('email.php', post_data, function(data) {
				$("#result").hide()
					.html('<div class="success">' + data + '</div>').slideDown();
				$('#contact_form input').val('');
				$('#contact_form textarea').val('');

			}).fail(function(err) {
				$("#result").hide()
					.html('<div class="error">' + err.statusText + '</div>').slideDown();
			});
		}

	});

	$("#contact_form input, #contact_form textarea").keyup(function() {
		$("#contact_form input, #contact_form textarea").css('border-color', '');
		$("#result").slideUp();
	});
}



// Create tabs
function createTabs() {
	$('.infoTabs').tabs({
		activate: function(e, ui) {
			e.currentTarget.blur();
		}
	});
	$('.focusTabs').tabs({
		active: false,
		collapsible: true,
		activate: function(e, ui) {
			var name = ui.newTab.attr('aria-controls');
			if (name) {
				name = name.substr(0, name.length - 7);
				updateActiveFocus(name);	
			} else {
				updateActiveFocus('');
			}
		},

	});
	$('.postTypeTabs, .postTabs').tabs({
		active: 0,
		activate: function(e, ui) {
			e.currentTarget.blur();
		}
	});

	createTimetable();
	createTimetableSearch();
	setMouseCallbacks();
}