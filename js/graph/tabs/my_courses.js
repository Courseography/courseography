// Updates the "My Courses" tab
// Note: not called on hybrids
function updateClickedCourses(name, active) {
    'use strict';

    var i = clickedCourses.indexOf(name);
    var diff = (name === 'CSC200' || name === 'Calc1') ? 1 : 0.5; // Full-year
    if (active && i === -1) {
        clickedCourses.push(name);
    } else if (!active && i > -1) {
        diff *= -1;
        clickedCourses.splice(i, 1);
    }

    if (math.indexOf(name) > -1) {
        FCEsMAT += diff;
    } else if (name.charAt(3) === '1') {
        FCEs100 += diff;
    } else if (name.charAt(3) === '2') {
        FCEs200 += diff;
    } else if (name.charAt(3) === '3') {
        FCEs300 += diff;
    } else if (name.charAt(3) === '4') {
        FCEs400 += diff;
    }
}


// Generate table of clicked courses
function updateMyCoursesTab() {
    'use strict';

    var courseGridObject = $('#courseGrid');
    courseGridObject.empty();

    // Get data from course calendar
    var htmlClickedString = $.map(clickedCourses, function (course) {
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
                success: function (data) {
                    result = data.title;
                }
            });
            title = result;
        }
        return '<td class="courseCell" style="background: ' +
               $('#' + course + '> rect').css('fill') +
               '"><div id="' + course + 'cell"><p class="courseName">' +
               course +
               '</p><p class=' +
               course + 'text>' +
               title +
               '</p></div></td>';
    }).join('');

    courseGridObject.html(htmlClickedString);
}