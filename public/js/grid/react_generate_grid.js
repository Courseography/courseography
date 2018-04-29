
/** Holds the containers of the Fall and Spring timetables,
    and performs some pre-processing steps with a list of 'Lecture' objects*/
var Row = React.createClass({
  render: function(){
    var fallSession = {};
    var springSession = {};

    // Stores the 'colSpan' attribute value for each day-cell in the header
    var fallColSpans = {'M': 0, 'T': 0, 'W': 0, 'R': 0, 'F': 0};
    var springColSpans = {'M': 0, 'T': 0, 'W': 0, 'R': 0, 'F': 0};

    // From a list of Course objects, create a list of Lecture objects
    var courses = this.props.courses;
    var lectures = [];
    storeLectures(courses, lectures);

    // Organize the structure of the <fallSession> and <springSession> 2-D dictionaries
    initializeSessions(fallSession, springSession);
    // Store the list of Lecture objects into <fallSession> and <springSession>
    organizeLectures(lectures, springSession, fallSession);

    // For each session, set the 'width' attribute of each Lecture
    // (go to Lecture constructor for definition of 'width')
    setWidths(fallSession);
    setWidths(springSession);

    // Fill session colSpans dictionaries
    storeColSpans(fallSession, fallColSpans);
    storeColSpans(springSession, springColSpans);

    // Generate a container for each of the Fall and Spring timetables individually
    return(
      <div className="row">
        <TimetableContainer session={"F"} lectures={fallSession} headColSpans={fallColSpans} />
        <TimetableContainer session={"S"} lectures={springSession} headColSpans={springColSpans} />
      </div>
    );
  }
});

/** The container specifies formatting for all of the elements wrapped inside,
    (for example, every element inside a container will follow the same margin rules) */
var TimetableContainer = React.createClass({
    render: function(){
      return(
        <div className="col-md-6 col-xs-12 timetable-container">
          <Timetable session={this.props.session} lectures={this.props.lectures} headColSpans={this.props.headColSpans}/>
        </div>
      );
    }
});

/** A <table> element for the specified session*/
var Timetable = React.createClass({
  render: function(){
    return(
      <table className={"timetable table"} id={"timetable-" + this.props.session}>
        <TimetableHeader session = {this.props.session} lectures={this.props.lectures} headColSpans={this.props.headColSpans}/>
        <TimetableBody session = {this.props.session} lectures={this.props.lectures} headColSpans={this.props.headColSpans}/>
      </table>
    );
  }
});

/** Describes what the header of a table should look like, based off the session.
    The header contains five day cells, a dummy cell, and a term-name cell */
var TimetableHeader = React.createClass({
  render: function(){
      var days = ['M', 'T', 'W', 'R', 'F'];
      var dayStrings = ['Mon', 'Tue', 'Wed', 'Thu', 'Fri'];
      var tableHeader = null;
      var dayCell = null;
      var dayCells = [];
      var colSpans = this.props.headColSpans;

      // Iterate for each day
      for(var i = 0; i<5; i++){
        var dayKey = days[i];
        var dayString = dayStrings[i];
        // Create and store the React element for this day-cell
        dayCell = <th scope="col" colSpan={colSpans[dayKey] + ""}>{dayString}</th>
        dayCells.push(dayCell);
      }

      if(this.props.session == "F"){
        tableHeader = <thead>
                        <th className="timetable-dummy-cell"></th>
                        <th className="term-name">Fall</th>
                        {dayCells}
                      </thead>;
      }else if(this.props.session == "S"){
        tableHeader = <thead>
                        {dayCells}
                        <th className="term-name">Spring</th>
                        <th className="timetable-dummy-cell"></th>
                      </thead>;
      }
      return(
       tableHeader
      );
  }
});

/** Describes the body of the Timetable */
var TimetableBody = React.createClass({
  render: function(){
    var lectures = this.props.lectures;
    var TableRows = [];
    // For each row from 8 o'clock to 22 o'clock, there is an 'Hour' and 'Half hour' row
    for (var i = 8; i < 22; i++) {
      var TableRowHour = <TimetableRow session={this.props.session} time={i} currentLectures={lectures[i]} previousLectures={lectures[i-1]} headColSpans={this.props.headColSpans}/>
      var TableRowHalfHour = <TimetableRow session={this.props.session} time={i+0.5}/>
      TableRows.push(TableRowHour);
      TableRows.push(TableRowHalfHour);
    }

    // Render the list of TableRows generated above in the <table>
    return(
      <tbody>
        {TableRows}
      </tbody>
    );
  }
});

/** Describes what a row in the Timetable should look like, based off of the session, time,
    and previous cells generated.
*/
var TimetableRow = React.createClass({
  render: function(){
    var TableData = [];
    var days = ['M', 'T', 'W', 'R', 'F'];
    var dummyCell = <td className="timetable-dummy-cell"></td>;
    var currTime = this.props.time;
    var currSess = this.props.session;

    // If the time describes a row that starts at the hour
    // (for example, at 8:00 rather than 8:30)
    if (currTime % 1 === 0) {
      // Convert <time> to a 12-hour clock system
      var adjustedTime = (currTime === 12 ? 12 : currTime % 12) + ':00';
      var timeCell = <td className="timetable-time" rowSpan="2">{adjustedTime}</td>

      // The dictionary of lectures for this hour, and the hour before
      var currentLectures = this.props.currentLectures;
      var previousLectures = this.props.previousLectures;
      var headColSpans = this.props.headColSpans;

      days.forEach(function(day) {
        // Initialize attributes of the cell in this day-and-time slot
        var alreadyGenerated = false;
        var lectureCode = "";
        var type = "";
        var clicked = false;
        var className = "timetable-cell";
        var rowSpan = 2;
        var colSpan = 0;
        var inConflict = false;

        // Variables for calculations
        var dayCell = null;
        var headerColSpan = headColSpans[day];
        var totalColSpans = 0;
        var lecColSpan = 0;

        // Get the list of lectures at this time-day slot, as well as the list of lectures
        // occuring one hour prior this time-day slot
        var currentLectureList = currentLectures[day];
        var previousLectureList = previousLectures[day];

        if(currentLectureList.length != 0){
          // Render every lecture at this day-time slot (there can be more than one in a conflict case)
          currentLectureList.forEach(function(lecture) {
            // Check if this lecture has been previously rendered
            previousLectureList.forEach(function(lecturePrev) {
              if(lecturePrev.courseCode == lecture.courseCode){
                alreadyGenerated = true;
              }
            });

            // The 'colSpan' of the cells taken by this lecture, in this time-day slot
            // This should always be an integer value, since headerColSpan is the product of all
            // possible lecture widths in that day.
            lecColSpan = headerColSpan/lecture.width;

            if(!alreadyGenerated){
              rowSpan = 2*(lecture.endTime - lecture.startTime);
              lectureCode = lecture.courseCode;
              type = "L"
              clicked = true;
              className = "timetable-cell timetable-edge";
              inConflict = lecture.inConflict;
              colSpan = lecColSpan;
              dayCell = <td id={'' + day + currTime + '-0' + currSess}
                                data-in-conflict={inConflict}
                                data-satisfied={"true"}
                                rowSpan={rowSpan}
                                colSpan={colSpan}
                                className={className}
                                type={type}
                                clicked={clicked}
                                data-currentLectureList={currentLectureList}
                                >{lectureCode}
                            </td>;
              TableData.push(dayCell);
            }
            alreadyGenerated = false;

            // Record the total width taken up by this time-day slot so far,
            // from all the lectures generated
            totalColSpans += lecColSpan;

          });
          // In the case where there are not enough cells to fill the headerColSpan
          // of this time-day cell, generate remaining number of empty cells
          var remainingColSpan = headerColSpan-totalColSpans;
          for(var i = 0; i < remainingColSpan; i++){
            var dayCell = <td id={'' + days[i] + currTime + '-0' + currSess}
                            data-in-conflict="false"
                            data-satisfied={"true"}
                            rowSpan="2"
                            className="timetable-cell"></td>;
            TableData.push(dayCell);
          }

          totalColSpans = 0;

        // No courses to be generated at this day-time cell
        }else{
          // Generate the number of empty cells to fill the header colSpan
          for(var i = 0; i < headerColSpan; i++){
            var dayCell = <td id={'' + days[i] + currTime + '-0' + currSess}
                            data-in-conflict="false"
                            data-satisfied={"true"}
                            rowSpan="2"
                            className="timetable-cell"></td>;
            TableData.push(dayCell);
          }
        }
      });
      // Adjust the order at which these cells are rendered
      if(currSess == 'F'){
        TableData.unshift(timeCell);
        TableData.unshift(dummyCell);
      }else if(currSess == 'S'){
        TableData.push(timeCell);
        TableData.push(dummyCell);
      }

    // Otherwise, the time describe a row that starts at the half hour (for example, at 8:30)
    // For these such rows, generate five empty dayCells
    }else{
      for (var i = 0; i < 5; i++) {
        var dayCell = <td id={'' + days[i] + this.props.time.toString().replace('.', '-') + this.props.session + 'H'}
                          data-in-conflict="false"
                          data-satisfied={"true"}
                          rowSpan="1"
                          className="timetable-cell timetable-half-cell">
                      </td>;
        TableData.push(dayCell);
      }
      // Adjust the order at which these cells are rendered
      if(this.props.session == 'F'){
        TableData.unshift(dummyCell);
      }else if(this.props.session == 'S'){
        TableData.push(dummyCell);
      }
    }
    return(
      <tr>
        {TableData}
      </tr>
    );
  }
});

/** Renders everything wrapped inside the "Row" component */
export function drawTimetable() {
  // Sample Course objects, constructed with sample attributes

  // Note: W: [8, 12, 14, 15] denotes that this course runs on Wed from 8 to 12, and again on Wed from 14 - 15
  var lecture1 = createNewCourse("CSC100 (L)", "F", {'M': [8, 12],  'W': [8, 12, 14, 16], 'F': [8, 12]});
  var lecture2 = createNewCourse("CSC101 (L)", "F", {'M': [8, 11]});
  var lecture3 = createNewCourse("CSC102 (L)", "F", {'M': [10, 13]});
  var lecture4 = createNewCourse("CSC103 (L)", "F", {'M': [13, 14]});
  var lecture5 = createNewCourse("CSC104 (L)", "F", {'M': [13, 14]});
  var lecture6 = createNewCourse("CSC105 (L)", "F", {'M': [13, 14]});
  var lecture7 = createNewCourse("CSC106 (L)", "F", {'M': [13, 14]});
  var lecture8 = createNewCourse("CSC107 (L)", "F", {'M': [15, 19]});
  var lecture9 = createNewCourse("CSC108 (L)", "F", {'M': [16, 18]});
  var lecture10 = createNewCourse("CSC109 (L)", "F", {'M': [19, 20]});
  var lecture11 = createNewCourse("CSC110 (L)", "F", {'M': [19, 20]});
  var lecture12 = createNewCourse("CSC111 (L)", "F", {'M': [19, 20]});
  var lecture13 = createNewCourse("CSC112 (L)", "F", {'M': [19, 20]});
  var lecture14 = createNewCourse("CSC113 (L)", "F", {'M': [19, 21]});
  var lectureList = [lecture1, lecture2, lecture3, lecture4, lecture5, lecture6, lecture7, lecture8, lecture9, lecture10, lecture11, lecture12, lecture13, lecture14];

  // Render the React component inside the 'grid-body' HTML tag
  ReactDOM.render(
    <Row courses={lectureList}/>,
    document.getElementById('grid-body').getElementsByClassName('col-md-8 col-xs-12 col-md-pull-2')[0]
  );
}

/**
 * Constructor for a 'Course' object
 * @param {string} courseCode : Name of course
 * @param {string} session : Session of course
 * @param {dictionary} times : The days, and corresponding time-slot for which
                                this course is active
 * @return {dictionary} Represents a 'Course' object
*/
function createNewCourse(courseCode, session, times) {
  var lectures = {};
  var days = [];
  // Store the active days of this Course, and for each active
  // day, create and store a 'Lecture' object
  for (var day in times) {
    days.push(day);
    lectures[day] = [];
    // For the case where this lecture starts and ends more than once in one day
    for(var i = 0; i< times[day].length; i+=2){
      var startEndTimes = [times[day][i], times[day][i+1]];
      lectures[day].push(createNewLecture(courseCode, session, day, startEndTimes));
    }
  }

  var courseObject = {};
  courseObject.session = session;
  courseObject.days = days;
  courseObject.lectures = lectures;
  return courseObject;
}

/**
 * Constructor for a 'Lecture' object. Represents a single period of time
 * in which this course is active. (for ex; on Monday from 2-4)
 * @param {string} courseCode : Name of Lecture
 * @param {string} session : Session of Lecture
 * @param {string} day: Day of activity
 * @param {list} timePeriod : Active start and end time
 * @return {dictionary} Represents a 'Lecture' object
*/
function createNewLecture(courseCode, session, day, timePeriod) {
  var lectureObject = {};
  lectureObject.courseCode = courseCode;
  lectureObject.session = session;
  lectureObject.day = day;
  lectureObject.startTime = timePeriod[0];
  lectureObject.endTime = timePeriod[1];
  lectureObject.inConflict = false;
  // The width of a lecture is the maximum number of conflicts it has while it's active;
  //  if there are no conflicts, the width is 1.
  lectureObject.width = 1;
  return lectureObject;
}

/**
 * Helper function which stores a list of 'Lecture' objects,
 * from a list of 'Course' objects
*/
function storeLectures(courses, lectures){
  courses.forEach(function(course){
    course.days.forEach(function(day){
      course.lectures[day].forEach(function(lecture){
        lectures.push(lecture);
      });
    });
  });
  return lectures;
}

/**
 * Helper function to organize a list of 'Lectures' into <fallSession> and <springSession>
 * (look at the initializeSessions function to see how the
 * <fallSession> and <springSession> 2-D dictionaries are organized)
 * @param {list} lectures : List of 'Lecture' objects
 * @param {dictionary} fallSession : 2-D Dictionary storing LecturePeriods active in the fall
 * @param {dictionary} springSession : 2-D Dictionary storing LecturePeriods active in the spring
*/
function organizeLectures(lectures, springSession, fallSession){
  lectures.forEach(function(lecture) {
    if(lecture.session == 'F'){
        for (var i = lecture.startTime; i < lecture.endTime; i++){
          // Store this Lecture in it's active time-slot (denoted by <i>),
          // and in the list in it's active day slot (denoted by <lecture.day>), in <fallSession>.
          fallSession[i][lecture.day].push(lecture);
        }
    }else if(lecture.session == 'S'){
      // Same process as above for spring lectures in <springSession>
        for (var i = lecture.startTime; i < lecture.endTime; i++){
          springSession[i][lecture.day].push(lecture);
        }
    }
  });
}

/**
 * Helper function to initialize the <fallSession> and <springSession> 2-D dictionaries
 * @param {dictionary} fallSession : 2-D Dictionary storing lectures active in the fall
 * @param {dictionary} springSession : 2-D Dictionary storing lectures active in the spring
*/
function initializeSessions(fallSession, springSession){
  for(var i = 7; i<22; i++){
      // <i> denotes a time-slot, while <rowFall> and <rowSpring> denote dictionaries
      // with day-slots for that time slot
      var rowFall = {"M": [], "T": [], "W": [], "R": [], "F": []};
      var rowSpring = {"M": [], "T": [], "W": [], "R": [], "F": []};
      fallSession[i] = rowFall;
      springSession[i] = rowSpring;
  }
}

/**
 * Helper function which sets the width of each Lecture,
 * and also sets the inConflict attribute of all Lecture objects
 * @param {dictionary} session : 2-D Dictionary storing lectures active in the session
*/
function setWidths(session){
  var days = ['M', 'T', 'W', 'R', 'F'];
  // Iterate through every time-slot in the session
  for(var i = 8; i < 22; i++){
    var timeRow = session[i];
    days.forEach(function(day) {
      var timeDaySlot = timeRow[day];
      // If in this time-and-day slot there is a lecture conflict
      if(lectureConflict(timeDaySlot)){
        // Readjust (if needed) the 'width's of the lectures at this slot
        timeDaySlot.forEach(function(lecture) {
          if(lecture.width < timeDaySlot.length){
            lecture.width = timeDaySlot.length;
          }
          // Also specify that this lecture is in conflict
          lecture.inConflict = true;
        });
      }
    });
  }
}

/**
 * Helper function which stores the 'colSpan' attribute value for each day in the table header
 * @param {dictionary} colSpans : Dictionary storing the 'colSpan' attribute value for each day
*/
function storeColSpans(session, colSpans){
  var days = ['M', 'T', 'W', 'R', 'F'];
  // For each day, stores all possible 'width' values every lecture at that day could have
  var widths = {'M': [], 'T': [], 'W': [], 'R': [], 'F': []};
  storeWidths(session, widths);
  days.forEach(function(day) {
    // The 'colSpan' attribute of a header day cell is defined as the product of all
    // possible widths that could occur at that day
    colSpans[day] = calculateProduct(widths[day]);
  });
}

/**
 * Helper function which, for each day, stores all possible 'width' values the lectures in that day could have
 * @param {dictionary} widths : Dictionary storing all possible widths for each day
*/
function storeWidths(session, widths){
  var days = ['M', 'T', 'W', 'R', 'F'];
  // Iterate through every time-day slot in the session
  for(var i = 8; i < 22; i++){
    var timeRow = session[i];
    days.forEach(function(day) {
      var timeDaySlot = timeRow[day];
      // If there are lectures running at this time-day slot
      if(timeDaySlot.length > 0){
        // Retrieve a lecture at this time-day slot
        var lecture = timeDaySlot[0];
        // Store the width of this lecture, if it hasn't already been stored
        if(!isIn(widths[day], lecture.width)){
          widths[day].push(lecture.width);
        }
      }
    });
  }
}

/**
 * Helper function which returns true if an element is in an array, and false otherwise
*/
function isIn(array, element){
  for(var i = 0; i<array.length; i++){
    if(array[i] == element){
      return true;
    }
  }
  return false;
}

/**
 * Helper function which, from an array of integers, returns the product of all those integers
*/
function calculateProduct(array){
  var product = 1;
  array.forEach(function(conflictNum){
    product *= conflictNum;
  });
  return product;
}

/**
 * Helper function which returns if there is a course conflict at a time-and-day slot
 * (denoted by the number of courses in the list at this time-and-day slot)
*/
function lectureConflict(courseList){
  return courseList.length > 1;
}
