
/** Holds the containers of the Fall and Spring timetables,
    and do some pre-processing steps with the list of 'Lecture' objects*/
var Row = React.createClass({
  render: function(){
    var fallSession = {};
    var springSession = {};
    var fallWidths = {'M': [], 'T': [], 'W': [], 'R': [], 'F': []};
    var springWidths = {'M': [], 'T': [], 'W': [], 'R': [], 'F': []};

    // Organize the structure of the <fallSession> and <springSession> 2-D dictionaries
    initializeSessions(fallSession, springSession);
    // Store the list of 'Lecture' objects into <fallSession> and <springSession>
    organizeLectures(this.props.lectures, springSession, fallSession);
    // For each session, set the 'width' attribute of each lecture
    // (the width of a lecture is the maximum number of conflicts it has while it's active)
    setWidths(fallSession);
    setWidths(springSession);
    // For each day, store the widths of each lecture active at that day
    storeWidths(fallSession, fallWidths);
    storeWidths(springSession, springWidths);

    // Generate a container for each of the Fall and Spring timetables individually
    return(
      <div className="row">
        <TimetableContainer session={"F"} lectures={fallSession} widths={fallWidths} />
        <TimetableContainer session={"S"} lectures={springSession} widths={springWidths} />
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
          <Timetable session={this.props.session} lectures={this.props.lectures} widths={this.props.widths}/>
        </div>
      );
    }
});

/** A <table> element for the specified session*/
var Timetable = React.createClass({
  render: function(){
    return(
      <table className={"timetable table"} id={"timetable-" + this.props.session}>
        <TimetableHeader session = {this.props.session} lectures={this.props.lectures} widths={this.props.widths}/>
        <TimetableBody session = {this.props.session} lectures={this.props.lectures} widths={this.props.widths}/>
      </table>
    );
  }
});

/** Describes what the header of a table should look like, based off the session.
    The header contains five day cells, a dummy cell and a term-name cell*/
var TimetableHeader = React.createClass({
  render: function(){
      var tableHeader = null;
      var dayCell = null;
      var dayCells = [];
      var conflicts = this.props.conflicts;
      var widths = this.props.widths;

      // Variables used to determine the colSpan of each day-cell
      var widthProd = 1;
      var days = ['M', 'T', 'W', 'R', 'F'];
      var dayStrings = ['Mon', 'Tue', 'Wed', 'Thu', 'Fri'];

      // Iterate for each day
      for(var i = 0; i<5; i++){
        var dayKey = days[i];
        var dayString = dayStrings[i];
        // Calculate the product of all the widths occuring on this day
        widthProd = calculateProduct(widths[dayKey]);
        // Create and store the React element for this day-cell
        dayCell = <th scope="col" colSpan={widthProd + ""}>{dayString}</th>
        dayCells.push(dayCell);
        // Reset widthProd for the next day-cell
        widthProd = 1;
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

/** Fills the body of the <table> element with lower React components.*/
var TimetableBody = React.createClass({
  render: function(){
    var lectures = this.props.lectures;
    var TableRows = [];
    // For each row from 8 o'clock to 22 o'clock, there is an 'Hour' and 'Half hour' row
    for (var i = 8; i < 22; i++) {
      var TableRowHour = <TimetableRow session={this.props.session} time={i} currentLectures={lectures[i]} previousLectures={lectures[i-1]} conflicts={this.props.conflicts}/>
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
    ***Notice that the ordering in which the 'cell' elements are rendered
    is important, as it allows the generated HTML to comply with the CSS.***
*/
var TimetableRow = React.createClass({
  render: function(){
    var TableData = [];
    var days = ['M', 'T', 'W', 'R', 'F'];
    var dummyCell = <td className="timetable-dummy-cell"></td>;
    var currTime = this.props.time;
    var currSess = this.props.session;

    // If the time describes a row that starts at the hour
    // (for example, at 8:00 rather than 8:30), then place the lecture slot in this row.
    if (currTime % 1 === 0) {
      // convert <time> to a 12-hour clock system
      var adjustedTime = (currTime === 12 ? 12 : currTime % 12) + ':00';
      var timeCell = <td className="timetable-time" rowSpan="2">{adjustedTime}</td>

      // The dictionary of lectures for this hour, and the hour before
      var currentLectures = this.props.currentLectures;
      var previousLectures = this.props.previousLectures;

      // Get the lecture that occurs at this time, for each day in the row
      days.forEach(function(day) {
        // Initialize attributes of the cell in this day-and-time slot
        var alreadyGenerated = false;
        var lectureCode = "";
        var type = "";
        var clicked = false;
        var className = "timetable-cell";
        var rowSpan = 2;
        var inConflict = false;
        var dayCell = null;

        // Store the number of cells (denoted by the number of conflicts) that occur at this day
        var numCells = conflicts[day];
        if(numCells==0){
          numCells = 1;
        }

        // Get the list of lectures, as well as the list of lectures occuring
        // one hour prior at this day
        var currentLectureList = currentLectures[day];
        var previousLectureList = previousLectures[day];

        if(currentLectureList.length != 0){
          // Check the conflict case; i.e, the course in this day-and-time slot overlaps
          // with another course. The <inConflict> attribute is true for all such courses.
          if(currentLectureList[0].inConflict){
            // Render every course at this day-time slot (there can be more than one in the conflict case)
            currentLectureList.forEach(function(lecture) {
              // Check if this course has been previously rendered
              // (there can also be more than one previously rendered course in the conflict case)
              previousLectureList.forEach(function(lecturePrev) {
                if(lecturePrev.courseCode == lecture.courseCode){
                  alreadyGenerated = true;
                }
              });

              if(!alreadyGenerated){
                lectureStartTime = lecture.times[day][0];
                lectureEndTime = lecture.times[day][1]
                rowSpan = 2*(lectureEndTime - lectureStartTime);
                lectureCode = lecture.courseCode;
                type = "L"
                clicked = true;
                className = "timetable-cell timetable-edge";
                inConflict = true;
                dayCell = <td id={'' + day + currTime + '-0' + currSess}
                                  data-in-conflict={inConflict}
                                  data-satisfied={"true"}
                                  rowSpan={rowSpan}
                                  className={className}
                                  type={type}
                                  clicked={clicked}
                                  data-currentLectureList={currentLectureList}
                                  >{lectureCode}
                              </td>;
                TableData.push(dayCell);
              }
            });
          // Non-conflict case
          }else{
            var currentLecture = currentLectureList[0];
            var previousLecture = previousLectureList[0];
            alreadyGenerated = previousLecture == currentLecture;

            if(!alreadyGenerated){
              var lectureStartTime = currentLecture.times[day][0];
              var lectureEndTime = currentLecture.times[day][1]
              rowSpan = 2*(lectureEndTime - lectureStartTime);
              lectureCode = currentLecture.courseCode;
              type = "L"
              clicked = true;
              className = "timetable-cell timetable-edge";
              dayCell = <td id={'' + day + currTime + '-0' + currSess}
                                data-in-conflict={inConflict}
                                data-satisfied={"true"}
                                rowSpan={rowSpan}
                                colSpan={numCells}
                                className={className}
                                type={type}
                                clicked={clicked}
                                data-currentLectureList={currentLectureList}
                                >{lectureCode}
                            </td>;
                            TableData.push(dayCell);
            }
          }

        // No courses to be generated at this day-time cell
        }else{
          // Generate the number of empty cells, as there are conflicts in this day
            for(var l = 0; l < numCells; l++){
              var dayCell = <td id={'' + days[i] + currTime + '-0' + currSess}
                              data-in-conflict="false"
                              data-satisfied={"true"}
                              rowSpan="2"
                              className="timetable-cell"></td>;
              TableData.push(dayCell);
            }
        }
      });
      // Reorder the positions of the timeCell and dummyCell to comply with the CSS
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
      // Adjust the order at which these cells are generated
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
  // Sample course objects, constructed with sample attributes
  var lecture1 = createNewLecture("CSC108 (L)", "F", {'M': [8, 12],  'W': [8, 12], 'F': [8, 12]});
  var lecture2 = createNewLecture("CSC236 (L)", "F", {'M': [8, 11]});
  var lecture3 = createNewLecture("CSC209 (L)", "F", {'M': [8, 10]});
  var lecture4 = createNewLecture("MAT237 (L)", "F", {'M': [13, 14]});
  var lecture5 = createNewLecture("CSC209 (L)", "F", {'M': [15, 18]});
  var lecture6 = createNewLecture("STA257 (L)", "F", {'M': [15, 18]});
  var lecture7 = createNewLecture("CSC258 (L)", "S", {'M': [15, 18], 'T':[16, 18], 'R':[17, 18]});
  var lecture8 = createNewLecture("CSC404 (L)", "S", {'T': [15, 16], 'R': [15, 16]});
  var lecture9 = createNewLecture("CSC343 (L)", "S", {'T': [9, 10], 'R': [9, 10]});
  var lecture10 = createNewLecture("CSC369 (L)", "S", {'T': [11, 12], 'R': [11, 12]});
  var lectureList = [lecture1, lecture2, lecture3, lecture4, lecture5, lecture6, lecture7, lecture8, lecture9, lecture10];

  // Render the React component inside the 'grid-body' HTML tag
  ReactDOM.render(
    <Row lectures={lectureList}/>,
    document.getElementById('grid-body').getElementsByClassName('col-md-8 col-xs-12 col-md-pull-2')[0]
  );
}


/**
 * Constructor for a 'Lecture' object, created in drawTimetable()
 * @param {string} courseCode : Name of course
 * @param {string} session : Session of course
 * @param {dictionary} times : The days, and corresponding time-slot for which
                                this course is active
 * @return {dictionary} Represents a 'Lecture' object
*/
function createNewLecture(courseCode, session, times) {
  // Store the active days of this lecture,
  // and intialize the <widths> and <conflicts> dictionary of this lecture
  var days = [];
  var widths = {};
  var conflicts = {};
  for (var key in times) {
    days.push(key);
    widths.key = 1;
    conflicts.key = false;
  }

  var lectureObject = {};
  lectureObject.courseCode = courseCode;
  lectureObject.session = session;
  lectureObject.times = times;
  lectureObject.days = days;
  lectureObject.widths = widths;
  lectureObject.conflicts = conflicts;
  return lectureObject;
}

/**
 * Helper function to organize a list of 'Lecture' objects in <fallSession> and <springSession>
 * (look at initializeSessions function to see how the
 * <fallSession> and <springSession> 2-D dictionaries are organized)
 * @param {list} lectures : List of 'Lecture' objects
 * @param {dictionary} fallSession : 2-D Dictionary storing courses active in the fall
 * @param {dictionary} springSession : 2-D Dictionary storing courses active in the spring
*/
function organizeLectures(lectures, springSession, fallSession){
  var lectureStartTime;
  var lectureEndTime;

  // For each lecture in the list of lectures,
  lectures.forEach(function(lecture) {
    if(lecture.session == 'F'){
      // For each day that this fall lecture is active
      lecture.days.forEach(function(day) {
        lectureStartTime = lecture.times[day][0];
        lectureEndTime = lecture.times[day][1];
        for (var i = lectureStartTime; i < lectureEndTime; i++){
          // Store this lecture in it's active time-slot (denoted by <i>),
          // and in the list in it's active day slot (denoted by <day>), in <fallSession>.
          fallSession[i][day].push(lecture);
        }
      });

    }else if(lecture.session == 'S'){
      // Same process as above for spring lectures in <springSession>
      lecture.days.forEach(function(day) {
        lectureStartTime = lecture.times[day][0];
        lectureEndTime = lecture.times[day][1];
        for (var i = lectureStartTime; i < lectureEndTime; i++)
        springSession[i][day].push(lecture);
      });
    }
  });
}


/**
 * Helper function to initialize the <fallSession> and <springSession> 2-D dictionaries
 * @param {dictionary} fallSession : 2-D Dictionary storing courses active in the fall
 * @param {dictionary} springSession : 2-D Dictionary storing courses active in the spring
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
 * Helper function which sets the width of each lecture,
 * and also to set the inConflict attribute of all lecture objects
 * @param {dictionary} session : 2-D Dictionary storing courses active in the session
*/
function setWidths(session){
  var days = ['M', 'T', 'W', 'R', 'F'];
  // Iterate through every time-slot in the session
  for(var i = 8; i < 22; i++){
    var currRow = session[i];
    days.forEach(function(day) {
      // If in this time-and-day slot there is a lecture conflict
      if(lectureConflict(currRow[day])){
        // Readjust (if needed) the 'width's of the lectures at this slot
        currRow[day].forEach(function(lecture) {
          if(lecture.width[day] < currRow.length){
            lecture.width[day] = currRow.length;
          }
        });
        // Also specify that this lecture is in conflict
        lecture.inConflict = true;
      }
    });
  }
}

/**
 * Helper function which stores the widths of each lecture for each day
 * @param {dictionary} widths : Dictionary storing the widths of each lecture for each day
*/
function storeWidths(session, widths){
  var days = ['M', 'T', 'W', 'R', 'F'];
  // Iterate through every time-slot in the session
  for(var i = 8; i < 22; i++){
    var currRow = session[i];
    days.forEach(function(day) {
      // Retrieve a lecture at this time-day slot
      currentLecture = currRow[day][0];
      // Store the width of this lecture, if it hasn't already been stored
      if(isIn(widths[day], currentLecture.width)){
        widths[day].push(currentLecture.width);
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
