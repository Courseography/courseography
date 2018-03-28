//import {createNewLecture, organizeLectures, testerfunct} from 'es6!grid/lecture_objects';

/** Holds the containers of the Fall and Spring timetables */
var Row = React.createClass({
  render: function(){
    let organizedFallLectures = {};
    let organizedSpringLectures = {};
    initializeOrganizedLectures(organizedFallLectures, organizedSpringLectures);
    organizeLectures(this.props.lectures, organizedSpringLectures, organizedFallLectures);

    return(
      <div className="row">
        <TimetableContainer session={"F"} lectures={organizedFallLectures} />
        <TimetableContainer session={"S"} lectures={organizedSpringLectures} />
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
        <Timetable session={this.props.session} lectures={this.props.lectures}/>
      </div>
      );
    }
});

/** A <table> element for the specified session*/
var Timetable = React.createClass({
  render: function(){
    return(
      <table className={"timetable table"} id={"timetable-" + this.props.session}>
        <TimetableHeader session = {this.props.session}/>
        <TimetableBody session = {this.props.session} lectures={this.props.lectures}/>
      </table>
    );
  }
});

/** Describes what the header of a table should look like, based off the session. */
var TimetableHeader = React.createClass({
  render: function(){
      let tableHeader = null;
      let dayCells = [<th>Mon</th>, <th>Tue</th>, <th>Wed</th>, <th>Thu</th>, <th>Fri</th>];
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

/** Fills the body of the table. For each hour from 8 o'clock to 22 o'clock,
    there will be two rows displaying information for the half hours */
var TimetableBody = React.createClass({
  render: function(){
    let lectures = this.props.lectures;
    let TableRows = [];
    for (var i = 8; i < 22; i++) {
      let TableRowHour = <TimetableRow session={this.props.session} time={i} currentLectures={lectures[i]} previousLectures={lectures[i-1]}/>
      let TableRowHalfHour = <TimetableRow session={this.props.session} time={i+0.5}/>
      TableRows.push(TableRowHour);
      TableRows.push(TableRowHalfHour);
    }
    return(
      <tbody>
        {TableRows}
      </tbody>
    );
}});

/** Describes what a row in the Timetable should look like,
    based off of the session and time.
    Each row has seven cells -- <dayCell> (used for weekday information),
    <timeCell> (displays time of the row),
    and <dummyCell> (used for formatting).
    Notice that the ordering in which the 'cell' elements are rendered
    is important, as it allows the generated HTML to comply with the CSS.*/
var TimetableRow = React.createClass({

  render: function(){

    let TableData = [];
    let weekPrefixArray = ['M', 'T', 'W', 'R', 'F'];
    let dummyCell = <td className="timetable-dummy-cell"></td>;

    // If the time describes a row that starts at the hour (for example, at 8:00),
    // then place the lecture slot in this row.
    if (this.props.time % 1 === 0) {
      // The adjusted time given by the time variable index
      var adjustedTime = (this.props.time === 12 ? 12 : this.props.time % 12) + ':00';
      let timeCell = <td className="timetable-time" rowSpan="2">{adjustedTime}</td>

      // The 2-D Dictionary of lectures for this time, and the hour before
      let currentLectures = this.props.currentLectures;
      let previousLectures = this.props.previousLectures;

      // Get the lecture that occurs at this time for each day in the row
      for (var i = 0; i < 5; i++) {
        var generateEmpty = false;
        var day = weekPrefixArray[i];
        var lectureCode = "";
        var typeL = "";
        var clickedL = false;
        var classNameL = "timetable-cell";
        var rowSpanL = 2;
        var inConflictL = false;

        // Get the list of lectures occuring at this day,
        // both at this time and at the hour before.
        var currentLectureList = currentLectures[day];
        var previousLectureList = previousLectures[day];

        if(currentLectureList.length != 0){
          if(currentLectureList.length > 1){
            // CONFLICTION CASE
            inConflictL = true;
              // var trueStart = currentLecture.times[day][0];
              // for(var i = currentLecture.times[day][0]; i < currentLecture.times[day][1]; i++){
              //   if(i >= previousLecture.times[day][1]){
              //     console.log(i + " " + currentLecture.courseCode);
              //     trueStart = i;
              //     break;
              //   }
              // }
              // rowSpanL = 2*(currentLecture.times[day][1] - i);
              // lectureCode = currentLecture.courseCode;
              // typeL = "L"
              // clickedL = true;
              // classNameL = "timetable-cell timetable-edge";
          }

          // To do the merging of blocks, we have to adjust the rowspan.
          // If we are printing an extended course, then change the rowSpan
          // of this to 0. Otherwise, adjust the rowSpan accordingly.
          var currentLecture = currentLectureList[0];
          var previousLecture = previousLectureList[0];
          if(previousLecture == currentLecture){
            generateEmpty = true;
          }else{
            rowSpanL = 2*(currentLecture.times[day][1] - currentLecture.times[day][0]);
            lectureCode = currentLecture.courseCode;
            typeL = "L"
            clickedL = true;
            classNameL = "timetable-cell timetable-edge";
          }
        }

        let dayCell = null;
        if(!generateEmpty){
        dayCell = <td id={'' + day + this.props.time + '-0' + this.props.session}
                          data-in-conflict={inConflictL}
                          data-satisfied={"true"}
                          rowSpan={rowSpanL}
                          className={classNameL}
                          type={typeL}
                          clicked={clickedL}
                          data-currentLectureList={currentLectureList}
                          inConflictL={inConflictL}
                          >{lectureCode}
                      </td>;
                    }
        TableData.push(dayCell);
      }
      if(this.props.session == 'F'){
        TableData.unshift(timeCell);
        TableData.unshift(dummyCell);
      }else if(this.props.session == 'S'){
        TableData.push(timeCell);
        TableData.push(dummyCell);
      }

    // Otherwise, the will time describe a row that starts at the half hour (for example, at 8:30)
    }else{
      for (var i = 0; i < 5; i++) {
        let dayCell = <td id={'' + weekPrefixArray[i] + this.props.time.toString().replace('.', '-') + this.props.session + 'H'}
                          data-in-conflict="false"
                          data-satisfied={"true"}
                          rowSpan="1"
                          className="timetable-cell timetable-half-cell">
                      </td>;
        TableData.push(dayCell);
      }
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

/** Renders everything wrapped inside the "Row" element */
export function drawTimetable() {

  // Sample lectures, at sample times
  var lecture1 = createNewLecture("CSC108 (L)", "F", {'M': [8, 12],  'W': [8, 12], 'F': [8, 12]});
  var lecture2 = createNewLecture("CSC236 (L)", "F", {'M': [8, 11]});
  var lecture3 = createNewLecture("CSC258 (L)", "S", {'M': [15, 18], 'T':[16, 18], 'R':[17, 18]});
  var lecture4 = createNewLecture("CSC404 (L)", "S", {'T': [15, 16], 'R': [15, 16]});
  var lecture5 = createNewLecture("CSC343 (L)", "S", {'T': [9, 10], 'R': [9, 10]});
  var lecture6 = createNewLecture("CSC369 (L)", "S", {'T': [11, 12], 'R': [11, 12]});
  let lectureList = [lecture2, lecture1, lecture3, lecture4, lecture5, lecture6];

  ReactDOM.render(
    <Row lectures={lectureList}/>,
    document.getElementById('grid-body').getElementsByClassName('col-md-8 col-xs-12 col-md-pull-2')[0]
  );
}

function createNewLecture(courseCode, session, times) {
  /* Representation invariants:
     timeStart and timeEnd must be integer values (cannot be 8.5, for ex) */

   // Gets the keys (i.e, the running days) of a lecture
   var days = [];
   for (var key in times) {
       if (times.hasOwnProperty(key)) {
           days.push(key);
       }
   }

  var obj = {};
  obj.courseCode = courseCode;
  obj.session = session;
  obj.times = times;
  obj.days = days;
  return obj;
}

function organizeLectures(lectures, springDict, fallDict){
  var lectureStartTime;
  var lectureEndTime;

  // For each lecture in the list of lectures,
  lectures.forEach(function(lecture) {
    if(lecture.session == 'F'){
      // For each day that this lecture runs
      lecture.days.forEach(function(day) {
        lectureStartTime = lecture.times[day][0];
        lectureEndTime = lecture.times[day][1];
        for (var i = lectureStartTime; i < lectureEndTime; i++)
          // Push this lecture at this time & day in the dictionary
          fallDict[i][day].push(lecture);
        });

    }else if(lecture.session == 'S'){
      lecture.days.forEach(function(day) {
        lectureStartTime = lecture.times[day][0];
        lectureEndTime = lecture.times[day][1];
        for (var i = lectureStartTime; i < lectureEndTime; i++)
        springDict[i][day].push(lecture);
      });
    }
  });
}

function initializeOrganizedLectures(fallDict, springDict){
  for(var i = 7; i<22; i++){
      var rowDictFall = {"M": [], "T": [], "W": [], "R": [], "F": []};
      var rowDictSpring = {"M": [], "T": [], "W": [], "R": [], "F": []};
      fallDict[i] = rowDictFall;
      springDict[i] = rowDictSpring;
  }
}
