import { CoursePanel } from './course_panel.js.jsx';
import { SearchPanel } from './search_panel.js.jsx';
import { Row } from './calendar.js.jsx';

class Grid extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      selectedLectures: []
    };
  }

  componentDidMount() {
    this.setState({ selectedLectures: generateData() });
  }

  render() {
    return (
      <div>
        <CoursePanel />
        <Row courses={this.state.selectedLectures}/>
        <SearchPanel />
      </div>
    );
  }
}


ReactDOM.render(
  <Grid />,
  document.getElementById('grid-body')
);


/******************************************************************************
 * TODO: Remove the sample data generation below.
 *****************************************************************************/

/**
 * Generates mock data; for illustrative purposes only.
*/
export function generateData() {
  // Sample Course objects, constructed with sample attributes

  // Note: W: [8, 12, 14, 15] denotes that this course runs on Wed from 8 to 12, and again on Wed from 14 - 15
  let lecture1 = createNewCourse("CSC100 (L)", "F", {'M': [8, 12], 'W': [8, 12, 14, 16], 'F': [8, 12]});
  let lecture2 = createNewCourse("CSC101 (L)", "F", {'M': [8, 11]});
  let lecture3 = createNewCourse("CSC102 (L)", "F", {'M': [10, 13]});
  let lecture4 = createNewCourse("CSC103 (L)", "F", {'M': [13, 14]});
  let lecture5 = createNewCourse("CSC104 (L)", "F", {'M': [13, 14]});
  let lecture6 = createNewCourse("CSC105 (L)", "F", {'M': [13, 14]});
  let lecture7 = createNewCourse("CSC106 (L)", "F", {'M': [13, 14]});
  let lecture8 = createNewCourse("CSC107 (L)", "F", {'M': [15, 19]});
  let lecture9 = createNewCourse("CSC108 (L)", "F", {'M': [16, 18]});
  let lecture10 = createNewCourse("CSC109 (L)", "F", {'M': [19, 20]});
  let lecture11 = createNewCourse("CSC110 (L)", "F", {'M': [19, 20]});
  let lecture12 = createNewCourse("CSC111 (L)", "F", {'M': [19, 20]});
  let lecture13 = createNewCourse("CSC112 (L)", "F", {'M': [19, 20]});
  let lecture14 = createNewCourse("CSC113 (L)", "F", {'M': [19, 21]});
  let lectureList = [lecture1, lecture2, lecture3, lecture4, lecture5, lecture6, lecture7, lecture8, lecture9, lecture10, lecture11, lecture12, lecture13, lecture14];

  return lectureList;
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
  let lectures = {};
  let days = [];
  // Store the active days of this Course, and for each active
  // day, create and store a 'Lecture' object
  for (let day in times) {
    days.push(day);
    lectures[day] = [];
    // For the case where this lecture starts and ends more than once in one day
    for(let i = 0; i< times[day].length; i+=2){
      let startEndTimes = [times[day][i], times[day][i+1]];
      lectures[day].push(createNewLecture(courseCode, session, day, startEndTimes));
    }
  }

  let courseObject = {};
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
  let lectureObject = {};
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