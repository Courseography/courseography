import React, { useCallback } from "react"
import { MapModal } from "../common/react_modal.js.jsx"

/*
 * Holds the containers of the Fall and Spring timetables,
 * and performs some pre-processing steps with a list of 'Lecture' objects
 */
export const Row = props => {
  // Create a list of lecture objects
  let lectures = props.lectureSections.map((lectureSection, index, lectureSections) =>
    createNewLectures(lectureSection, index, lectureSections)
  )
  lectures = [].concat.apply([], lectures)

  const fallLectures = lectures.filter(lecture => {
    return lecture.session === "F" || lecture.session === "Y"
  })

  const springLectures = lectures.filter(lecture => {
    return lecture.session === "S" || lecture.session === "Y"
  })

  // Organize the structure of the <fallSession> and <springSession> 2-D dictionaries
  let fallSession, springSession
  ;[fallSession, springSession] = initializeSessions(lectures)

  // For each session, set the 'width' attribute of each Lecture
  // (go to Lecture constructor for definition of 'width')
  setWidths(fallSession)
  setWidths(springSession)

  // Fill session colSpans dictionaries
  const fallColSpans = { M: 0, T: 0, W: 0, R: 0, F: 0 }
  const springColSpans = { M: 0, T: 0, W: 0, R: 0, F: 0 }
  storeColSpans(fallSession, fallColSpans)
  storeColSpans(springSession, springColSpans)

  // Generate a container for each of the Fall and Spring timetables individually
  return (
    <>
      <TimetableContainer
        session="F"
        lecturesByTime={fallSession}
        headColSpans={fallColSpans}
        lectures={fallLectures}
      />
      <TimetableContainer
        session="S"
        lecturesByTime={springSession}
        headColSpans={springColSpans}
        lectures={springLectures}
      />
    </>
  )
}

/*
 * The container specifies formatting for all of the elements wrapped inside,
 * (for example, every element inside a container will follow the same margin rules)
 */
const TimetableContainer = props => {
  return (
    <div className="col-md-5 col-12 timetable-container">
      <Timetable
        session={props.session}
        lecturesByTime={props.lecturesByTime}
        headColSpans={props.headColSpans}
        lectures={props.lectures}
      />
    </div>
  )
}

/*
 * A <table> element for the specified session
 */
const Timetable = props => {
  const modal = React.createRef()
  const displayMap = session =>
    useCallback(() => {
      modal.current.openModal()
    }, [])

  return (
    <table className={"timetable table"} id={"timetable-" + props.session}>
      <MapModal ref={modal} lectures={props.lectures} />
      <TimetableHeader
        session={props.session}
        lecturesByTime={props.lecturesByTime}
        headColSpans={props.headColSpans}
        openMap={displayMap}
      />
      <TimetableBody
        session={props.session}
        lecturesByTime={props.lecturesByTime}
        headColSpans={props.headColSpans}
      />
    </table>
  )
}

/*
 * Describes what the header of a table should look like, based on the session.
 * The header contains five day cells, a dummy cell, and a term-name cell
 */
const TimetableHeader = props => {
  const dayStrings = ["Mon", "Tue", "Wed", "Thu", "Fri"]
  const colSpans = props.headColSpans

  const dayCells = []
  for (let i = 0; i < 5; i++) {
    const dayString = dayStrings[i]
    // Store the React element for this day-cell
    dayCells.push(
      <th scope="col" colSpan={colSpans[i]} key={"day-header-" + i}>
        {dayString}
      </th>
    )
  }

  if (props.session === "F") {
    return (
      <thead>
        <tr>
          <th className="timetable-dummy-cell"></th>
          <th className="term-name">
            <img
              src="/static/res/ico/blue-marker.png"
              className="map-icon"
              onClick={() => props.openMap(props.session)}
            />
            Fall
          </th>
          {dayCells}
        </tr>
      </thead>
    )
  } else {
    return (
      <thead>
        <tr>
          {dayCells}
          <th className="term-name">
            Spring
            <img
              src="/static/res/ico/blue-marker.png"
              className="map-icon"
              onClick={() => props.openMap(props.session)}
            />
          </th>
          <th className="timetable-dummy-cell"></th>
        </tr>
      </thead>
    )
  }
}

/*
 * Describes the body of the Timetable
 */
const TimetableBody = props => {
  const rows = []
  // For each row from 8 o'clock to 22 o'clock, there is an 'Hour' and 'Half hour' row
  for (let i = 8; i < 22; i += 0.5) {
    rows.push(
      <TimetableRow
        session={props.session}
        time={i}
        key={"timetable-row-" + i + props.session}
        currentLectures={props.lecturesByTime[i]}
        headColSpans={props.headColSpans}
      />
    )
  }

  return <tbody>{rows}</tbody>
}

/*
 * Describes what a row in the Timetable should look like,
 * based off of the session, time, and previous cells generated.
 */
const TimetableRow = props => {
  const tableData = []
  const dummyCell = (
    <td key="timetable-dummy-cell" className="timetable-dummy-cell"></td>
  )
  const currTime = props.time
  const currSess = props.session

  // The dictionary of lectures for this hour
  const currentLectures = props.currentLectures
  const headColSpans = props.headColSpans

  for (let day = 0; day < 5; day++) {
    // Variables for calculations
    const headerColSpan = headColSpans[day]
    let totalColSpans = 0

    // Get the list of lectures at this time-day slot, as well as the list of lectures
    // occuring one hour prior this time-day slot
    const currentLectureList = currentLectures[day]

    if (currentLectureList.length !== 0) {
      // Render every lecture at this day-time slot (there can be more than one in a conflict case)
      currentLectureList.forEach(lecture => {
        // The 'colSpan' of the cells taken by this lecture, in this time-day slot
        // This should always be an integer value, since headerColSpan is the product of all
        // possible lecture widths in that day.
        const lecColSpan = headerColSpan / lecture.width

        if (lecture.startTime === currTime) {
          const rowSpan = 2 * (lecture.endTime - lecture.startTime)
          tableData.push(
            <td
              id={"" + day.toString() + currTime + "-0" + totalColSpans + currSess}
              key={"" + day.toString() + currTime + "-0" + totalColSpans + currSess}
              data-in-conflict={lecture.inConflict}
              data-satisfied={lecture.dataSatisfied}
              rowSpan={rowSpan}
              colSpan={lecColSpan}
              className="timetable-cell timetable-edge"
              type="L"
              clicked="true"
              data-currentlecturelist={currentLectureList}
            >
              {lecture.courseCode}
            </td>
          )
        }
        // Record the total width taken up by this time-day slot so far,
        // from all the lectures generated
        totalColSpans += lecColSpan
      })
    }
    // In the case where there are not enough cells to fill the headerColSpan
    // of this time-day cell or if there are no courses to be generated at this time-day cell,
    // generate remaining number of empty cells
    // Note: totalColSpans is 0 if there are no courses to be generated at this time-day cell
    for (let i = totalColSpans; i < headerColSpan; i++) {
      tableData.push(
        <td
          id={"" + day.toString() + currTime + "-0" + i + currSess}
          key={"" + day.toString() + currTime + "-0" + i + currSess}
          data-in-conflict="false"
          data-satisfied="true"
          rowSpan="1"
          className={
            currTime % 1 === 0 ? "timetable-cell-tophalf" : "timetable-cell-bottomhalf"
          }
        ></td>
      )
    }
  }

  if (currTime % 1 === 0) {
    // Convert <time> to a 12-hour clock system
    const adjustedTime = (currTime === 12 ? 12 : currTime % 12) + ":00"
    const timeCell = (
      <td key="timetable-time" className="timetable-time" rowSpan="2">
        {adjustedTime}
      </td>
    )

    // Adjust the order at which these cells are rendered
    if (currSess === "F") {
      tableData.unshift(timeCell)
      tableData.unshift(dummyCell)
    } else if (currSess === "S") {
      tableData.push(timeCell)
      tableData.push(dummyCell)
    }
  } else {
    // Adjust the order at which these cells are rendered
    if (props.session === "F") {
      tableData.unshift(dummyCell)
    } else if (props.session === "S") {
      tableData.push(dummyCell)
    }
  }

  return <tr>{tableData}</tr>
}

/**
 * Helper function to initialize the <fallSession> and <springSession> 2-D dictionaries
 * @param {list} lectures : List of 'Lecture' objects
 */
function initializeSessions(lectures) {
  const fallSession = {}
  const springSession = {}
  for (let i = 7; i < 22; i += 0.5) {
    fallSession[i] = { 0: [], 1: [], 2: [], 3: [], 4: [] }
    springSession[i] = { 0: [], 1: [], 2: [], 3: [], 4: [] }
  }

  lectures.forEach(lecture => {
    if (lecture.session === "F" || lecture.session === "Y") {
      for (let i = lecture.startTime; i < lecture.endTime; i += 0.5) {
        // Store this Lecture in its active time-slot (denoted by <i>),
        // and in the list in its active day slot (denoted by <lecture.day>), in <fallSession>.
        fallSession[i][lecture.day].push(lecture)
      }
    }
    if (lecture.session === "S" || lecture.session === "Y") {
      // Same process as above for spring lectures in <springSession>
      for (let i = lecture.startTime; i < lecture.endTime; i += 0.5) {
        springSession[i][lecture.day].push(lecture)
      }
    }
  })
  return [fallSession, springSession]
}

/**
 * Helper function which sets the width of each Lecture,
 * and also sets the inConflict attribute of all Lecture objects
 * @param {dictionary} session : 2-D Dictionary storing lectures active in the session
 */
function setWidths(session) {
  for (let i = 8; i < 22; i += 0.5) {
    const timeRow = session[i]
    // Reset all widths back to 1 and inConflict to false if this time slot is the startTime of
    // the lecture
    for (let day = 0; day < 5; day++) {
      const timeDaySlot = timeRow[day]
      timeDaySlot.forEach(lecture => {
        // Do not set the width back to 1 if there is already a conflict (this situation applies to
        // 'Y' session lectures)
        if (lecture.startTime === i && lecture.width === 1) {
          lecture.width = 1
          lecture.inConflict = false
        }
        // If in this time-and-day slot there is a lecture conflict
        if (lectureConflict(timeDaySlot)) {
          // Readjust (if needed) the 'width's of the lectures at this slot
          if (lecture.width < timeDaySlot.length) {
            lecture.width = timeDaySlot.length
          }
          // Also specify that this lecture is in conflict
          lecture.inConflict = true
        }
      })
    }
  }
}

/**
 * Helper function which stores the 'colSpan' attribute value for each day in the table header
 * @param {dictionary} colSpans : Dictionary storing the 'colSpan' attribute value for each day
 */
function storeColSpans(session, colSpans) {
  // For each day, stores all possible 'width' values every lecture at that day could have
  const widths = storeWidths(session)
  for (let day = 0; day < 5; day++) {
    // The 'colSpan' attribute of a header day cell is defined as the product of all
    // possible widths that could occur at that day
    colSpans[day] = widths[day].reduce((a, b) => a * b, 1)
  }
}

/**
 * Helper function which, for each day, stores all possible 'width' values the lectures in that day could have
 * @param {dictionary} session : 2-D Dictionary storing lectures active in the session
 */
function storeWidths(session) {
  const widths = {}
  // Iterate through every time-day slot in the session
  for (let day = 0; day < 5; day++) {
    widths[day] = []
    for (let i = 8; i < 22; i += 0.5) {
      const timeRow = session[i]
      const timeDaySlot = timeRow[day]
      // If there are lectures running at this time-day slot
      if (timeDaySlot.length > 0) {
        // Retrieve a lecture width at this time-day slot
        const width = timeDaySlot[0].width
        // Store the width of this lecture, if it hasn't already been stored
        if (widths[day].indexOf(width) < 0) {
          widths[day].push(width)
        }
      }
    }
  }
  return widths
}

/**
 * Helper function which returns if there is a course conflict at a time-and-day slot
 * (denoted by the number of courses in the list at this time-and-day slot)
 */
function lectureConflict(courseList) {
  return courseList.length > 1
}

/**
 * Create a list of lecture objects representing each separate time a given lecture section takes place
 * @param {Object} lectureSection : Represents a lecture section for a course with time periods in which the lecture
 *                            takes place
 * @return {array} An array of objects representing each disconnected time period the given
 *                  lecture occurs in
 */
function createNewLectures(lectureSection, index, lectureSections) {
  // dataSatisfied is false if a tutorial or practical for a course is selected in a particular session(F or S) and there
  // is no lecture selected for that course in the given session.
  lectureSection.dataSatisfied = false
  const lectureSelected = lectureSections.filter(lecture => {
    return (
      lecture.lectureCode.substring(0, 1) === "L" &&
      lecture.courseCode.substring(0, 6) ===
        lectureSection.courseCode.substring(0, 6) &&
      lecture.session === lectureSection.session
    )
  })
  if (lectureSelected.length > 0) {
    lectureSection.dataSatisfied = true
  }

  const dayStrings = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday"]
  const lectures = lectureSection.times.map(time => {
    return {
      courseCode: lectureSection.courseCode,
      session: lectureSection.session,
      day: time.weekDay,
      dayString: dayStrings[time.weekDay],
      startTime: time.startHour,
      endTime: time.endHour,
      fstRoom: time.firstRoom,
      secRoom: time.secondRoom,
      inConflict: false,
      width: 1,
      dataSatisfied: lectureSection.dataSatisfied,
    }
  })
  return lectures
}
