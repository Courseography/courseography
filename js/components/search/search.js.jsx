import React from "react"
import { createRoot } from "react-dom/client"

function filterCourse(inst, time, lec) {
  return lec.meetData.instructor.indexOf(inst) >= 0 && hasTime(time, lec.timeData)
}

function hasTime(timeStr, times) {
  if (timeStr === "") {
    return true // Blank value means the time input has been left empty
  }
  const day = "MTWRF".indexOf(timeStr[0])
  const hour = parseInt(timeStr.substr(1))
  if (day < 0 || isNaN(hour)) {
    return false // Invalid weekday or hour being searched for
  }

  return times.some(time => {
    return time.weekDay === day && time.startHour <= hour && hour < time.endHour
  })
}

function timeToString(timeData) {
  const times = timeData
    .sort((t1, t2) => {
      if (
        t1.weekDay > t2.weekDay ||
        (t1.weekDay == t2.weekDay && t1.startHour > t2.startHour)
      ) {
        return 1
      } else if (
        t1.weekDay < t2.weekDay ||
        (t1.weekDay == t2.weekDay && t1.startHour < t2.startHour)
      ) {
        return -1
      } else {
        return 0
      }
    })
    .map(time => {
      if (time.weekDay > 4) {
        return ""
      }
      let s = "MTWRF"[time.weekDay]
      if (s === undefined) {
        console.log(time)
        return ""
      }
      const startHour = time.startHour <= 12 ? time.startHour : time.startHour % 12
      const endHour = time.endHour <= 12 ? time.endHour : time.endHour % 12
      s += `${startHour}-${endHour}`
      return s
    })
    .filter(s => s.length > 0)

  return times.join(", ")
}

class Search extends React.Component {
  constructor(props) {
    super(props)
    this.state = {
      curDept: "",
      depts: [],
    }
    this.updateDept = this.updateDept.bind(this)
    this.deptRef = React.createRef(null)
    this.timetableRef = React.createRef(null)
  }

  componentDidMount() {
    $.ajax({
      url: "/depts",
      dataType: "json",
      success: data => {
        this.setState({ depts: data })
      },
      error: (xhr, status, err) => {
        console.error("course-info", status, err.toString())
      },
    })
  }

  updateDept() {
    const selectedDept = this.deptRef.current.value
    this.setState({ curDept: selectedDept })
    this.timetableRef.current.populateTable(selectedDept)
  }

  render() {
    const options = this.state.depts.map(dept => {
      return (
        <option key={dept} value={dept}>
          {dept}
        </option>
      )
    })

    return (
      <div id="search">
        <div id="timetableSearch">
          <h2>Timetable Search</h2>
          <div id="searchOptions">
            <label htmlFor="deptSelect">Dept:</label>
            <select
              ref={this.deptRef}
              name="dept"
              onChange={this.updateDept}
              id="deptSelect"
              defaultValue="none"
            >
              <option value="none">---</option>
              {options}
            </select>
            <br />
            <label htmlFor="codeFilter">Code:</label>
            <input
              type="text"
              className="text-input"
              id="codeFilter"
              placeholder="CSC108"
            />
            <br />
            <label htmlFor="instFilter">Instructor:</label>
            <input
              type="text"
              className="text-input"
              id="instFilter"
              placeholder="Liu"
            />
            <br />
            <label htmlFor="timeFilter">Time:</label>
            <input
              type="text"
              className="text-input"
              id="timeFilter"
              placeholder="M10"
            />
          </div>
        </div>
        <div id="timetableContainer">
          <Timetable dept={this.state.curDept} ref={this.timetableRef} />
        </div>
      </div>
    )
  }
}

class Timetable extends React.Component {
  constructor(props) {
    super(props)
    this.state = { courses: [], codeSearch: "", instSearch: "", timeSearch: "" }
    this.populateTable = this.populateTable.bind(this)
  }

  componentDidMount() {
    document.getElementById("codeFilter").addEventListener("keyup", e => {
      this.setState({ codeSearch: e.target.value })
    })
    document.getElementById("instFilter").addEventListener("keyup", e => {
      this.setState({ codeSearch: e.target.value })
    })
    document.getElementById("timeFilter").addEventListener("keyup", e => {
      this.setState({ codeSearch: e.target.value })
    })
  }

  populateTable(dept) {
    $.ajax({
      url: "/course-info",
      data: { dept: dept },
      dataType: "json",
      success: data => {
        this.setState({ courses: data })
      },
      error: (xhr, status, err) => {
        console.error("course-info", status, err.toString())
      },
    })
  }

  render() {
    const state = this.state
    const courseRows = this.state.courses
      .filter(course => {
        return (
          course.name.indexOf(state.codeSearch) > -1 &&
          course.allMeetingTimes.some(meetingData =>
            filterCourse(state.instSearch, state.timeSearch, meetingData)
          )
        )
      })
      .map(course => {
        const lectures = course.allMeetingTimes
          .filter(meetingData =>
            filterCourse(state.instSearch, state.timeSearch, meetingData)
          )
          .sort((lec1, lec2) => {
            if (lec1.meetData.section > lec2.meetData.section) {
              return 1
            } else if (lec1.meetData.section > lec2.meetData.section) {
              return -1
            } else {
              return 0
            }
          })

        const lectureRows = {
          F: [],
          S: [],
          Y: [],
        }
        for (const lecture of lectures) {
          lectureRows[lecture.meetData.session].push(
            <tr
              key={`${course.name}-${lecture.meetData.session}-${lecture.meetData.section}`}
            >
              <td className="timetableSection">{lecture.meetData.section}</td>
              <td className="timetableTime">{timeToString(lecture.timeData)}</td>
              <td className="timetableInstructor">{lecture.meetData.instructor}</td>
              <td className="timetableCap">
                {lecture.meetData.enrol} / {lecture.meetData.cap}
              </td>
              <td className="timetableWait">{lecture.meetData.wait}</td>
            </tr>
          )
        }

        if (lectureRows["Y"].length === 0) {
          return (
            <tr key={course.name}>
              <td className="timetableCourseName">{course.name}</td>
              <td className="FOffering">
                <table className="courseTable">
                  <tbody>{lectureRows["F"]}</tbody>
                </table>
              </td>
              <td className="SOffering">
                <table className="courseTable">
                  <tbody>{lectureRows["S"]}</tbody>
                </table>
              </td>
            </tr>
          )
        } else {
          return (
            <tr key={course.name}>
              <td className="timetableCourseName">{course.name}</td>
              <td colSpan="2" className="YOffering">
                <table className="courseTable">
                  <tbody>{lectureRows["Y"]}</tbody>
                </table>
              </td>
            </tr>
          )
        }
      })

    return (
      <table id="timetableMain">
        <thead>
          <tr>
            <th className="timetableCourseName">Courses</th>
            <th className="sessionHeader FOffering">Fall</th>
            <th className="sessionHeader SOffering">Spring</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td className="timetableCourseName"></td>
            <td className="FOffering">
              <table className="courseTable">
                <thead>
                  <tr>
                    <th className="timetableSection">Sec</th>
                    <th className="timetableTime">Time</th>
                    <th className="timetableInstructor">Instructor</th>
                    <th className="timetableCap">Cap</th>
                    <th className="timetableWait">Wait</th>
                  </tr>
                </thead>
              </table>
            </td>
            <td className="SOffering">
              <table className="courseTable">
                <thead>
                  <tr>
                    <th className="timetableSection">Sec</th>
                    <th className="timetableTime">Time</th>
                    <th className="timetableInstructor">Instructor</th>
                    <th className="timetableCap">Cap</th>
                    <th className="timetableWait">Wait</th>
                  </tr>
                </thead>
              </table>
            </td>
          </tr>
          {courseRows}
        </tbody>
      </table>
    )
  }
}

const container = document.getElementById("content")
const root = createRoot(container)
root.render(<Search />)
