import { JSX } from "react"
import { AgGridReact } from "ag-grid-react"
import { CourseDetails, Sessions } from "./types"

interface DescriptionProps {
  course: CourseDetails
  sessions: Sessions
}

/**
 * Renders the descriptive content and timetable grid of a course within CourseModal.
 *
 * Dynamically displays all populated course requirement fields (prerequisites,
 * corequisites, exclusions, breadth, etc.) with course codes linked to navigation,
 * alongside an AG Grid table displaying lecture meeting times and locations per session.
 *
 * @param course Formatted course details object containing description and requirement lists.
 * @param sessions Meeting times data partitioned by academic session (F, S, Y).
 */
export function Description({ course, sessions }: DescriptionProps): JSX.Element {
  const timetableUnavailable =
    Object.keys(sessions).length !== 0 &&
    sessions.F?.length === 0 &&
    sessions.S?.length === 0 &&
    sessions.Y?.length === 0

  let timetableDisplay
  if (timetableUnavailable) {
    timetableDisplay = (
      <div className="timetable-unavailable">No timetable information available</div>
    )
  } else {
    timetableDisplay = Object.keys(sessions).map(session =>
      sessions[session]?.length !== 0 ? (
        <div key={session}>
          <strong className="semester-heading">
            {session === "F" ? "Fall" : session === "S" ? "Winter" : "Full Year"}
          </strong>
          <div className="ag-theme-alpine" style={{ width: 1088 }}>
            <AgGridReact
              rowData={sessions[session]}
              columnDefs={[
                { field: "activity", width: session === "Y" ? 96 : 155 },
                { field: "instructor", width: session === "Y" ? 190 : 215 },
                { field: "availability", width: session === "Y" ? 180 : 205 },
                { field: "waitList", width: session === "Y" ? 130 : 145 },

                {
                  field: "time1",
                  headerName: session === "Y" ? "Fall Time" : "Time",
                  wrapHeaderText: true,
                  cellStyle: {
                    whiteSpace: "pre",
                    lineHeight: "1.8",
                    paddingTop: "7px",
                    paddingBottom: "6px",
                  },
                  valueFormatter: col => col.data?.time1.join("\n") ?? "",
                  width: session === "Y" ? 155 : 205,
                  autoHeight: true,
                },
                {
                  field: "location1",
                  headerName: session === "Y" ? "Fall Location" : "Location",
                  wrapHeaderText: true,
                  cellStyle: {
                    whiteSpace: "pre",
                    lineHeight: "1.8",
                    paddingTop: "7px",
                    paddingBottom: "6px",
                  },
                  valueFormatter: col => col.data?.location1.join("\n") ?? "",
                  width: session === "Y" ? 90 : 153,
                  autoHeight: true,
                },
                {
                  field: "time2",
                  headerName: "Winter Time",
                  wrapHeaderText: true,
                  cellStyle: {
                    whiteSpace: "pre",
                    lineHeight: "1.8",
                    paddingTop: "7px",
                    paddingBottom: "6px",
                  },
                  valueFormatter: col => col.data?.time2.join("\n") ?? "",
                  width: 155,
                  autoHeight: true,
                  hide: session !== "Y",
                },
                {
                  field: "location2",
                  headerName: "Winter Location",
                  wrapHeaderText: true,
                  cellStyle: {
                    whiteSpace: "pre",
                    lineHeight: "1.8",
                    paddingTop: "7px",
                    paddingBottom: "6px",
                  },
                  valueFormatter: col => col.data?.location2.join("\n") ?? "",
                  width: 90,
                  autoHeight: true,
                  hide: session !== "Y",
                },
              ]}
              domLayout="autoHeight"
            ></AgGridReact>
          </div>
        </div>
      ) : (
        <div key={session}></div>
      )
    )
  }

  const COURSE_DETAILS_FIELDS = [
    { key: "prereqString", label: "Prerequisite: " },
    { key: "coreqs", label: "Corequisite: " },
    { key: "prep", label: "Recommended Preparation: " },
    { key: "exclusions", label: "Exclusion: " },
    { key: "distribution", label: "Distribution Requirement Status: " },
    { key: "breadth", label: "Breadth Requirement: " },
  ] as const

  //We want to use the Timetable component, but that component needs to be independent before using it here
  return (
    <div>
      {/* Description does not have a strong label prefix */}
      {(course.description?.length ?? 0) > 0 && <p>{course.description}</p>}

      {/* Dynamically render all labeled course fields */}
      {COURSE_DETAILS_FIELDS.map(({ key, label }) => {
        const content = course[key]
        if (!content || content.length === 0) return null

        return (
          <p key={key}>
            <strong>{label}</strong>
            {content}
          </p>
        )
      })}

      <p>
        <strong>Timetable: </strong>
      </p>

      {timetableDisplay}
    </div>
  )
}
