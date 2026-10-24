import { JSX, useState } from "react"
import { convertTimeToString } from "./utils"
import { BuildingLocation, LectureTimeframe } from "./types"

interface DayBoxProps {
  day: string
  dayLectures: LectureTimeframe[]
  selectLecTimeframe: (lecture: LectureTimeframe) => void
  selectDayTimeframes: (lecture: LectureTimeframe[]) => void
  isDaySelected: (lectures: LectureTimeframe[]) => boolean
  findSelectedLecTimeframe: (
    lecture: LectureTimeframe,
    selectedLectures: LectureTimeframe[]
  ) => number
  selectedLecTimeframes: LectureTimeframe[]
}

/**
 * Accordion day container in the map modal sidebar.
 *
 * Displays the day header with selection indicators and expansion arrows. When expanded,
 * lists each lecture occurring on that day with its time, room, and building location,
 * allowing individual lectures to be clicked to highlight their corresponding pin on the map.
 *
 * @param day The name of the weekday (e.g., "Monday").
 * @param dayLectures Array of lecture timeframes occurring on this day.
 * @param selectLecTimeframe Callback to toggle selection of a specific lecture timeframe.
 * @param selectDayTimeframes Callback to select or deselect all lectures for this day.
 * @param isDaySelected Function checking if all lectures on this day are currently selected.
 * @param findSelectedLecTimeframe Helper function checking if a lecture is selected.
 * @param selectedLecTimeframes List of currently selected lecture timeframes.
 */
export function DayBox({
  day,
  dayLectures,
  selectLecTimeframe,
  selectDayTimeframes,
  isDaySelected,
  findSelectedLecTimeframe,
  selectedLecTimeframes,
}: DayBoxProps): JSX.Element {
  const [expanded, setExpanded] = useState<boolean>(false)

  const toggleExpand = (): void => {
    setExpanded(prev => !prev)
  }

  const formatLocation = (location: BuildingLocation): string => {
    return "Location: " + location.buildingName + " (" + location.buildingCode + ")"
  }

  const isSelected = isDaySelected(dayLectures)

  const arrowColour = isSelected ? "lightpurple" : "darkpurple"
  const direction = expanded ? "doubleup-" : "doubledown-"

  return (
    <div>
      <div
        className={
          "sidebar-day " +
          (expanded ? "accordian-expanded" : "accordian-collapsed") +
          (isSelected ? " selected-day" : " unselected-day")
        }
      >
        <div id="map-day-title">
          <h3
            onClick={() => selectDayTimeframes(dayLectures)}
            style={{ display: "inline-block" }}
          >
            {" "}
            {day}{" "}
          </h3>
        </div>

        <img
          src={`/static/res/ico/${direction}${arrowColour}.png`}
          id="expand-icon"
          onClick={toggleExpand}
        />
      </div>

      {expanded && (
        <ul id="map-day-list">
          {dayLectures.map(lec => {
            let locationStr = ""
            if (lec.location) {
              locationStr += formatLocation(lec.location)
            }

            return (
              <li
                key={lec.courseCode + "-" + lec.startTime + "-" + lec.endTime}
                className={
                  "map-day-lec " +
                  (findSelectedLecTimeframe(lec, selectedLecTimeframes) === -1
                    ? "unselected-day-lec"
                    : "selected-day-lec")
                }
                onClick={() => selectLecTimeframe(lec)}
              >
                <h4> {lec.courseCode} </h4>
                <h5>
                  {" "}
                  {convertTimeToString(lec.startTime) +
                    "-" +
                    convertTimeToString(lec.endTime)}{" "}
                </h5>
                <h5> {locationStr} </h5>
              </li>
            )
          })}
        </ul>
      )}
    </div>
  )
}
