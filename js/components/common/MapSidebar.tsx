import type { JSX } from "react"
import { DayBox } from "./DayBox"
import type { LectureTimeframe } from "./types"

interface MapSidebarProps {
  lecturesByDay: Record<string, LectureTimeframe[]>
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
 * Collapsible weekly sidebar for the campus map modal.
 *
 * Iterates through the weekdays (Monday through Friday) and renders a `DayBox`
 * for each day that contains scheduled lectures, allowing users to expand days
 * and toggle selection of specific lecture timeframes.
 *
 * @param lecturesByDay Dictionary mapping day names to their array of lecture timeframes.
 * @param selectLecTimeframe Callback to toggle selection of a single lecture timeframe.
 * @param selectDayTimeframes Callback to select or deselect all lectures on a given day.
 * @param isDaySelected Function returning whether all lectures on a day are currently selected.
 * @param findSelectedLecTimeframe Helper function finding the index of a lecture in selected lectures.
 * @param selectedLecTimeframes List of currently selected lecture timeframes.
 */
export function MapSidebar({
  lecturesByDay,
  selectLecTimeframe,
  selectDayTimeframes,
  isDaySelected,
  findSelectedLecTimeframe,
  selectedLecTimeframes,
}: MapSidebarProps): JSX.Element {
  const dayLectures = []
  const days = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday"]

  for (const day of days) {
    if (lecturesByDay[day]) {
      dayLectures.push(
        <DayBox
          key={day}
          day={day}
          dayLectures={lecturesByDay[day]}
          selectLecTimeframe={selectLecTimeframe}
          selectDayTimeframes={selectDayTimeframes}
          isDaySelected={isDaySelected}
          findSelectedLecTimeframe={findSelectedLecTimeframe}
          selectedLecTimeframes={selectedLecTimeframes}
        />
      )
    }
  }

  return <div id="map-sidebar">{dayLectures}</div>
}
