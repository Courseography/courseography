import type { JSX } from "react";
import { LectureTimeframe } from "../common/types";

export interface TimetableRowProps {
  session: "F" | "S"
  currentLectures: Record<number, LectureTimeframe[]>
  headColSpans: Record<number, number>
  time: number
}

/**
 * A table row element of the timetable, representing one half-hour interval.
 * 
 * @param session the selected session
 * @param currentLectures a dictionary mapping the time of each interval to the lectures it overlaps
 * @param headColSpans a map storing the 'colSpan' attribute of every day in the header
 * @param time the start of the interval in decimal hours (e.g. 13.5 for the interval 13:30-14:00)
 */
export function TimetableRow({
  session, currentLectures, headColSpans, time
}: TimetableRowProps): JSX.Element {
  const tableData = [];
  const dummyCell = (
    <td key="timetable-dummy-cell" className="timetable-dummy-cell"></td>
  );
  const currTime = time;
  const currSess = session;

  for (let day = 0; day < 5; day++) {
    // Variables for calculations
    const headerColSpan = headColSpans[day];
    let totalColSpans = 0;

    // Get the list of lectures at this time-day slot, as well as the list of lectures
    // occuring one hour prior this time-day slot
    const currentLectureList = currentLectures[day];

    if (currentLectureList.length !== 0) {
      // Render every lecture at this day-time slot (there can be more than one in a conflict case)
      currentLectureList.forEach(lecture => {
        // The 'colSpan' of the cells taken by this lecture, in this time-day slot
        // This should always be an integer value, since headerColSpan is the product of all
        // possible lecture widths in that day.
        const lecColSpan = headerColSpan / lecture.width;

        if (lecture.startTime === currTime) {
          const rowSpan = 2 * (lecture.endTime - lecture.startTime);
          tableData.push(
            <td
              id={"" + day.toString() + currTime + "-0" + totalColSpans + currSess}
              key={"" + day.toString() + currTime + "-0" + totalColSpans + currSess}
              data-in-conflict={lecture.inConflict}
              data-satisfied={lecture.dataSatisfied}
              rowSpan={rowSpan}
              colSpan={lecColSpan}
              className="timetable-cell timetable-edge"
              data-type="L"
              data-clicked="true"
              data-currentlecturelist={currentLectureList}
            >
              {lecture.courseCode}
            </td>
          );
        }
        // Record the total width taken up by this time-day slot so far,
        // from all the lectures generated
        totalColSpans += lecColSpan;
      });
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
          rowSpan={1}
          className={currTime % 1 === 0 ? "timetable-cell-tophalf" : "timetable-cell-bottomhalf"}
        ></td>
      );
    }
  }

  if (currTime % 1 === 0) {
    // Convert <time> to a 12-hour clock system
    const adjustedTime = (currTime === 12 ? 12 : currTime % 12) + ":00";
    const timeCell = (
      <td key="timetable-time" className="timetable-time" rowSpan={2}>
        {adjustedTime}
      </td>
    );

    // Adjust the order at which these cells are rendered
    if (currSess === "F") {
      tableData.unshift(timeCell);
      tableData.unshift(dummyCell);
    } else if (currSess === "S") {
      tableData.push(timeCell);
      tableData.push(dummyCell);
    }
  } else {
    // Adjust the order at which these cells are rendered
    if (session === "F") {
      tableData.unshift(dummyCell);
    } else if (session === "S") {
      tableData.push(dummyCell);
    }
  }
  return <tr>{tableData}</tr>;
}

