import type { JSX } from "react";
import { TimetableRow } from "./TimetableRow";
import { TimetableMap } from "./types";

export interface TimetableBodyProps {
  session: "F" | "S"
  lecturesByTime: TimetableMap
  headColSpans: Record<number, number>
}

/**
 * Describes the body of the timetable grid.
 * 
 * @param session the selected session
 * @param lecturesByTime a map listing the selected lectures for each half-hour interval
 * @param headColSpans a map storing the 'colSpan' attribute of every day in the header
 */
export function TimetableBody({
  session, lecturesByTime, headColSpans
}: TimetableBodyProps): JSX.Element {
  const rows = [];
  // For each row from 8 o'clock to 22 o'clock, there is an 'Hour' and 'Half hour' row
  for (let i = 8; i < 22; i += 0.5) {
    rows.push(
      <TimetableRow
        session={session}
        time={i}
        key={"timetable-row-" + i + session}
        currentLectures={lecturesByTime[i]}
        headColSpans={headColSpans} />
    );
  }

  return <tbody>{rows}</tbody>;
}
