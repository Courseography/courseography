import type { JSX } from "react";
import { TimetableProps } from "./Timetable";
import { Timetable } from "./Timetable";

/**
 * The container specifies formatting for all of the elements wrapped inside,
 * (for example, every element inside a container will follow the same margin rules)
 * 
 * @param session the selected session
 * @param lectures the selected lecture sections currently added to the timetable
 * @param lecturesByTime a map listing the selected lectures for each half-hour interval
 * @param headColSpans a map storing the 'colSpan' attribute of every day in the header
 */
export function TimetableContainer({
  session, lectures, lecturesByTime, headColSpans
}: TimetableProps): JSX.Element {
  return (
    <div className="col-md-5 col-12 timetable-container">
      <Timetable
        session={session}
        lecturesByTime={lecturesByTime}
        headColSpans={headColSpans}
        lectures={lectures} />
    </div>
  );
}
