import type { JSX } from "react";


export interface TimetableHeaderProps {
  session: "F" | "S";
  headColSpans: Record<number, number>;
  openMap: (session: "F" | "S") => void;
}

/** 
 * Describes what the header of a table should look like, based on the session.
 * The header contains five day cells, a dummy cell, and a term-name cell
 * 
 * @param session the selected session
 * @param headColSpans a map storing the 'colSpan' attribute of every day in the header
 * @param openMap the callback function used to open the map modal
 */
export function TimetableHeader({
  session, headColSpans, openMap
}: TimetableHeaderProps): JSX.Element {
  const dayStrings = ["Mon", "Tue", "Wed", "Thu", "Fri"];
  const colSpans = headColSpans;

  const dayCells = [];
  for (let i = 0; i < 5; i++) {
    const dayString = dayStrings[i];
    // Store the React element for this day-cell
    dayCells.push(
      <th scope="col" colSpan={colSpans[i]} key={"day-header-" + i}>
        {dayString}
      </th>
    );
  }

  if (session === "F") {
    return (
      <thead>
        <tr>
          <th className="timetable-dummy-cell"></th>
          <th className="term-name">
            <img
              src="/static/res/ico/blue-marker.png"
              className="map-icon"
              onClick={() => openMap(session)} />
            Fall
          </th>
          {dayCells}
        </tr>
      </thead>
    );
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
              onClick={() => openMap(session)} />
          </th>
          <th className="timetable-dummy-cell"></th>
        </tr>
      </thead>
    );
  }
}

