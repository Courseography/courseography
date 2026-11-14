import { type JSX, useCallback, RefObject, useRef } from "react";
import { MapModalHandle, MapModal } from "../common/MapModal";
import { TimetableBody } from "./TimetableBody";
import { TimetableHeader } from "./TimetableHeader";
import { LectureTimeframe } from "../common/types";
import { TimetableMap } from "./types";

export interface TimetableProps {
  session: "F" | "S"
  lectures: LectureTimeframe[]
  lecturesByTime: TimetableMap
  headColSpans: Record<number, number>
}

/**
 * A table element displaying the timetable of selected lectures for a single session.
 *
 * @param session the selected session
 * @param lectures the selected lecture sections currently added to the timetable
 * @param lecturesByTime a map listing the selected lectures for each half-hour interval
 * @param headColSpans a map storing the 'colSpan' attribute of every day in the header
 */
export function Timetable({
  session, lectures, lecturesByTime, headColSpans
}: TimetableProps): JSX.Element {
  const openMapModal = useCallback((modal: RefObject<MapModalHandle | null>) => {
    modal.current?.openModal();
  }, []);

  const modal = useRef<MapModalHandle>(null);
  const displayMap = () => openMapModal(modal);

  return (
    <table className={"timetable table"} id={"timetable-" + session}>
      <MapModal ref={modal} lectures={lectures} />
      <TimetableHeader
        session={session}
        headColSpans={headColSpans}
        openMap={displayMap} />
      <TimetableBody
        session={session}
        lecturesByTime={lecturesByTime}
        headColSpans={headColSpans} />
    </table>
  );
}
