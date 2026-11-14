import type { JSX } from "react";
import { LectureTimeframe } from "../common/types";
import { createNewLectures, initializeSessions, setWidths, storeColSpans } from "./calendar";
import { TimetableContainer } from "./TimetableContainer";
import { SelectedLecture, TimetableMap } from "./types";

export interface RowProps {
  lectureSections: SelectedLecture[]
}

/**
 * Holds the containers of the Fall and Spring timetables,
 * and performs some pre-processing steps with a list of 'Lecture' objects
 * 
 * @param lectureSections the list of all selected lectures for both sessions
 */
export function Row({ lectureSections }: RowProps): JSX.Element {
  // Create a list of lecture objects
  let rawLectures = lectureSections.map((lectureSection, _index, selectedLectures) => 
    createNewLectures(lectureSection, selectedLectures)
  );
  let lectures = ([] as LectureTimeframe[]).concat.apply([], rawLectures);

  const fallLectures = lectures.filter(lecture => {
    return lecture.session === "F" || lecture.session === "Y";
  });

  const springLectures = lectures.filter(lecture => {
    return lecture.session === "S" || lecture.session === "Y";
  });

  // Organize the structure of the <fallSession> and <springSession> 2-D dictionaries
  let fallSession: TimetableMap, springSession: TimetableMap;[fallSession, springSession] = initializeSessions(lectures);

  // For each session, set the 'width' attribute of each Lecture
  // (go to Lecture constructor for definition of 'width')
  setWidths(fallSession);
  setWidths(springSession);

  // Fill session colSpans dictionaries
  const fallColSpans = {};
  const springColSpans = {};
  storeColSpans(fallSession, fallColSpans);
  storeColSpans(springSession, springColSpans);
  // Generate a container for each of the Fall and Spring timetables individually
  return (
    <>
      <TimetableContainer
        session="F"
        lecturesByTime={fallSession}
        headColSpans={fallColSpans}
        lectures={fallLectures} />
      <TimetableContainer
        session="S"
        lecturesByTime={springSession}
        headColSpans={springColSpans}
        lectures={springLectures} />
    </>
  );
}
