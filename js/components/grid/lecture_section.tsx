import { SelectedLecture } from "./types"

export interface LectureSectionProps {
  session: string
  courseCode: string
  lecture: SelectedLecture
  hoverLecture: (lecture: SelectedLecture) => void
  unhoverLecture: () => void
  selectLecture: (lecture: SelectedLecture) => void
}

/**
 * A lecture, tutorial or practical section for the specified course in the specified
 * session. The section is added to or removed from a list of selected lecture upon click.
 */

export function LectureSection({
  session,
  courseCode,
  lecture,
  hoverLecture,
  unhoverLecture,
  selectLecture,
}: LectureSectionProps) {
  return (
    <li
      id={courseCode + "-" + lecture.lectureCode + "-" + session}
      onClick={() => selectLecture(lecture)}
      onMouseOver={() => hoverLecture(lecture)}
      onMouseOut={() => unhoverLecture()}
      data-satisfied={"true"}
    >
      {lecture.lectureCode}
    </li>
  )
}
