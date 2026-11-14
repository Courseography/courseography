import { SelectedLecture } from "./types"
import { LectureSection } from "./lecture_section"

export interface SectionListProps {
  courseCode: string
  session: string
  lectures: SelectedLecture[]
  hoverLecture: (lecture: SelectedLecture) => void
  unhoverLecture: () => void
  selectLecture: (lecture: SelectedLecture) => void
}

/**
 * A list of lecture, tutorial and practical sections for the specified course for the specified
 * session.
 */

export function SectionList({
  courseCode,
  session,
  lectures,
  hoverLecture,
  unhoverLecture,
  selectLecture,
}: SectionListProps) {
  const lectureSections = lectures.map(lecture => (
    <LectureSection
      key={courseCode + lecture.lectureCode}
      session={session}
      courseCode={courseCode}
      lecture={lecture}
      hoverLecture={hoverLecture}
      unhoverLecture={unhoverLecture}
      selectLecture={selectLecture}
    />
  ))
  return (
    <ul className={"sectionList-" + session} id="lecture-list">
      {lectureSections}
    </ul>
  )
}
