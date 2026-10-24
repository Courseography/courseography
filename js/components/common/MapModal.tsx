import { JSX, useImperativeHandle, useState } from "react"
import ReactModal from "react-modal"
import { CampusMap } from "./CampusMap"
import { MapSidebar } from "./MapSidebar"
import { BuildingGroup, LectureTimeframe } from "./types"

export interface MapModalHandle {
  openModal: () => void
}

interface MapModalProps {
  ref?: React.Ref<MapModalHandle>
  lectures: LectureTimeframe[]
}

/**
 * Modal popup that displays an interactive campus map alongside a weekly lecture schedule.
 *
 * Groups the user's lectures by building and day, allowing users to visualize where
 * their classes take place on campus and filter map markers by day or specific lecture sections.
 * Exposes an imperative `openModal` handle via ref so external components (e.g. TimetableHeader)
 * can trigger the modal to open.
 *
 * @param lectures List of lecture timeframes scheduled in the current timetable.
 * @param ref Optional ref exposing the `MapModalHandle` (`openModal`).
 */
export function MapModal({ ref, lectures }: MapModalProps): JSX.Element {
  const [modalIsOpen, setModalIsOpen] = useState<boolean>(false)
  const [selectedLecTimeframes, setSelectedLecTimeframes] = useState<
    LectureTimeframe[]
  >([])

  useImperativeHandle(ref, () => ({
    openModal: () => {
      setModalIsOpen(true)
    },
  }))

  const closeModal = (): void => {
    setModalIsOpen(false)
  }

  // Group each lecture time by the building they occur in. If the lecture has 2 rooms in different
  // buildings, it will appear under both of the buildings. Lectures for each building are grouped by
  // their course codes
  const groupLecturesByBuilding = (
    lecsByBuilding: BuildingGroup[],
    lecture: LectureTimeframe
  ): void => {
    const loc = lecture.location
    if (!loc) return

    const foundBuilding = lecsByBuilding.find(b => b.buildingName === loc.buildingName)

    const timeframeData = {
      courseCode: lecture.courseCode,
      session: lecture.session,
      startTime: lecture.startTime,
      endTime: lecture.endTime,
      room: loc.room,
    }

    const dayData = {
      dayString: lecture.dayString,
      dayOrder: lecture.day,
      timeframes: [timeframeData],
    }

    // if the building is not in the lecsByBuilding yet, add it into the array
    if (!foundBuilding) {
      lecsByBuilding.push({
        buildingName: loc.buildingName,
        buildingCode: loc.buildingCode,
        address: loc.buildingAddress,
        lat: loc.buildingLat,
        lng: loc.buildingLng,
        postalCode: loc.buildingPostalCode,
        days: [dayData],
      })
    }

    // the building is already in lecsByBuilding, so add the timeframe to the course if the course already
    // exists. Otherwise, add the course with the timeframe
    else {
      const foundDay = foundBuilding.days.find(c => c.dayOrder === lecture.day)

      if (!foundDay) {
        foundBuilding.days.push(dayData)
      } else {
        foundDay.timeframes.push(timeframeData)
      }
    }
  }

  // Add lecture to selectedLecTimeframes if lecture does not already exist in selectedLecTimeframes.
  // Otherwise, remove it from selectedLecTimeframes
  const selectLecTimeframe = (lecture: LectureTimeframe): void => {
    const updatedLecTimeframes = selectedLecTimeframes.slice()

    const lecTimeframeInd = findSelectedLecTimeframe(lecture, updatedLecTimeframes)

    if (lecTimeframeInd == -1) {
      updatedLecTimeframes.push(lecture)
    } else {
      updatedLecTimeframes.splice(lecTimeframeInd, 1)
    }

    setSelectedLecTimeframes(updatedLecTimeframes)
  }

  // Add each element of lectures that does not already exist in selectedLecTimeframes to selectedLecTimeframes
  const addDayTimeframes = (lectures: LectureTimeframe[]): void => {
    const updatedLecTimeframes = selectedLecTimeframes.slice()

    for (const lec of lectures) {
      if (findSelectedLecTimeframe(lec, updatedLecTimeframes) == -1) {
        updatedLecTimeframes.push(lec)
      }
    }

    setSelectedLecTimeframes(updatedLecTimeframes)
  }

  // Remove each element of lectures from selectedLecTimeframes
  const removeDayTimeframes = (lectures: LectureTimeframe[]): void => {
    const updatedLecTimeframes = selectedLecTimeframes.slice()

    for (const lec of lectures) {
      const lecTimeframeInd = findSelectedLecTimeframe(lec, updatedLecTimeframes)

      if (lecTimeframeInd != -1) {
        updatedLecTimeframes.splice(lecTimeframeInd, 1)
      }
    }

    setSelectedLecTimeframes(updatedLecTimeframes)
  }

  // If all elements of lectures are in selectedLecTimeframes, remove all elements of lectures
  // from selectedLecTimeframes. Otherwise, add all elements of lectures to selectedLecTimeframes
  const selectDayTimeframes = (lectures: LectureTimeframe[]): void => {
    if (isDaySelected(lectures)) {
      removeDayTimeframes(lectures)
    } else {
      addDayTimeframes(lectures)
    }
  }

  // Returns True if all elements of lectures are in selectedLecTimeframes
  const isDaySelected = (lectures: LectureTimeframe[]): boolean => {
    const updatedLecTimeframes = selectedLecTimeframes.slice()
    return lectures.every(
      lec => findSelectedLecTimeframe(lec, updatedLecTimeframes) != -1
    )
  }

  // Returns the index of lecture in selectedLecTimeframes if it exists. Otherwise, return -1
  const findSelectedLecTimeframe = (
    lecture: LectureTimeframe,
    selectedLectures: LectureTimeframe[]
  ): number => {
    return selectedLectures.findIndex(
      lec =>
        lec.courseCode === lecture.courseCode &&
        lec.session === lecture.session &&
        lec.day == lecture.day &&
        lec.startTime == lecture.startTime &&
        lec.endTime == lecture.endTime
    )
  }

  const lecturesByBuilding: BuildingGroup[] = []

  const lecturesByDay: Record<string, LectureTimeframe[]> = {}

  lectures.forEach(lecture => {
    if (lecture.location) {
      groupLecturesByBuilding(lecturesByBuilding, lecture)
    }

    if (lecturesByDay[lecture.dayString]) {
      lecturesByDay[lecture.dayString].push(lecture)
    } else {
      lecturesByDay[lecture.dayString] = [lecture]
    }
  })

  return (
    <ReactModal
      className="modal-class"
      overlayClassName="overlay"
      isOpen={modalIsOpen}
      onRequestClose={closeModal}
      ariaHideApp={false}
    >
      <div className="modal-body">
        <CampusMap
          lecturesByBuilding={lecturesByBuilding}
          selectedLecTimeframes={selectedLecTimeframes}
        />
        <MapSidebar
          lecturesByDay={lecturesByDay}
          selectLecTimeframe={selectLecTimeframe}
          selectDayTimeframes={selectDayTimeframes}
          isDaySelected={isDaySelected}
          findSelectedLecTimeframe={findSelectedLecTimeframe}
          selectedLecTimeframes={selectedLecTimeframes}
        />
      </div>
    </ReactModal>
  )
}
