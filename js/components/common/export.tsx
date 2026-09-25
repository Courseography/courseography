import React, { useImperativeHandle, useState } from "react"
import ReactModal from "react-modal"

if (document.getElementById("nav-export")) {
  ReactModal.setAppElement("#nav-export")
}

interface SelectedLecture {
  courseCode: string
  lectureCode: string
  session: string
}

export interface ExportModalHandle {
  getImage: () => void
}

interface ExportModalProps {
  page?: string
  session?: string
  context?: string
  open?: boolean
  onRequestClose?: () => void
  ref?: React.Ref<ExportModalHandle>
}

export function ExportModal({
  page,
  session,
  open,
  onRequestClose,
  ref,
}: ExportModalProps) {
  const [data, setData] = useState("")
  const [otherSession, setOtherSession] = useState("Spring")

  const getGraphImage = () => {
    const necessaryLS: Record<string, string | null> = {}
    for (const elem in localStorage) {
      if (!Object.prototype.hasOwnProperty.call(localStorage, elem)) {
        continue
      }

      if (
        elem.substring(0, 3).match(/^[a-zA-Z]+$/) &&
        elem.substring(3, 6).match(/^\d+$/)
      ) {
        if (document.getElementById(elem)) {
          necessaryLS[elem] = localStorage.getItem(elem)
        }
      } else {
        necessaryLS[elem] = localStorage.getItem(elem)
      }
    }

    const JsonLocalStorageObj = JSON.stringify(necessaryLS)
    $.ajax({
      url: "/image",
      data: { JsonLocalStorageObj: JsonLocalStorageObj },
      success: function (data: string) {
        setData("data:image/png;base64," + data)
      },
      error: function () {
        throw "No image generated"
      },
    })
  }

  const getGridImage = (session: string) => {
    const formattedSession = session.charAt(0).toUpperCase() + session.slice(1)
    const allCourses: SelectedLecture[] =
      JSON.parse(localStorage.getItem("selectedLectures") ?? "null") || []
    const courseData = allCourses.map(
      data => `${data.courseCode.split(" ")[0]}-${data.lectureCode}-${data.session}`
    )
    $.ajax({
      url: "/timetable-image",
      data: { session: formattedSession, courses: courseData.join("_") },
      success: function (data: string) {
        setData("data:image/png;base64," + data)
        setOtherSession(formattedSession === "Fall" ? "Spring" : "Fall")
      },
      error: function () {
        throw "No image generated"
      },
    })
  }

  const toggleSession = () => {
    getGridImage(otherSession)
  }

  const getImage = () => {
    if (page === "graph") {
      getGraphImage()
    } else {
      // `session` is expected to always be provided on this code path.
      getGridImage(session!)
    }
  }

  useImperativeHandle(ref, () => ({ getImage }))

  if (page === "graph") {
    return (
      <ReactModal
        className="modal-class"
        overlayClassName="overlay"
        isOpen={open ?? false}
        onRequestClose={onRequestClose}
      >
        <GraphImage data={data} />
      </ReactModal>
    )
  } else {
    return (
      <ReactModal
        className="modal-class"
        overlayClassName="overlay"
        isOpen={open ?? false}
        onRequestClose={onRequestClose}
      >
        <GridImage data={data} toggleSession={toggleSession} />
      </ReactModal>
    )
  }
}

function getCalendar() {
  const allCourses: SelectedLecture[] =
    JSON.parse(localStorage.getItem("selectedLectures") ?? "null") || []
  const courseData = allCourses.map(
    data => `${data.courseCode.split(" ")[0]}-${data.lectureCode}-${data.session}`
  )
  $.ajax({
    type: "post",
    url: "/calendar",
    data: { courses: courseData.join("_") },
    success: function (data: string) {
      const dataURI = "data:text/calendar;charset=utf8," + escape(data)
      const downloadLink = document.createElement("a")
      downloadLink.href = dataURI
      downloadLink.download = "timetable.ics"
      document.body.appendChild(downloadLink)
      downloadLink.click()
      document.body.removeChild(downloadLink)
    },
    error: function () {
      throw "No calendar avaiable"
    },
  })
}

function getPDF() {
  const necessaryLS: Record<string, string | null> = {}
  for (const elem in localStorage) {
    if (!Object.prototype.hasOwnProperty.call(localStorage, elem)) {
      continue
    }

    if (
      elem.substring(0, 3).match(/^[a-zA-Z]+$/) &&
      elem.substring(3, 6).match(/^\d+$/)
    ) {
      if (document.getElementById(elem)) {
        necessaryLS[elem] = localStorage.getItem(elem)
      }
    } else {
      necessaryLS[elem] = localStorage.getItem(elem)
    }
  }

  const allCourses: SelectedLecture[] =
    JSON.parse(localStorage.getItem("selectedLectures") ?? "null") || []
  const courseData = allCourses.map(
    data => `${data.courseCode.split(" ")[0]}-${data.lectureCode}-${data.session}`
  )

  $.ajax({
    url: "/timetable-pdf",
    data: {
      courses: courseData.join("_"),
      JsonLocalStorageObj: JSON.stringify(necessaryLS),
    },
    success: function (data: string) {
      const dataURI = "data:application/pdf;base64," + data
      const downloadLink = document.createElement("a")
      downloadLink.href = dataURI
      downloadLink.download = "timetable.pdf"
      document.body.appendChild(downloadLink)
      downloadLink.click()
      document.body.removeChild(downloadLink)
    },
    error: function () {
      throw "No pdf generated"
    },
  })
}

function GraphImage({ data }: { data: string }) {
  return (
    <div>
      <div className="modal-header">Export</div>
      <div className="modal-body">
        <a onClick={getPDF} href="#">
          Download PDF
        </a>
        <div>{data && <img id="post-image" src={data} />}</div>
      </div>
    </div>
  )
}

function GridImage({
  data,
  toggleSession,
}: {
  data: string
  toggleSession: () => void
}) {
  return (
    <div>
      <div className="modal-header">Export</div>
      <div className="modal-body">
        <a onClick={getCalendar} href="#">
          Download timetable as ICS
        </a>
        <br />
        <a onClick={getPDF} href="#">
          Download PDF
        </a>
        <div>{data && <img id="post-image" src={data} />}</div>
        <button
          type="button"
          className="btn btn-primary"
          id="switch-session-button"
          onClick={toggleSession}
        >
          Switch Sessions
        </button>
      </div>
    </div>
  )
}
