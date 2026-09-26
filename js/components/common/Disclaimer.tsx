import React, { useState } from "react"

/**
 * A React component representing the disclaimer popup
 */
export default function Disclaimer() {
  const [hidden, setHidden] = useState(
    localStorage.getItem("hide-disclaimer") === "true"
  )

  const handleClose = () => {
    setHidden(true)
  }

  const handleCheck = (event: React.MouseEvent<HTMLInputElement>) => {
    if (event.currentTarget.checked) {
      localStorage.setItem("hide-disclaimer", "true")
    } else {
      localStorage.setItem("hide-disclaimer", "false")
    }
  }

  if (hidden) {
    return null
  }

  const timetable = (
    <a href="https://timetable.iit.artsci.utoronto.ca/">Official Timetable</a>
  )
  const calendar = <a href="https://artsci.calendar.utoronto.ca/">Academic Calendar</a>

  return (
    <div className="popup-card">
      <button className="close-popup" onClick={handleClose}>
        &times;
      </button>
      <div className="popup-content">
        <h3>Disclaimer</h3>
        <p>
          Please make sure to confirm your course selections and prerequisites with
          official sources like the {timetable} and {calendar} as they are more reliable
          and up-to-date.{" "}
        </p>
        <button className="accept-popup" onClick={handleClose}>
          Understood
        </button>
        <label>
          <input
            type="checkbox"
            id="disclaimerCheck"
            className="dont-show-checkbox"
            onClick={handleCheck}
          />
          Do not show this again
        </label>
      </div>
    </div>
  )
}
