import React from "react"
import { render, screen, cleanup } from "@testing-library/react"
import { Description } from "../react_modal.js.jsx"

describe("Displays correct content based on timetable availability", () => {
  beforeEach(() => cleanup())

  it("displays a timetable when there is only one session", () => {
    const courseInfo = {
      course: {
        name: "CSC108H1",
        description: "sample description",
        prereqStr: [],
        distribution: null,
        breadth: "The Physical and Mathematical Universes (5)",
      },
      sessions: {
        F: [
          {
            activity: "LEC0101",
            availability: "50 out of 100 available",
            instructor: "Beyonce",
            room: ["BA "],
            time: ["Monday  13 - 14", "Wednesday  13 - 14", "Friday  13 - 14"],
            waitlist: "0 students",
          },
        ],
        S: [],
        Y: [],
      },
    }

    render(<Description course={courseInfo.course} sessions={courseInfo.sessions} />)
    expect(screen.getByText(/CSC108H1/)).not.toBeNull()
    expect(screen.getByText(/LEC0101/)).not.toBeNull()
    expect(screen.getByText("sample description")).not.toBeNull()
    expect(screen.getByText("Beyonce")).not.toBeNull()
  })

  it("displays a timetable when there is more than one session", () => {
    const courseInfo = {
      course: {
        name: "CSC108H1",
        description: "sample description",
        prereqStr: [],
        distribution: null,
        breadth: "The Physical and Mathematical Universes (5)",
      },
      sessions: {
        F: [
          {
            activity: "LEC0101",
            availability: "50 out of 100 available",
            instructor: "Beyonce",
            room: ["BA "],
            time: ["Monday  13 - 14", "Wednesday  13 - 14", "Friday  13 - 14"],
            waitlist: "0 students",
          },
        ],
        S: [
          {
            activity: "LEC0202",
            availability: "100 out of 200 available",
            instructor: "David. Liu",
            room: ["BA "],
            time: ["Monday  13 - 14", "Wednesday  13 - 14", "Friday  13 - 14"],
            waitlist: "0 students",
          },
        ],
        Y: [],
      },
    }

    render(<Description course={courseInfo.course} sessions={courseInfo.sessions} />)
    expect(screen.getAllByText(/CSC108H1/)).not.toBeNull()
    expect(screen.getByText(/LEC0101/)).not.toBeNull()
    expect(screen.getByText(/LEC0202/)).not.toBeNull()
    expect(screen.getByText("David. Liu")).not.toBeNull()
  })

  it("displays reminder when there's no timetable information", () => {
    const courseInfo = {
      course: {
        name: "CSC108H1",
        description: "sample description",
        prereqStr: [],
        distribution: null,
        breadth: "The Physical and Mathematical Universes (5)",
      },
      sessions: {
        F: [],
        S: [],
        Y: [],
      },
    }

    render(<Description course={courseInfo.course} sessions={courseInfo.sessions} />)
    expect(screen.getByText("No timetable information available")).not.toBeNull()
  })
})
