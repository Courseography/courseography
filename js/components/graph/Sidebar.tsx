import { useState } from "react"
import Button from "./Button"

type Course = [courseId: string, courseLabel: string]

interface SidebarProps {
  fceCount: number
  reset?: () => void
  activeCourses: Set<string>
  courses: Course[]
  courseClick?: (courseId: string | null) => void
  xClick?: (courseId: string | null) => void
  sidebarItemClick?: () => void
  onHover?: () => void
  onMouseLeave?: () => void
}

export default function Sidebar({
  fceCount,
  reset,
  activeCourses,
  courses,
  courseClick,
  xClick,
  sidebarItemClick,
  onHover,
  onMouseLeave,
}: SidebarProps) {
  const [collapsed, setCollapsed] = useState(true)
  const [results, setResults] = useState<Course[] | undefined>([])

  const toggleSidebar = () => {
    setCollapsed(!collapsed)
  }

  const filteredSearch = (query: string) => {
    if (!query || !courses) {
      return
    }

    return courses
      .filter(([courseId, courseLabel]) => {
        return (
          courseId.includes(query) ||
          courseId.toUpperCase().includes(query) ||
          courseLabel.includes(query.toUpperCase())
        )
      })
      .sort()
  }

  /**
   * Given a course label return the id of the corresponding Node component.
   * If an invalid course label is supplied, `null` will be returned.
   * e.g. "CSC263/265" will resolve to "csc263265"
   *      "CSC999" will resolve to `null`
   * @return course node id
   */
  const courseIdFromLabel = (courseLabel: string) => {
    for (let i = 0; i < courses.length; i++) {
      if (courses[i][1] === courseLabel) {
        return courses[i][0]
      }
    }
    return null
  }

  // Sidebar rendering methods
  /**
   * Render the FCE counter above the sidebar on the left side.
   * @return FCE to the DOM
   */
  const renderFCE = () => {
    const fceString = Number.isInteger(fceCount) ? fceCount + ".0" : fceCount

    return (
      <div className="fcecount" data-testid="test-fcecount">
        FCE Count: {fceString}
      </div>
    )
  }

  /**
   * Render the dropdown results within the sidebar dropdown.
   * @return Searchbar to the DOM
   */
  const renderDropdown = () => {
    if (courses) {
      const showDropdown = results ? "" : "hidden"
      const masterDropdown = `${showDropdown} search-dropdown`
      return (
        <ul className={masterDropdown} data-testid="test-searchDropdown">
          {results?.map(([resultId, resultLabel]) => (
            <li
              aria-label="test-li"
              key={`search ${resultId}`}
              className="dropdown-item"
              onClick={sidebarItemClick}
              data-node-id={courseIdFromLabel(resultLabel)}
              onMouseEnter={onHover}
              onMouseLeave={onMouseLeave}
            >
              {resultLabel}
            </li>
          ))}
        </ul>
      )
    }
  }

  /**
   * Render courses that are in the sidebar.
   * @return list of div's for each course that is active
   */
  const renderActiveCourses = () => {
    const temp = activeCourses ? [...activeCourses] : []
    // sort the list of rendered courses, alphabetically
    temp.sort((a, b) => a.localeCompare(b))
    return (
      <div className="courses" data-testid="test-course-selection">
        {temp.map(course => {
          return (
            <div
              key={`active ${course}`}
              data-testid={`test ${course}`}
              onClick={() => {
                courseClick?.(courseIdFromLabel(course))
              }}
              className="course-selection"
              onMouseEnter={onHover}
              onMouseLeave={onMouseLeave}
            >
              {course}
              <Button text="X" mouseDown={() => xClick?.(courseIdFromLabel(course))} />
            </div>
          )
        })}
      </div>
    )
  }

  const collapsedClass = collapsed ? "collapsed" : "expanded"
  const masterSidebarClass = `${collapsedClass} sidebar`

  return (
    <div
      className={masterSidebarClass}
      data-testid="test-toggle"
      onWheel={e => e.stopPropagation()}
    >
      {renderFCE()}
      <div className="sidebar-dropdown" data-testid="test-sidebar">
        <div>
          <label htmlFor="header-search">
            {/* For text to speech purposes */}
            <span className="label-hidden">Search courses</span>
          </label>
          <input
            id="header-search"
            className="search-bar"
            data-testid="test-search-bar"
            type="text"
            onChange={e => {
              setResults(filteredSearch(e.target.value))
            }}
          />
        </div>
        {renderDropdown()}
        <h3 className="selected-courses">Selected courses</h3>
        {renderActiveCourses()}
        <button
          className="reset-selections"
          data-testid="test-reset"
          onClick={() => reset?.()}
        >
          Reset Selections
        </button>
      </div>
      <div
        className="sidebar-button"
        onClick={() => toggleSidebar()}
        data-testid="test-sidebar-button"
      >
        <img id="sidebar-icon" src="/static/res/ico/sidebar.png" />
      </div>
    </div>
  )
}
