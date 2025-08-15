import React, { useEffect, useState } from "react"
import Autocomplete from "@mui/material/Autocomplete"
import { useField } from "formik"
import PropTypes from "prop-types"

export default function AutocompleteDropdown({ name, placeholder, id, ...props }) {
  const [field, helpers] = useField(name)
  const { setValue, setTouched } = helpers
  const [courseList, setCourseList] = useState([])

  useEffect(() => {
    fetch("/courses")
      .then(response => response.text())
      .then(data => {
        const depts = ["CSC", "MAT", "STA"]
        const courses = data
          .split("\n")
          .map(course => course.substring(0, 8))
          .filter(course => depts.some(prefix => course.startsWith(prefix)))
        setCourseList(courses)
      })
  }, [])

  return (
    <Autocomplete
      freeSolo
      options={courseList}
      value={field.value || ""}
      onChange={(event, value) => setValue(value)}
      onInputChange={(event, newInputValue) => setValue(newInputValue)}
      onBlur={() => setTouched(true)}
      includeInputInList
      disableClearable
      popupIcon={null}
      sx={{ width: "100%" }}
      renderInput={params => (
        <div ref={params.InputProps.ref}>
          <input
            color="#76609c"
            type="text"
            {...params.inputProps}
            placeholder={placeholder}
            id={id}
          />
        </div>
      )}
      {...props}
    />
  )
}

AutocompleteDropdown.propTypes = {
  name: PropTypes.string,
  placeholder: PropTypes.string,
  id: PropTypes.string,
}
