import React, { useEffect, useState } from "react"
import Autocomplete from "@mui/material/Autocomplete"
import Chip from "@mui/material/Chip"
import { useField } from "formik"
import PropTypes from "prop-types"

export default function AutocompleteDropdown({
  name,
  placeholder,
  id,
  onSelectedChange,
  ...props
}) {
  const [, , helpers] = useField(name)
  const { setValue } = helpers
  const [optionList, setOptionList] = useState([])

  useEffect(() => {
    if (id == "courses") {
      fetch("/courses")
        .then(response => response.text())
        .then(data => {
          const courses = data.split("\n").map(course => course.substring(0, 8))
          setOptionList(courses)
        })
    } else if (id == "programs") {
      fetch("/programs")
        .then(response => response.text())
        .then(data => {
          const programs = data.split("\n")
          setOptionList(programs)
        })
    }
  }, [])

  return (
    <Autocomplete
      multiple
      onChange={(event, newValues) => {
        onSelectedChange(newValues)
        setValue(newValues.join(", "))
      }}
      options={optionList}
      includeInputInList
      disableClearable
      disableCloseOnSelect
      popupIcon={null}
      sx={{ width: "100%" }}
      renderValue={(value, getItemProps) =>
        value.map((option, index) => {
          const { key, ...itemProps } = getItemProps({ index })
          return <Chip variant="outlined" label={option} key={key} {...itemProps} />
        })
      }
      renderInput={params => (
        <div ref={params.InputProps.ref}>
          <input
            aria-label={name}
            type="text"
            {...params.inputProps}
            placeholder={placeholder}
            id={id}
          />
        </div>
      )}
      renderOption={(props, option) => (
        <li
          {...props}
          style={{
            fontFamily: '"Trebuchet MS", Arial, sans-serif',
            color: "#5c497e",
          }}
        >
          {option}
        </li>
      )}
      {...props}
    />
  )
}

AutocompleteDropdown.propTypes = {
  name: PropTypes.string,
  placeholder: PropTypes.string,
  id: PropTypes.string,
  onSelectedChange: PropTypes.func,
}
