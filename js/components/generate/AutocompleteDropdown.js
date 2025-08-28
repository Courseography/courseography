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
    fetch("/courses")
      .then(response => response.text())
      .then(data => {
        const courses = data.split("\n").map(course => course.substring(0, 8))
        setOptionList(courses)
      })
  }, [])

  return (
    <Autocomplete
      multiple
      onChange={(event, newValues) => {
        {
          onSelectedChange(newValues)
        }
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
  onSelectedChange: PropTypes.func,
}
