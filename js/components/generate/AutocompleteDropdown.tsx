import React, { useEffect, useState } from "react"
import Autocomplete, { AutocompleteProps } from "@mui/material/Autocomplete"
import Chip from "@mui/material/Chip"
import { useField } from "formik"

interface AutocompleteDropdownOwnProps {
  name: string
  placeholder?: string
  id?: string
  onSelectedChange: (newValues: string[]) => void
}

type AutocompleteDropdownProps = AutocompleteDropdownOwnProps &
  Partial<
    Omit<
      AutocompleteProps<string, true, true, false>,
      keyof AutocompleteDropdownOwnProps | "options" | "onChange" | "renderInput"
    >
  >

export default function AutocompleteDropdown({
  name,
  placeholder,
  id,
  onSelectedChange,
  ...props
}: AutocompleteDropdownProps) {
  const [, , helpers] = useField<string>(name)
  const { setValue } = helpers
  const [optionList, setOptionList] = useState<string[]>([])

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
