import React, { useEffect, useState } from "react"
import Autocomplete from "@mui/material/Autocomplete"
import Chip from "@mui/material/Chip"
// import Typography from '@mui/material/Typography';
import { useField } from "formik"
import PropTypes from "prop-types"
//import { createTheme, ThemeProvider } from '@mui/material/styles';

// const theme = createTheme({
//   components: {
//     MuiChip: {
//       styleOverrides: {
//         label: {
//           fontFamily: 'Trebuchet MS',
//           color: '#76609c'
//         }
//       }
//     }
//   }
// });

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
    // <ThemeProvider theme={theme} >
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
      // I also did a hail mary and tried adding what is on line 72 here as well and obviously it didn't work.
      renderValue={(value, getItemProps) =>
        value.map((option, index) => {
          // I completely removed ...itemProps as it has a className element and
          const { key, ...itemProps } = getItemProps({ index })
          return (
            <Chip
              variant="outlined"
              //label={<Typography variant="caption"
              //sx={{ fontFamily: 'Trebuchet MS', color: '#76609c', fontSize: '12px'}}>
              //{option} </Typography>}
              label={option}
              key={key}
              {...itemProps}
              // className="autocomplete-input" just wanted the same attributes was going to pick a more suiting name for the className as a whole
              // sx={{ 'fontFamily': 'Trebuchet MS'}}
              // sx={{ '& .MuiChip-label': {fontFamily: 'Trebuchet MS',}}} // https://mui.com/material-ui/react-chip/
              // I also tried with style={{ fontFamily: 'Trebuchet MS'}} again obviously didn't work
            />
          )
        })
      }
      renderInput={params => (
        <div ref={params.InputProps.ref}>
          <input
            aria-label={name}
            className="autocomplete-input"
            type="text"
            {...params.inputProps}
            placeholder={placeholder}
            id={id}
          />
        </div>
      )}
      {...props}
    />
    // </ThemeProvider>
  )
}

AutocompleteDropdown.propTypes = {
  name: PropTypes.string,
  placeholder: PropTypes.string,
  id: PropTypes.string,
  onSelectedChange: PropTypes.func,
}
