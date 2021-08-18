import PropTypes from "prop-types"
import React from "react"

// This now uses the new syntax for a stateless React component
// (component with only a render method).
export default function RegionGroup({ regionsJSON, labelsJSON }) {
  return (
    <g id="regions">
      {regionsJSON.map(function (entry, value) {
        var pathAttrs = { d: "M" }
        entry.points.forEach(function (x) {
          pathAttrs["d"] += x[0] + "," + x[1] + " "
        })

        var pathStyle = { fill: entry.fill }
        return <path {...pathAttrs} key={value} className="region" style={pathStyle} />
      })}
      {labelsJSON.map(function (entry, value) {
        var textAttrs = {
          x: entry.pos[0],
          y: entry.pos[1],
        }

        var textStyle = { fill: entry.fill }

        return (
          <text
            {...textAttrs}
            key={value}
            style={textStyle}
            className="region-label"
            textAnchor={entry["text-anchor"]}
          >
            {entry["text"]}
          </text>
        )
      })}
    </g>
  )
}

RegionGroup.propTypes = {
  labelsJSON: PropTypes.array,
  regionsJSON: PropTypes.array,
}
