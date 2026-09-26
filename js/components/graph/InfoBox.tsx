import React from "react"

interface InfoBoxProps {
  showInfoBox: boolean
  nodeId: string
  xPos: number
  yPos: number
  onClick: () => void
  onMouseEnter: () => void
  onMouseLeave: () => void
}

export default function InfoBox({
  showInfoBox,
  nodeId,
  xPos,
  yPos,
  onClick,
  onMouseEnter,
  onMouseLeave,
}: InfoBoxProps) {
  // guard against rendering with no course
  if (!nodeId) {
    return null
  }

  const className = showInfoBox ? "tooltip-group-display" : "tooltip-group-hidden"

  const rectAttrs = {
    id: nodeId + "-tooltip" + "-rect",
    x: xPos,
    y: yPos,
    rx: "4",
    ry: "4",
    fill: "white",
    stroke: "black",
    strokeWidth: "2",
    width: "60",
    height: "30",
  }

  const textAttrs = {
    id: nodeId + "-tooltip" + "-text",
    x: xPos + 60 / 2 - 18,
    y: yPos + 30 / 2 + 6,
  }

  return (
    <g
      id="infoBox"
      className={className}
      onClick={onClick}
      onMouseEnter={onMouseEnter}
      onMouseLeave={onMouseLeave}
    >
      <rect {...rectAttrs} />
      <text {...textAttrs}>Info</text>
    </g>
  )
}
