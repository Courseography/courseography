import PropTypes from "prop-types"

export default function InfoBox({
  showInfoBox,
  nodeId,
  xPos,
  yPos,
  onClick,
  onMouseEnter,
  onMouseLeave,
}) {
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

InfoBox.propTypes = {
  showInfoBox: PropTypes.bool,
  nodeId: PropTypes.string,
  xPos: PropTypes.number,
  yPos: PropTypes.number,
  onClick: PropTypes.func,
  onMouseEnter: PropTypes.func,
  onMouseLeave: PropTypes.func,
}
