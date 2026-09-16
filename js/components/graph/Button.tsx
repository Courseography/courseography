import React from "react"

interface ButtonProps {
  divId?: string
  mouseDown?: () => void
  mouseUp?: () => void
  onMouseEnter?: () => void
  onMouseLeave?: () => void
  disabled?: boolean
  text?: string
  children?: React.ReactNode
}

export default function Button({
  divId,
  mouseDown,
  mouseUp,
  onMouseEnter,
  onMouseLeave,
  disabled,
  text,
  children,
}: ButtonProps) {
  return (
    <button
      id={divId}
      className="graph-control-button"
      onMouseDown={mouseDown}
      onMouseUp={mouseUp}
      onMouseEnter={onMouseEnter}
      onMouseLeave={onMouseLeave}
      disabled={disabled}
    >
      {text}
      {children}
    </button>
  )
}
