import React from "react"
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome"
import { faTriangleExclamation } from "@fortawesome/free-solid-svg-icons"

interface GraphFallbackProps {
  error: Error
}

export default function GraphFallback({ error }: GraphFallbackProps) {
  return (
    <div className="error-boundary-container">
      <div className="error-boundary-box">
        <FontAwesomeIcon icon={faTriangleExclamation} className="error-svg" />
        <div className="error-boundary-text">
          Your graph has failed to render. Please reload this page or report this issue
          to David Liu at {""}
          <a className="graph-fallback-email" href="mailto:david@cs.toronto.edu">
            david@cs.toronto.edu
          </a>
          <p className="graph-fallback-error">Details: {error.message}</p>
        </div>
      </div>
    </div>
  )
}
