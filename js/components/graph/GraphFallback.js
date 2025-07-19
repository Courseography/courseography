import React from "react"
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome"
import { faTriangleExclamation } from "@fortawesome/free-solid-svg-icons"
import PropTypes from "prop-types"

export default class GraphFallback extends React.Component {
  sendEmail() {
    const email = "david@cs.toronto.edu"
    window.location.href = `mailto:${email}`
  }

  render() {
    const { error } = this.props

    return (
      <div className="error-boundary-container">
        <div className="error-boundary-box">
          <div className="error-boundary-text">
            <FontAwesomeIcon icon={faTriangleExclamation} id="error-svg" />
            <p style={{ marginBottom: "0.5rem" }} id="graph-fallback">
              Your graph has failed to render. Please reload this page or report this
              issue to David Liu at {""}
              <span
                style={{ fontWeight: "bold", cursor: "pointer" }}
                onClick={() => this.sendEmail()}
                onMouseEnter={e => (e.target.style.textDecoration = "underline")}
                onMouseLeave={e => (e.target.style.textDecoration = "none")}
              >
                david@cs.toronto.edu
              </span>
            </p>
            <p style={{ fontSize: "20px" }}>Details: {error.message}</p>
          </div>
        </div>
      </div>
    )
  }
}

GraphFallback.propTypes = {
  error: PropTypes.object.isRequired,
}
