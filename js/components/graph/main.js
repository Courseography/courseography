import React from "react"
import ReactDOM from "react-dom"
import Container from "./Container"

// The "main"
document.addEventListener("DOMContentLoaded", () => {
  ReactDOM.render(
    <Container start_blank={false} edit={false} />,
    document.getElementById("container"),
  )
})
