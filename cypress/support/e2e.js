// Hide the disclaimer popup, which otherwise covers part of the graph and blocks interactions
Cypress.on("window:before:load", win => {
  win.localStorage.setItem("hide-disclaimer", "true")
})
