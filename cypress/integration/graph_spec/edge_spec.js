// Edges out of hybrid nodes use the hybrid's database-generated id (e.g. "h3586->csc456h1"),
// which isn't stable across databases, so look it up from the hybrid node at runtime.
const hybridEdge = (hybridTestId, target) =>
  cy
    .get(`[data-testid="${hybridTestId}"]`)
    .invoke("attr", "id")
    .then(id => cy.get(`[data-testid="${id}->${target}"]`))

// h(csc209h1) is the hybrid node for CSC209H1, a prereq of CSC456H1
const SOURCE = "csc209h1"
const HYBRID = "h(csc209h1)"
const DEST = "csc456h1"
const edge = () => hybridEdge(HYBRID, DEST)

describe("Edge", () => {
  beforeEach(() => {
    cy.visit("/graph")
  })
  describe("Clicking course nodes", () => {
    it("unselected source and destination => 'inactive' Edge", () => {
      cy.get('[data-testid="csc108h1->csc148h1"]').should("have.class", "inactive")
    })
    it("with selected source and unselected destination => 'takeable' Edge", () => {
      cy.get('[data-testid="csc108h1"]').click()
      cy.get('[data-testid="csc108h1->csc148h1"]').should("have.class", "takeable")
    })

    it("with unselected source with selected destination => 'inactive' Edge", () => {
      cy.get('[data-testid="csc148h1"]').click()
      cy.get('[data-testid="csc108h1->csc148h1"]').should("have.class", "inactive")
    })

    it("with selected source and destination => 'active' Edge", () => {
      cy.get('[data-testid="csc108h1"]').click()
      cy.get('[data-testid="csc148h1"]').click()
      cy.get('[data-testid="csc108h1->csc148h1"]').should("have.class", "active")
    })
  })

  describe("hovering behaviour", () => {
    describe("hovering over source does nothing", () => {
      it("hovered and unselected source and unselected destination => stays 'inactive'", () => {
        edge().should("have.class", "inactive")
        cy.get(`[data-testid="${SOURCE}"]`).trigger("mouseout")
        edge().should("have.class", "inactive")
      })

      it("hovered and selected source and unselected destination => stays 'takeable'", () => {
        cy.get(`[data-testid="${SOURCE}"]`).click()
        edge().should("have.class", "takeable")
        cy.get(`[data-testid="${SOURCE}"]`).trigger("mouseover")
        edge().should("have.class", "takeable")
      })

      it("hovered and unselected source and selected destination => stays 'active'", () => {
        cy.get(`[data-testid="${DEST}"]`).click()
        edge().should("have.class", "inactive")
        cy.get(`[data-testid="${SOURCE}"]`).trigger("mouseover")
        edge().should("have.class", "inactive")
      })

      it("(hovered and selected source) and selected destination => stays 'active'", () => {
        cy.get(`[data-testid="${SOURCE}"]`).click()
        cy.get(`[data-testid="${DEST}"]`).click()
        edge().should("have.class", "active")
        cy.get(`[data-testid="${SOURCE}"]`).trigger("mouseover")
        edge().should("have.class", "active")
      })
    })
  })

  describe("hovering over destination", () => {
    it("unselected source and (unselected and hovered destination) => edge should transition from 'inactive' to 'missing'", () => {
      edge().should("have.class", "inactive")
      cy.get(`[data-testid="${DEST}"]`).trigger("mouseover")
      edge().should("have.class", "missing")
    })
    it("selected source and (unselected and hovered destination) => edge remains 'takeable'", () => {
      cy.get(`[data-testid="${SOURCE}"]`).click()
      edge().should("have.class", "takeable")
      cy.get(`[data-testid="${DEST}"]`).trigger("mouseover")
      edge().should("have.class", "takeable")
    })
    it("unselected source and (selected and hovered destination) => edge should transition to inactive 'missing'", () => {
      cy.get(`[data-testid="${DEST}"]`).click()
      edge().should("have.class", "inactive")
      cy.get(`[data-testid="${DEST}"]`).trigger("mouseover")
      edge().should("have.class", "missing")
    })
    it("unselected source and (selected and hovered destination) => edge and all unmet and inactive prereqs should be 'missing'", () => {
      // selected node with missing prereqs: CSC443H1 <- CSC209H1 <- CSC207H1
      cy.get('[data-testid="csc443h1"]').click().trigger("mouseout")

      cy.get('[data-testid="csc209h1->csc443h1"]').should("have.class", "inactive")
      cy.get('[data-testid="csc207h1->csc209h1"]').should("have.class", "inactive")

      cy.get('[data-testid="csc443h1"]').trigger("mouseover")
      cy.get('[data-testid="csc209h1->csc443h1"]').should("have.class", "missing")
      cy.get('[data-testid="csc207h1->csc209h1"]').should("have.class", "missing")
    })

    it("selected source and selected destination, hovering over the destination will have the Edge remain 'active'", () => {
      cy.get(`[data-testid="${SOURCE}"]`).click()
      cy.get(`[data-testid="${DEST}"]`).click()
      edge().should("have.class", "active")
      cy.get(`[data-testid="${DEST}"]`).trigger("mouseover")
      edge().should("have.class", "active")
    })
  })
})
