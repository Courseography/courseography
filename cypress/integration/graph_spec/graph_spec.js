describe("Graph", () => {
  beforeEach(() => {
    cy.visit("/graph")
  })

  describe("Boolean tests", () => {
    it("should initially be 'bool' and 'inactive'", () => {
      cy.get(`[data-testid="and(csc209,csc258)"]`)
        .should("have.class", "bool")
        .should("have.class", "inactive")
    })
    it("shouldn't do anything when you click or hover over it", () => {
      cy.get(`[data-testid="and(csc209,csc258)"]`)
        .should("have.class", "bool")
        .should("have.class", "inactive")
      cy.get(`[data-testid="and(csc209,csc258)"]`).click()
      cy.get(`[data-testid="and(csc209,csc258)"]`)
        .should("have.class", "bool")
        .should("have.class", "inactive")
      cy.get(`[data-testid="and(csc209,csc258)"]`).trigger("mouseover")
      cy.get(`[data-testid="and(csc209,csc258)"]`)
        .should("have.class", "bool")
        .should("have.class", "inactive")
    })

    it("AND should be 'active' when prereq parents are met", () => {
      cy.get(`[data-testid="and(csc209,csc258)"]`).contains("and")
      cy.get(`[data-testid="csc258"]`).click()
      cy.get(`[data-testid="csc209"]`).click()
      cy.get(`[data-testid="and(csc209,csc258)"]`).should("have.class", "active")
      cy.get(`[data-testid="csc367"]`).should("have.class", "takeable")
    })
    it("OR should be 'active' when 1+ of the prereq parents are met", () => {
      cy.get(`[data-testid="and(csc463,csc373)"]`).contains("or")
      cy.get(`[data-testid="csc373"]`).click()
      cy.get(`[data-testid="csc463"]`).click()
      cy.get(`[data-testid="and(csc463,csc373)"]`).should("have.class", "active")
      cy.get(`[data-testid="csc438"]`).should("have.class", "takeable")
    })
  })

  it("'missing' class when the mouse is hovering over a child class", () => {
    cy.get(`[data-testid="csc458"]`).trigger("mouseover")
    cy.get(`[data-testid="csc458"]`).should("have.class", "missing")
    cy.get(`[data-testid='h(csc263265)']`).should("have.class", "missing")
    cy.get(`[data-testid='and(csc209,csc258)']`).should("have.class", "missing")
    cy.get(`[data-testid='csc209']`).should("have.class", "missing")
    cy.get(`[data-testid='csc258']`).should("have.class", "missing")
    cy.get(`[data-testid='csc108']`).should("have.class", "missing")

    // unaffected, despite sharing the same prereqs as CSC458
    cy.get(`[data-testid='csc385']`).should("have.class", "inactive")
  })

  describe("Info Box", () => {
    it("clicking the info box should produce a pop up modal", () => {
      cy.visit("/graph")
      cy.contains("CSC108").trigger("mouseover")
      cy.contains("Info").click()

      cy.contains("CSC108 Introduction to Computer Programming")
      // force click because it's not visible
      cy.get(".ReactModal__Overlay").click({ force: true })
    })
  })
})
