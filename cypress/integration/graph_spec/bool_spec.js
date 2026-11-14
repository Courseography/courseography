describe("Boolean tests", () => {
  beforeEach(() => {
    cy.visit("/graph")
  })

  it("should initially be 'bool' and 'inactive'", () => {
    cy.get('[data-testid="and(csc209h1,csc258h1)"]')
      .should("have.class", "bool")
      .should("have.class", "inactive")
  })
  it("shouldn't do anything when you click or hover over it", () => {
    cy.get('[data-testid="and(csc209h1,csc258h1)"]')
      .should("have.class", "bool")
      .should("have.class", "inactive")
    cy.get('[data-testid="and(csc209h1,csc258h1)"]').click()
    cy.get('[data-testid="and(csc209h1,csc258h1)"]')
      .should("have.class", "bool")
      .should("have.class", "inactive")
    cy.get('[data-testid="and(csc209h1,csc258h1)"]').trigger("mouseover")
    cy.get('[data-testid="and(csc209h1,csc258h1)"]')
      .should("have.class", "bool")
      .should("have.class", "inactive")
  })

  it("AND should be 'active' when prereq parents are met", () => {
    cy.get('[data-testid="and(csc209h1,csc258h1)"]').contains("and")
    cy.get('[data-testid="csc258h1"]').click()
    cy.get('[data-testid="csc209h1"]').click()
    cy.get('[data-testid="and(csc209h1,csc258h1)"]').should("have.class", "active")
    cy.get('[data-testid="csc367h1"]').should("have.class", "takeable")
  })
  it("OR should be 'active' when 1+ of the prereq parents are met", () => {
    cy.get('[data-testid="and(csc373h1,csc463h1)"]').contains("or")
    cy.get('[data-testid="csc373h1"]').click()
    cy.get('[data-testid="csc463h1"]').click()
    cy.get('[data-testid="and(csc373h1,csc463h1)"]').should("have.class", "active")
    cy.get('[data-testid="csc438h1"]').should("have.class", "takeable")
  })
})
