describe("Hybrid Node", () => {
  beforeEach(() => {
    cy.visit("/graph")

    // avoids cypress duplicates during trigger("mouseover")
    cy.get("body").trigger("mouseout")
  })
  it("should have the 'hybrid' CSS class", () => {
    cy.get('[data-testid="h(csc209h1)"]').should("have.class", "hybrid")
    cy.get('[data-testid="h(csc209h1)"]').should("have.class", "inactive")
  })
  it("shouldn't do anything when you hover or click it", () => {
    cy.get('[data-testid="h(csc209h1)"]').should("have.class", "inactive")
    cy.get('[data-testid="h(csc209h1)"]')
      .click()
      .should("have.class", "inactive")
      .trigger("mouseover")
      .should("have.class", "inactive")
      .trigger("mouseout")
      .should("have.class", "inactive")
  })
  it("should be 'inactive' when it's prereq parent is NOT met", () => {
    cy.get('[data-testid="h(csc209h1)"]').should("have.class", "inactive")
  })
  it("should be 'active' when its prereq parent is met", () => {
    cy.get('[data-testid="csc209h1"]').click()
    cy.get('[data-testid="h(csc209h1)"]').should("have.class", "active")
  })

  it("should be 'missing' if not 'active' and it's an unmet prereq of the currently hovered course", () => {
    cy.get('[data-testid="csc456h1"]').trigger("mouseover")
    cy.get('[data-testid="h(csc209h1)"]').should("have.class", "missing")
    cy.get('[data-testid="csc456h1"]').trigger("mouseout")
  })
})

describe("Course Node", () => {
  beforeEach(() => {
    cy.visit("/graph")
  })

  it("should have the CSS class: 'node'", () => {
    cy.get('[data-testid="csc108h1"]').should("have.class", "node")
  })
  describe("Unselected Course Node", () => {
    it("should be 'takeable' if it has no prereqs", () => {
      cy.get('[data-testid="csc108h1"]').should("have.class", "takeable")
    })
    it("should be 'inactive' if the prereqs are NOT met", () => {
      cy.get('[data-testid="csc148h1"]').should("have.class", "inactive")
    })
    it("should be 'takeable' if the prereqs are met", () => {
      cy.get('[data-testid="csc108h1"]').click()
      cy.get('[data-testid="csc148h1"]').should("have.class", "takeable")

      // cleanup
      cy.get('[data-testid="csc108h1"]').click()
    })
    it('should be "missing" when unselected and hovered over', () => {
      cy.get('[data-testid="csc108h1"]').trigger("mouseover")
      cy.get('[data-testid="csc108h1"]').should("have.class", "missing")
    })

    it("when hovered, should set all unmet prereqs and itself as 'missing'", () => {
      cy.get('[data-testid="csc207h1"]').trigger("mouseover")
      cy.get('[data-testid="csc148h1"]').should("have.class", "missing")
      cy.get('[data-testid="csc108h1"]').should("have.class", "missing")
    })
  })

  describe("Selected Course Node", () => {
    it("with met prereqs should 'active'", () => {
      cy.get('[data-testid="csc108h1"]').click()
      cy.get('[data-testid="csc108h1"]').should("have.class", "active")

      // cleanup
      cy.get('[data-testid="csc108h1"]').click()
    })
    describe("selected course with un-met prereqs", () => {
      it("should be 'overridden' (if you don't hover over it)", () => {
        cy.get('[data-testid="csc148h1"]').click()
        cy.get('[data-testid="csc148h1"]').should("have.class", "overridden")

        // cleanup
        cy.get('[data-testid="csc148h1"]').click()
      })

      it("'overridden' course counts as a satisfied prereq (acts like 'active')", () => {
        cy.get('[data-testid="csc148h1"]').click()
        cy.get('[data-testid="csc148h1"]').should("have.class", "overridden")
        cy.get('[data-testid="csc207h1"]').should("have.class", "takeable")
      })

      // cy.click() leaves the mouse over the clicked node, so trigger "mouseout" after
      // clicking to avoid that node's hover state affecting the assertions.
      it("hovering over a course with an 'overridden' prereq does not mark the 'overridden' unmet prereqs as 'missing'", () => {
        cy.get('[data-testid="csc148h1"]').click().trigger("mouseout")
        cy.get('[data-testid="csc207h1"]').trigger("mouseover")
        // not missing because csc148h1 is overridden
        cy.get('[data-testid="csc108h1"]').should("have.class", "takeable")
      })

      it("hovering over the child of an 'active' course will ignore any unmet ancestor courses", () => {
        cy.get('[data-testid="csc148h1"]').click().trigger("mouseout")
        cy.get('[data-testid="csc207h1"]').click().trigger("mouseout")
        cy.get('[data-testid="csc207h1"]').trigger("mouseover")
        cy.get('[data-testid="csc108h1"]').should("have.class", "takeable")
      })
      it("'missing' class when the mouse is hovering over a child class", () => {
        cy.get('[data-testid="csc458h1"]')
          .trigger("mouseover")
          .should("have.class", "missing")
        cy.get('[data-testid="h(csc263h1,csc265h1)"]').should("have.class", "missing")
        cy.get('[data-testid="and(csc209h1,csc258h1)"]').should("have.class", "missing")
        cy.get('[data-testid="csc209h1"]').should("have.class", "missing")
        cy.get('[data-testid="csc258h1"]').should("have.class", "missing")
        cy.get('[data-testid="csc108h1"]').should("have.class", "missing")

        // unaffected, despite sharing the same prereqs as CSC458
        cy.get('[data-testid="csc385h1"]').should("have.class", "inactive")

        // cleanup
        cy.get('[data-testid="csc148h1"]').click()
      })
      it("selected + hovered over + unmet prereqs: 'missing'", () => {
        cy.get('[data-testid="csc324h1"]').click()
        cy.get('[data-testid="csc324h1"]').should("have.class", "overridden")

        cy.get('[data-testid="csc324h1"]').trigger("mouseover")
        cy.get('[data-testid="csc324h1"]').should("have.class", "missing")

        // cleanup
        cy.get('[data-testid="csc324h1"]').click()
      })
    })
  })
})
