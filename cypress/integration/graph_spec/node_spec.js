describe("Hybrid Node", () => {
  it("should have the 'hybrid' CSS class", () => {
    cy.get("[data-testId=h48").should("have.class", "hybrid");
    cy.get("[data-testId=h48").should("have.class", "inactive");
  });
  it("shouldn't do anything when you hover or click it", () => {
    cy.get("[data-testId=h48").should("have.class", "inactive");
    // TODO: click, mouseover, mouseout on h48
  });
  it("should be 'inactive' when it's prereq parent is NOT met", () => {
    cy.get("[data-testId=h48").should("have.class", "inactive");
  });
  it("should be 'active' when its prereq parent is met", () => {
    cy.get("[data-testId=csc324]").click();
    cy.get("[data-testId=h48").should("have.class", "active");
  });

  it("should be 'missing' if not 'active' and it's an unmet prereq of the currently hovered course", () => {
    cy.get("[data-testid=csc488]").trigger("mouseover");
    cy.get("[data-testId=h48").should("have.class", "missing");
  });
});

describe("Course Node", () => {
  it("should have the CSS class: 'node'", () => {
    cy.get("[data-testId=csc108]").should("have.class", "node");
  });
  describe("Unselected Course Node", () => {
    it("should be 'takeable' if it has no prereqs", () => {
      cy.get("[data-testId=csc108]").should("have.class", "takeable");
    });
    it("should be 'inactive' if the prereqs are NOT met", () => {
      cy.get("[data-testId=csc148]").should("have.class", "inactive");
    });
    it("should be 'takeable' if the prereqs are met", () => {
      cy.get("[data-testId=csc108]").click();
      cy.get("[data-testId=csc148]").should("have.class", "takeable");
    });
    it('should be "missing" when unselected and hovered over', () => {
      cy.get("[data-testId=csc108]").trigger("mouseover");
      cy.get("[data-testId=csc108]").should("have.class", "missing");
    });

    it("when hovered, should set all unmet prereqs and itself as 'missing'", () => {
      cy.get("[data-testId=csc207]").trigger("mouseover");
      cy.get("[data-testId=csc148]").should("have.class", "missing");
      cy.get("[data-testId=csc108]").should("have.class", "missing");
    });
  });

  describe("Selected Course Node", () => {
    it("with met prereqs should 'active'", () => {
      cy.get("[data-testId=csc108]").click();
      cy.get("[data-testId=csc108]").should("have.class", "active");
    });
    describe("selected course with un-met prereqs", () => {
      it("should be 'overridden' (if you don't hover over it)", () => {
        cy.get("[data-testId=csc148]").click();
        cy.get("[data-testId=csc148]").should("have.class", "overridden");
      });

      it("'overridden' course counts as a satisfied prereq (acts like 'active')", () => {
        cy.get("[data-testId=csc148]").click();
        cy.get("[data-testId=csc148]").should("have.class", "overridden");
        cy.get("[data-testId=csc207]").should("have.class", "takeable");
      });

      it("hovering over a course with an 'overridden' prereq does not mark the 'overridden' unmet prereqs as 'missing'", () => {
        cy.get("[data-testId=csc148]").click();
        cy.get("[data-testId=csc207]").trigger("mouseover");
        cy.get("[data-testId=csc108]").should("have.class", "takeable");
      });
      it("'missing' class when the mouse is hovering over a child class", () => {
        cy.get(["data-testid=csc458"]).trigger("mouseover");
        cy.get(["data-testid=csc458"]).should("have.class", "missing");
        cy.get(["data-testid=h47"]).should("have.class", "missing");
        cy.get(["data-testid=bool1"]).should("have.class", "missing");
        cy.get(["data-testid=csc209"]).should("have.class", "missing");
        cy.get(["data-testid=csc258"]).should("have.class", "missing");
        cy.get(["data-testid=csc108"]).should("have.class", "missing");

        // unaffected, despite sharing the same prereqs as CSC458
        cy.get(["data-testid=csc358"]).should("have.class", "inactive");
      });
      it("selected + hovered over + unmet prereqs: 'missing'", () => {
        cy.get("[data-testId=csc324]").click();
        cy.get("[data-testId=csc324]").should("have.class", "overridden");

        cy.get("[data-testId=csc324]").trigger("mouseover");
        cy.get("[data-testId=csc324]").should("have.class", "missing");
      });
    });
  });
});
