describe("Edge", () => {
  beforeEach(() => {
    cy.visit("/graph");
  });
  describe("Clicking course nodes", () => {
    it("unselected source and destination => 'inactive' Edge", () => {
      cy.get('[data-testid="csc108->csc148"]').should("have.class", "inactive");
    });
    it("with selected source and unselected destination => 'takeable' Edge", () => {
      cy.get('[data-testid="csc108"]').click();
      cy.get('[data-testid="csc108->csc148"]').should("have.class", "takeable");
    });

    it("with unselected source with selected destination => 'inactive' Edge", () => {
      cy.get('[data-testid="csc148"]').click();
      cy.get('[data-testid="csc108->csc148"]').should("have.class", "inactive");
    });

    it("with selected source and destination => 'active' Edge", () => {
      cy.get('[data-testid="csc108"]').click();
      cy.get('[data-testid="csc148"]').click();
      cy.get('[data-testid="csc108->csc148"]').should("have.class", "active");
    });
  });

  describe("hovering behaviour", () => {
    describe("hovering over source does nothing", () => {
      it("hovered and unselected source and unselected destination => stays 'inactive'", () => {
        cy.get('[data-testid="h62->csc343"]').should("have.class", "inactive");
        cy.get('[data-testid="csc111"]').trigger("mouseout");
        cy.get('[data-testid="h62->csc343"]').should("have.class", "inactive");
      });

      it("hovered and selected source and unselected destination => stays 'takeable'", () => {
        cy.get('[data-testid="csc111"]').click();
        cy.get('[data-testid="h62->csc343"]').should("have.class", "takeable");
        cy.get('[data-testid="csc111"]').trigger("mouseover");
        cy.get('[data-testid="h62->csc343"]').should("have.class", "takeable");
      });

      it("hovered and unselected source and selected destination => stays 'active'", () => {
        cy.get('[data-testid="csc343"]').click();
        cy.get('[data-testid="h62->csc343"]').should("have.class", "inactive");
        cy.get('[data-testid="csc111"]').trigger("mouseover");
        cy.get('[data-testid="h62->csc343"]').should("have.class", "inactive");
      });

      it("(hovered and selected source) and selected destination => stays 'active'", () => {
        cy.get('[data-testid="csc111"]').click();
        cy.get('[data-testid="csc343"]').click();
        cy.get('[data-testid="h62->csc343"]').should("have.class", "active");
        cy.get('[data-testid="csc111"]').trigger("mouseover");
        cy.get('[data-testid="h62->csc343"]').should("have.class", "active");
      });
    });
  });

  describe("hovering over destination", () => {
    it("unselected source and (unselected and hovered destination) => edge should transition from 'inactive' to 'missing'", () => {
      cy.get('[data-testid="h62->csc343"]').should("have.class", "inactive");
      cy.get('[data-testid="csc343"]').trigger("mouseover");
      cy.get('[data-testid="h62->csc343"]').should("have.class", "missing");
    });
    it("selected source and (unselected and hovered destination) => edge remains 'takeable'", () => {
      cy.get('[data-testid="csc111"]').click();
      cy.get('[data-testid="h62->csc343"]').should("have.class", "takeable");
      cy.get('[data-testid="csc343"]').trigger("mouseover");
      cy.get('[data-testid="h62->csc343"]').should("have.class", "takeable");
    });
    it("unselected source and (selected and hovered destination) => edge should transition to inactive 'missing'", () => {
      cy.get('[data-testid="csc343"]').click();
      cy.get('[data-testid="h62->csc343"]').should("have.class", "inactive");
      cy.get('[data-testid="csc343"]').trigger("mouseover");
      cy.get('[data-testid="h62->csc343"]').should("have.class", "missing");
    });
    it("unselected source and (selected and hovered destination) => edge and all unmet and inactive prereqs should be 'missing'", () => {
      // selected node with missing prereqs
      cy.get('[data-testid="csc443"]').click();

      cy.get('[data-testid="h62->csc343"]').should("have.class", "inactive");
      cy.get('[data-testid="csc343->csc443"]').should("have.class", "inactive");
      cy.get('[data-testid="csc207->csc343"]').should("have.class", "inactive");

      cy.get('[data-testid="csc443"]').trigger("mouseover");
      cy.get('[data-testid="h62->csc343"]').should("have.class", "missing");
      cy.get('[data-testid="csc343->csc443"]').should("have.class", "missing");
      cy.get('[data-testid="csc207->csc343"]').should("have.class", "missing");
    });

    it("selected source and selected destination, hovering over the destination will have the Edge remain 'active'", () => {
      cy.get('[data-testid="csc111"]').click();
      cy.get('[data-testid="csc343"]').click();
      cy.get('[data-testid="h62->csc343"]').should("have.class", "active");
      cy.get('[data-testid="csc343"]').trigger("mouseover");
      cy.get('[data-testid="h62->csc343"]').should("have.class", "active");
    });
  });
});
