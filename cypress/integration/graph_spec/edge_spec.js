describe("Edge", () => {
  beforeEach(() => {
    cy.visit("/graph");
  });
  describe("Clicking course nodes", () => {
    it("unselected source and destination => 'inactive' Edge", () => {
      cy.get(["data-testId=csc108->csc148"]).should("have.class", "inactive");
    });
    it("with selected source and unselected destination => 'takeable' Edge", () => {
      cy.get(["data-testId=aaa101"]).click();
      cy.get(["data-testId=csc108->csc148"]).should("have.class", "takeable");
    });

    it("with unselected source with selected destination => 'inactive' Edge", () => {
      cy.get(["data-testId=csc148"]).click();
      cy.get(["data-testId=csc108->csc148"]).should("have.class", "inactive");
    });

    it("with selected source and destination => 'active' Edge", () => {
      cy.get(["data-testId=aaa101"]).click();
      cy.get(["data-testId=csc148"]).click();
      cy.get(["data-testId=csc108->csc148"]).should("have.class", "active");
    });
  });

  describe("hovering behaviour", () => {
    describe("hovering over source does nothing", () => {
      it("hovered and unselected source and unselected destination => stays 'inactive'", () => {
        cy.get(["data-testId=h101->aaa201"]).should("have.class", "inactive");
        cy.get(["data-testId=aaa101"]).trigger("mouseout");
        cy.get(["data-testId=h101->aaa201"]).should("have.class", "inactive");
      });

      it("hovered and selected source and unselected destination => stays 'takeable'", () => {
        cy.get(["data-testId=aaa101"]).click();
        cy.get(["data-testId=h101->aaa201"]).should("have.class", "takeable");
        cy.get(["data-testId=aaa101"]).trigger("mouseover");
        cy.get(["data-testId=h101->aaa201"]).should("have.class", "takeable");
      });

      it("hovered and unselected source and selected destination => stays 'active'", () => {
        cy.get(["data-testId=aaa201"]).click();
        cy.get(["data-testId=h101->aaa201"]).should("have.class", "inactive");
        cy.get(["data-testId=aaa101"]).trigger("mouseover");
        cy.get(["data-testId=h101->aaa201"]).should("have.class", "inactive");
      });

      it("(hovered and selected source) and selected destination => stays 'active'", () => {
        cy.get(["data-testId=aaa101"]).click();
        cy.get(["data-testId=aaa201"]).click();
        cy.get(["data-testId=h101->aaa201"]).should("have.class", "active");
        cy.get(["data-testId=aaa101"]).trigger("mouseover");
        cy.get(["data-testId=h101->aaa201"]).should("have.class", "active");
      });
    });
  });

  describe("hovering over destination", () => {
    it("unselected source and (unselected and hovered destination) => edge should transition from 'inactive' to 'missing'", () => {
      cy.get(["data-testId=h101->aaa201"]).should("have.class", "inactive");
      cy.get(["data-testId=aaa201"]).trigger("mouseover");
      cy.get(["data-testId=h101->aaa201"]).should("have.class", "missing");
    });
    it("selected source and (unselected and hovered destination) => edge remains 'takeable'", () => {
      cy.get(["data-testId=aaa101"]).click();
      cy.get(["data-testId=h101->aaa201"]).should("have.class", "takeable");
      cy.get(["data-testId=aaa201"]).trigger("mouseover");
      cy.get(["data-testId=h101->aaa201"]).should("have.class", "takeable");
    });
    it("unselected source and (selected and hovered destination) => edge should transition to inactive 'missing'", () => {
      cy.get(["data-testId=aaa201"]).click();
      cy.get(["data-testId=h101->aaa201"]).should("have.class", "inactive");
      cy.get(["data-testId=aaa201"]).trigger("mouseover");
      cy.get(["data-testId=h101->aaa201"]).should("have.class", "missing");
    });
    it("unselected source and (selected and hovered destination) => edge and all unmet and inactive prereqs should be 'missing'", () => {
      // selected node with missing prereqs
      cy.get(["data-testId=aaa303"]).click();

      cy.get(["data-testId=h101->aaa201"]).should("have.class", "inactive");
      cy.get(["data-testId=aaa201->bool1"]).should("have.class", "inactive");
      cy.get(["data-testId=aaa102->bool1"]).should("have.class", "inactive");
      cy.get(["data-testId=bool1->aaa303"]).should("have.class", "inactive");

      cy.get(["data-testId=aaa303"]).trigger("mouseover");
      cy.get(["data-testId=h101->aaa201"]).should("have.class", "missing");
      cy.get(["data-testId=aaa201->bool1"]).should("have.class", "missing");
      cy.get(["data-testId=aaa102->bool1"]).should("have.class", "missing");
      cy.get(["data-testId=bool1->aaa303"]).should("have.class", "missing");
    });

    it("selected source and selected destination, hovering over the destination will have the Edge remain 'active'", () => {
      cy.get(["data-testId=aaa101"]).click();
      cy.get(["data-testId=aaa201"]).click();
      cy.get(["data-testId=h101->aaa201"]).should("have.class", "active");
      cy.get(["data-testId=aaa201"]).trigger("mouseover");
      cy.get(["data-testId=h101->aaa201"]).should("have.class", "active");
    });
  });
});
