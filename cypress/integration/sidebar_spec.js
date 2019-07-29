describe("Draw", () => {
  beforeEach("", () => {
    cy.visit("/graph");
  });
  it("Switch to Aboriginal Studies", () => {
    cy.get("#sidebar-button").click();
    cy.contains("Aboriginal").click();
    cy.get("#sidebar-button").click();
  });
  it("navigation buttons", () => {
    cy.get("#zoom-in-button")
      .click()
      .click();
    cy.get("#pan-right-button").click();
    cy.get("#zoom-out-button").click();
  });
});
