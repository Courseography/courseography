describe("Draw", () => {
  it("should be able to finds stuff", () => {
    cy.visit("/draw");
    cy.get('svg').click(200, 200);
  });
});
