describe("timesearch", () => {
  it("should be able to finds stuff", () => {
    cy.visit("/timesearch");
    cy.get("#deptSelect").select("CSC");
    cy.get("#codeFilter").type("CSC324");
    cy.get("#instFilter").type("Liu{enter}");
  });
});
