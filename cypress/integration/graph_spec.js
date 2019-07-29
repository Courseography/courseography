describe("Visit website", function() {
  it("InfoBox Tests", function() {
    cy.visit("/");
    cy.contains("CSC108").trigger("mouseover");
    cy.contains("Info").click();

    cy.contains("CSC108");
    
    // force click because it's not visible
    cy.get(".ReactModal__Overlay").click({force: true});
  });
});
