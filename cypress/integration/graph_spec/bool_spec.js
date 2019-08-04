describe("Boolean tests", () => {
  beforeEach(() => {
    cy.visit("/graph");
  });

  // TODO: make sure this works
  function hasDefaultBoolClasses(cyObject) {
    cyObject.should("have.classes", ["bool", "inactive"]);
  }
  it("should initially be 'bool' and 'inactive'", () => {
    cy.get(["data-testid=bool1"]).hasDefaultBoolClasses();
  });
  it("shouldn't do anything when you click or hover over it", () => {
    cy.get(["data-testid=bool1"]).hasDefaultBoolClasses();
    cy.get(["data-testid=bool1"]).click();
    cy.get(["data-testid=bool1"]).hasDefaultBoolClasses();
    cy.get(["data-testid=bool1"]).trigger("mouseover");
    cy.get(["data-testid=bool1"]).hasDefaultBoolClasses();
  });

  it("AND should be 'active' when prereq parents are met", () => {
    cy.get(["data-testid=bool1"]).contains("and");
    cy.get(["data-testid=csc258"]).click();
    cy.get(["data-testid=csc209"]).click();
    cy.get(["data-testid=bool1"]).should("have.class", "active");
    cy.get(["data-testid=csc367"]).should("have.class", "takeable");
  });
  it("OR should be 'active' when 1+ of the prereq parents are met", () => {
    ez;
    cy.get(["data-testid=bool2"]).contains("or");
    cy.get(["data-testid=csc373"]).click();
    cy.get(["data-testid=csc463"]).click();
    cy.get(["data-testid=bool2"]).should("have.class", "active");
    cy.get(["data-testid=csc438"]).should("have.class", "takeable");
  });
});
