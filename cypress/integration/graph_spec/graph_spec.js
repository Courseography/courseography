describe("Graph", () => {
  beforeEach(() => {
    cy.visit("/graph");
  });
  function getByTestId(testId) {
    return cy.get([`data-testid=${testId}`]);
  }

  describe("Boolean tests", () => {
    function hasDefaultBoolClasses(cyObject) {
      cyObject.should("have.classes", ["bool", "inactive"]);
    }
    it("should initially be 'bool' and 'inactive'", () => {
      getByTestId("bool1").hasDefaultBoolClasses();
    });
    it("shouldn't do anything when you click or hover over it", () => {
      getByTestId("bool1").hasDefaultBoolClasses();
      getByTestId("bool1").click();
      getByTestId("bool1").hasDefaultBoolClasses();
      getByTestId("bool1").trigger("mouseover");
      getByTestId("bool1").hasDefaultBoolClasses();
    });

    it("AND should be 'active' when prereq parents are met", () => {
      getByTestId("bool1").contains("and");
      getByTestId("csc258").click();
      getByTestId("csc209").click();
      getByTestId("bool1").should("have.class", "active");
      getByTestId("csc367").should("have.class", "takeable");
    });
    it("OR should be 'active' when 1+ of the prereq parents are met", () => {ez
      getByTestId("bool2").contains("or");
      getByTestId("csc373").click();
      getByTestId("csc463").click();
      getByTestId("bool2").should("have.class", "active");
      getByTestId("csc438").should("have.class", "takeable");
    });
  });

  it("'missing' class when the mouse is hovering over a child class", () => {
    getByTestId("csc458").trigger("mouseover");
    getByTestId("csc458").should('have.class', 'missing');
    getByTestId('h47').should("have.class", 'missing');
    getByTestId('bool1').should("have.class", 'missing');
    getByTestId('csc209').should("have.class", 'missing');
    getByTestId('csc258').should("have.class", 'missing');
    getByTestId('csc108').should("have.class", 'missing');
    
    // unaffected, despite sharing the same prereqs as CSC458
    getByTestId('csc358').should("have.class", 'inactive');
  });

  describe("Info Box", () => {
    it("clicking the info box should produce a pop up modal", () => {
      cy.visit("/graph");
      cy.contains("CSC108").trigger("mouseover");
      cy.contains("Info").click();

      cy.contains("CSC108 Introduction to Computer Programming");
      // force click because it's not visible
      cy.get(".ReactModal__Overlay").click({ force: true });
    });
  });
});
