describe("Visit website", function() {
    it("Find Courses", function() {
      cy.visit("localhost:8000/grid");
      cy.get("#course-filter");
    });
  });
