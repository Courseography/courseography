import { cleanup} from "react-testing-library";
 
afterEach(cleanup);

// FCEs are only displayed in the sidebar component and inside the state
describe("Graph", () => {
    it("Pressing a course node should increase the FCE count by 0.5 if it's a half-year course", () => {
        // TODO: we could use enzyme's render() and see if clicking a node increases or decreases the FCECount in graph.state
    });
    it.skip("Pressing a course node should increase the FCE count by 1.0 if it's a full-year course", () => {
        // March 11, 2019: there's no way to encode a 1.0 FCE and a 0.5 FCE course in the Graph
    });

    it('should highlighted all the class nodes of a focus if the focus is selected', () => {});
    it('zooming increases the size of the nodes and zooming out decreases the size of the nodes', () => {});
});
