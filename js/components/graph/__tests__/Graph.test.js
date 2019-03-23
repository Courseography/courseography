import { cleanup, fireEvent } from "react-testing-library";
import setupGraph from './setupGraph';
 
afterEach(cleanup);

// FCEs are only displayed in the sidebar component and inside the state
describe("Graph", () => {
    it("A new graph should start with 0 FCEs", async () => {
        await setupGraph();
        expect(document.getElementById('fcecount').textContent).toBe('FCE Count: 0');
    })
    it("Pressing a course node should increase the FCE count by 0.5 if it's a half-year course", async () => {
        const graph = await setupGraph();
        const aaa100 = graph.getByText("AAA100");
        const aaa201 = graph.getByText("AAA201");

        fireEvent.click(aaa100);
        expect(document.getElementById('fcecount').textContent).toBe('FCE Count: 0.5');
        fireEvent.click(aaa201);
        expect(document.getElementById('fcecount').textContent).toBe('FCE Count: 1');
        fireEvent.click(aaa100);
        expect(document.getElementById('fcecount').textContent).toBe('FCE Count: 0.5');
        fireEvent.click(aaa201);
        expect(document.getElementById('fcecount').textContent).toBe('FCE Count: 0');
    });
    // TODO: March 11, 2019: there's no way to encode a 1.0 FCE and a 0.5 FCE course in the Graph
    // it("Pressing a course node should increase the FCE count by 1.0 if it's a full-year course", () => {});
    // Focuses are hard-coded in courseography/js/components/graph/sidebar/focus_descriptions
    // it('should highlighted all the class nodes of a focus if the focus is selected', () => {});
});
