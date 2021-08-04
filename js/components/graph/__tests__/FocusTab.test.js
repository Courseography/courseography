import { fireEvent } from "@testing-library/react";
import TestContainer from "./TestContainer";

describe("FocusTab", () => {
  it("When focuses are clicked, focuses should be active", async () => {
    const container = await TestContainer.build();
    fireEvent.click(container.getByRole("button", {name: /FOCUSES ⪢/i}));
    let focusDiv = container.getByRole("button", {name: /scientific computing/i}).closest("div");
    expect(focusDiv.classList.contains("active-focus")).toBe(false);
    fireEvent.click(container.getByRole("button", {name: /scientific computing/i}));
    expect(focusDiv.classList.contains("active-focus")).toBe(true);
  });

  it("When focuses are clicked, the graph should be highlighted", async () => {
    const container = await TestContainer.build();
    expect(container.getByTestId("react-graph").classList.contains("highlight-nodes")).toBe(false);
    fireEvent.click(container.getByRole("button", {name: /FOCUSES ⪢/i}));
    fireEvent.click(container.getByRole("button", {name: /scientific computing/i}));
    expect(container.getByTestId("react-graph").classList.contains("highlight-nodes")).toBe(true);
  });

  it("When focuses are clicked, an information button should appear", async () => {
    const container = await TestContainer.build();
    expect(container.queryByRole("button", {name: "i"})).toBeNull;
    fireEvent.click(container.getByRole("button", {name: /FOCUSES ⪢/i}));
    fireEvent.click(container.getByRole("button", {name: /scientific computing/i}));
    expect(container.queryByRole("button", {name: /Focus Description/i})).toBeInTheDocument;
  });
});
