import { fireEvent} from "@testing-library/react";
import TestFocusBar from "./TestFocusBar";
import TestContainer from "./TestContainer";

describe("FocusBar", () => {
  it("should match shallow snapshot", async () => {
    const focusBar = await TestFocusBar.build();
    expect(focusBar).toMatchSnapshot();
  });

  it("Focusbar should be disabled for non-computer science graphs", async () => {
    const container = await TestContainer.build();
    expect(container.getByRole("button", {name: /FOCUSES ⪢/i})).toBeInTheDocument;
    fireEvent.click(container.getByTestId("test-graph-1"));
    expect(container.queryByRole("button", {name: /FOCUSES ⪢/i})).toBeNull;
  });

  it("Pressing the focusbar button should open it", async () => {
    const focusBar = await TestFocusBar.build();
    expect(focusBar.queryByRole("button", {name: /Scientific Computing/i})).toBeNull;
    fireEvent.click(focusBar.getByRole("button", {name: /FOCUSES ⪢/i}));
    expect(focusBar.getByRole("button", {name: /Scientific Computing/i})).toBeInTheDocument;
  });
});
