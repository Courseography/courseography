import React from "react";
import Sidebar from "../Sidebar";
import { render } from "@testing-library/react";

export default class TestSidebar {
  /**
   * For async construction of the TestSidebar
   * @return {TestSidebar}
   */
  static async build() {
    const sidebarProps = {
      currFocus: null,
      graphs: [ "Computer Science", "Biology"]
    };

    const sidebar = render(<Sidebar {...sidebarProps} />);
    return sidebar;
  }
}
