import { cleanup } from "react-testing-library";

afterEach(() => {
  cleanup();
  localStorage.clear();
});
