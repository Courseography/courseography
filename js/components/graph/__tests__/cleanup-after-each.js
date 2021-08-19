import { cleanup } from "@testing-library/react"

afterEach(() => {
  cleanup()
  localStorage.clear()
})
