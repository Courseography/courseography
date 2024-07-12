import { createRoot } from "react-dom/client"
import GenerateForm from "./GenerateForm.js"

const container = document.getElementById("generateRoot")
const root = createRoot(container)
root.render(<GenerateForm />)
