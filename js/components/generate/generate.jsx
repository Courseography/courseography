import { createRoot } from "react-dom/client"
import GenerateForm from "./GenerateForm.js"
import { NavBar } from "../common/NavBar.js.jsx"

const navbar = document.getElementById("navbar")
const navbarRoot = createRoot(navbar)
navbarRoot.render(<NavBar page="generate" />)

const container = document.getElementById("generateRoot")
const root = createRoot(container)
root.render(<GenerateForm />)
