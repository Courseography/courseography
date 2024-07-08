import { SpecialistPost, MajorPost, MinorPost } from "./post_components.js.jsx"

import React from "react"
import {createRoot} from "react-dom/client"
import { createRef } from "react"

export default class CheckMyPost extends React.Component {
  constructor(props) {
    super(props)

    this.changeActiveTab = this.changeActiveTab.bind(this)
    this.updateNavCreditCounts = this.updateNavCreditCounts.bind(this)
    this.updatePostStatus = this.updatePostStatus.bind(this)
    this.postNavRef = createRef(null)
    this.tabs = ["spePost", "majPost", "minPost"]
    this.postTypeRefMap = this.tabs.reduce((a, v) => (
      { ...a, [v]: createRef(null)}), {}
    ) 
  }

  componentDidMount() {
    const activeTab = this.postNavRef.current.state.activeTab + "Post"
    this.changeActiveTab(activeTab)
    this.updateNavCreditCounts()
  }

  changeActiveTab(newTab) {
    const activeTab = newTab

    this.tabs.forEach(tab => {
      if (tab === activeTab) {
        this.postTypeRefMap[tab].current.changeTabView(true)
      } else {
        this.postTypeRefMap[tab].current.changeTabView(false)
      }
    })
  }

  updateNavCreditCounts() {
    const newCounts = this.tabs.map(
      (tab) => this.postTypeRefMap[tab].current.getCreditCount()
    )
    this.postNavRef.current.setState({ creditCounts: newCounts }, this.updatePostStatus())
  }

  updatePostStatus() {
    const newStatuses = this.tabs.map(
      (tab) => this.postTypeRefMap[tab].current.setIfCompleted()
    )
    this.postNavRef.current.setState({ completed: newStatuses })
  }

  render() {
    return (
      <div id="check_my_post">
        <PostNav
          ref={this.postNavRef}
          updateTab={this.changeActiveTab}
          getCreditCountClass={this.getCreditCountClass}
        />
        <SpecialistPost ref={this.postTypeRefMap["spePost"]} />
        <MajorPost ref={this.postTypeRefMap["majPost"]} />
        <MinorPost ref={this.postTypeRefMap["minPost"]} />
      </div>
    )
  }
}

class PostNav extends React.Component {
  constructor(props) {
    super(props)
    this.state = {
      visible:
        localStorage.getItem("activePost") === ""
          ? "spe"
          : localStorage.getItem("activePost"),
      creditCounts: [0.0, 0.0, 0.0],
      completed: [false, false, false],
      activeTab: localStorage.getItem("activePost"),
    }

    this.getActiveTab = this.getActiveTab.bind(this)
    this.changeActiveTab = this.changeActiveTab.bind(this)
    this.getNavClass = this.getNavClass.bind(this)
    this.getCreditCountClass = this.getCreditCountClass.bind(this)
  }

  getActiveTab() {
    return localStorage.getItem("activePost")
  }

  changeActiveTab(e) {
    const newVisible = e.target.id.substring(0, 3)
    this.setState({ visible: newVisible }, () => {
      this.props.updateTab(newVisible + "Post")
      localStorage.setItem("activePost", newVisible)
    })
  }

  getNavClass(type) {
    return this.state.visible === type ? "nav_selected" : "nav_not_selected"
  }

  getCreditCountClass(postIndex) {
    return this.state.completed[postIndex]
      ? "credits_completed"
      : "credits_not_completed"
  }

  render() {
    return (
      <nav id="posts">
        <ul>
          <li id="specialist" className={this.getNavClass("spe")}>
            <a id="spec_link" onClick={this.changeActiveTab}>
              {" "}
              Specialist{" "}
            </a>
            <div id="spec_creds" className={this.getCreditCountClass(0)}>
              {"(" + this.state.creditCounts[0].toFixed(1) + "/12.0)"}{" "}
            </div>
          </li>
          <li id="major" className={this.getNavClass("maj")}>
            <a id="maj_link" onClick={this.changeActiveTab}>
              {" "}
              Major{" "}
            </a>
            <div id="maj_creds" className={this.getCreditCountClass(1)}>
              {"(" + this.state.creditCounts[1].toFixed(1) + "/8.0)"}{" "}
            </div>
          </li>
          <li id="minor" className={this.getNavClass("min")}>
            <a id="min_link" onClick={this.changeActiveTab}>
              {" "}
              Minor{" "}
            </a>
            <div id="min_creds" className={this.getCreditCountClass(2)}>
              {"(" + this.state.creditCounts[2].toFixed(1) + "/4.0)"}{" "}
            </div>
          </li>
        </ul>
      </nav>
    )
  }
}

const container = document.getElementById("all_posts")
const root = createRoot(container)
root.render(<CheckMyPost />)