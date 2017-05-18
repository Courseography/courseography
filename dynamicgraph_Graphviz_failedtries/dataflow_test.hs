module Main where

import DataFlow.Core
import DataFlow.Graphviz.Renderer
import DataFlow.DFD
import DataFlow.Graphviz

diagram {
  title = "Webapp"


  threats = `
    No particular threats at this point.

    It's **extremely** safe.`

  boundary {
    title = "Browser"

    function client {
      title = "Client"
    }
  }

  boundary {
    title = "Amazon AWS"

    function server {
      title = "Web Server"
    }
    database logs {
      title = "Logs"
    }
  }
  io analytics {
    title = "Google Analytics"
  }

  client -> server {
    operation = "Request /"
    description = `User navigates with a browser to see some content.`
  }
  server -> logs {
    operation = "Log"
    data = `The user
            IP address.`
    description = `Logged to a ELK stack.`
  }
  server -> client {
    operation = "Response"
    data = "User Profile"
    description = `The server responds with some HTML.`
  }
  analytics <- client {
    operation = "Log"
    data = "Page Navigation"
    description = `The Google Analytics plugin sends navigation
                   data to Google.`
  }
}