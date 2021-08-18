/**
 * Calculates the position of the click in relation to the page.
 * @param {object} e The click event.
 * @param {HTMLElement} elem The target of the click event.
 * @return {object} The position of the click.
 */
function getClickPosition(e, elem) {
  "use strict"

  var parentPosition = getPosition(elem)
  var xPosition = e.clientX - parentPosition.x
  var yPosition = e.clientY - parentPosition.y
  xPosition = Math.round(xPosition / 4) * 4 // for snapping!!
  yPosition = Math.round(yPosition / 4) * 4

  return { x: xPosition, y: yPosition }
}

/**
 * Calculates the position of elem in relation to the page.
 * @param {HTMLElement} elem The target of the click event.
 * @return {object} The position of elem on the page.
 */
function getPosition(elem) {
  "use strict"

  var xPosition = 0
  var yPosition = 0

  while (elem) {
    // || 0 for firefox compatability
    xPosition += (elem.offsetLeft || 0) - elem.scrollLeft + elem.clientLeft
    yPosition += (elem.offsetTop || 0) - elem.scrollTop + elem.clientTop
    elem = elem.parentElement // offsetParent undefined in mozilla
  }
  return { x: xPosition, y: yPosition }
}

/**
 * Creates a new node within the SVG canvas with specific attributes.
 * If no attributes are specified, then a generic blank node is created.
 * @param {String} posX The x-coordinate of the node.
 * @param {String} posY The y-coordinate of the node.
 * @param {JSON} jsonObj Specifies the node's attributes.
 */
function makeNode(posX, posY, jsonObj) {
  "use strict"

  var textPosX = posX
  var textPosY = posY
  var textStr = ""
  var textFill = ""
  var textAlign = "begin"
  var textId = "t" + nodeId

  if (jsonObj) {
    var nodeWidth = jsonObj.width
    var nodeHeight = jsonObj.height
    var nodeFill = jsonObj.fill || "none"
    var nodeId_ = "n" + jsonObj.id_
    var nodeTolerance = jsonObj.tolerance

    switch (jsonObj.type_) {
      case "Hybrid":
        var node = document.createElementNS(xmlns, "rect")
        var gClass = "hybrid"
        textFill = "white"
        break
      case "BoolNode":
        var node = document.createElementNS(xmlns, "ellipse")
        var gClass = "bool"
        node.setAttribute("stroke", "black")
        node.setAttribute("rx", 10)
        node.setAttribute("ry", 7)
        node.setAttribute("cx", posX)
        node.setAttribute("cy", posY)
        break
      default:
        // Node
        var node = document.createElementNS(xmlns, "rect")
        var gClass = "node"
        node.setAttribute("rx", 4)
        node.setAttribute("ry", 4)
    }
  } else {
    var nodeWidth = 40
    var nodeHeight = 32
    var nodeFill = "#" + $("#select-colour").val()
    var nodeId_ = "n" + nodeId
    var nodeTolerance = 9

    var gClass = "node"
    var node = document.createElementNS(xmlns, "rect")
    node.setAttribute("rx", 4)
    node.setAttribute("ry", 4)
  }

  var g = document.createElementNS(xmlns, "g")

  g.setAttribute("class", gClass)
  g.setAttribute("id", "g" + nodeId)
  g.setAttribute("data-active", "active")

  node.setAttribute("x", posX)
  node.setAttribute("y", posY)
  node.setAttribute("fill", nodeFill)
  node.setAttribute("id", nodeId_)
  node.setAttribute("width", nodeWidth)
  node.setAttribute("height", nodeHeight)
  node.setAttribute("class", "node")
  node.setAttribute("tolerance", nodeTolerance)
  node.predecessors = []
  node.successors = []
  // note: children doesn't work because javascript objects already have a children attribute
  node.inEdges = []
  node.outEdges = []

  g.appendChild(node)
  document.getElementById("nodes").appendChild(g)
  document
    .getElementById(nodeId_)
    .addEventListener("mousedown", nodeClicked, false)

  // Input Text into Shape
  if (jsonObj) {
    for (var i = 0; i < jsonObj.text.length; i++) {
      textPosX = jsonObj.pos[0]
      if (jsonObj.type_ !== "BoolNode") {
        textPosX += jsonObj.width / 2
      }

      textPosY = jsonObj.text[i].pos[1] - jsonObj.height / 4
      textStr = jsonObj.text[i].text || ""
      textFill = jsonObj.text[i].fill
      textAlign = jsonObj.text[i].align
      textId = "t" + jsonObj.id_

      var code = document.createElementNS(xmlns, "text")
      code.setAttributeNS(null, "id", textId)
      code.setAttributeNS(null, "fill", textFill)
      code.setAttributeNS(null, "align", textAlign)
      code.setAttributeNS(null, "x", textPosX)
      code.setAttributeNS(null, "y", textPosY)
      code.setAttributeNS(null, "class", "mylabel") // note: label is a class in bootstrap
      var textNode = document.createTextNode(textStr)
      code.appendChild(textNode)
      g.appendChild(code)
      document
        .getElementById(textId)
        .addEventListener("mousedown", nodeClicked, false)
    }
  }
  select(document.getElementById(nodeId_))
  nodeId += 1
}

/**
 * In node-mode creates a new node at the position of the click event on the SVG canvas.
 * In path-mode creates an elbow at the position of the click event on the SVG canvas,
                if the startNode is defined.
 * @param {object} e The mousedown event.
 */
export function makeNodePath(e) {
  "use strict"
  var position = getClickPosition(e, e.currentTarget)
  if (mode === "node-mode") {
    makeNode(position.x, position.y)
  } else if (mode === "path-mode") {
    // make elbow joint, only if the dummy point is outside the starting node
    if (
      startNode !== null &&
      (position.x < parseFloat(startNode.getAttribute("x"), 10) ||
        position.x > parseFloat(startNode.getAttribute("x"), 10) + nodeWidth ||
        position.y < parseFloat(startNode.getAttribute("y"), 10) ||
        position.y > parseFloat(startNode.getAttribute("y"), 10) + nodeHeight)
    ) {
      if (curPath === null) {
        // node to elbow path
        var pathString = findClosest(
          {
            x: parseFloat(startNode.getAttribute("x"), 10),
            y: parseFloat(startNode.getAttribute("y"), 10),
          },
          "node",
          position,
          "elbow"
        )
        startPath(pathString, "path")
      } else {
        // elbow to elbow path
        curPath.setAttributeNS(
          null,
          "d",
          curPath.getAttribute("d") + "L" + position.x + "," + position.y + " "
        )
      }

      makeElbow(position)
    }
  } else if (mode === "region-mode") {
    var elbow

    if (startPoint === null) {
      // start a region
      startPoint = document.createElementNS(xmlns, "circle")

      startPoint.setAttributeNS(null, "cx", position.x)
      startPoint.setAttributeNS(null, "cy", position.y)
      startPoint.setAttributeNS(null, "r", 4)
      startPoint.setAttributeNS(null, "class", "rElbow") // !! CHANGE CLASS

      startPoint.addEventListener("mousedown", selectElbow, false)
      svgDoc.appendChild(startPoint)

      startPoint.partOfPath = "start"
    } else if (curPath === null) {
      // first elbow
      startPath(
        "M" +
          startPoint.getAttribute("cx") +
          "," +
          startPoint.getAttribute("cy") +
          " L" +
          position.x +
          "," +
          position.y +
          " ",
        "region"
      )
      //curPath.setAttributeNS(null, 'class', 'region');
      curPath.setAttributeNS(null, "id", "r" + regionId)
      elbow = makeElbow(position)
      elbow.partOfPath = "elbow"
    } else {
      curPath.setAttributeNS(
        null,
        "d",
        curPath.getAttribute("d") + "L" + position.x + "," + position.y + " "
      )
      elbow = makeElbow(position)
      elbow.partOfPath = "elbow"
    }
  }
}

/**
 * Handles the clicking of the target of the event (a node) in different modes.
 * In erase-mode, erases the node, and all edges in and out of the node.
 * In change-mode, moves the node, and in path-mode, marks it a start node
 * or creates a path if valid.                                                          // !! FIX DESCRIPTION
 * @param {object} e The mousedown event.
 */
function nodeClicked(e) {
  "use strict"

  var index = null

  var targetNode = null
  if (e.currentTarget.id[0] === "t") {
    targetNode = document.getElementById("n" + e.currentTarget.id.slice(1))
  } else {
    targetNode = e.currentTarget
  }

  if (mode === "erase-mode") {
    // remove any paths leading to and from this node from the other node's
    // list of paths and remove this node from the other nodes' adjacency lists
    targetNode.inEdges.map(function (edge) {
      // Remove edge from parent's outEdges and current node from parent's successors list
      var edgeParent = document.getElementById(
        edge.id.slice(0, edge.id.lastIndexOf("n"))
      )
      index = edgeParent.outEdges.indexOf(edge)
      if (index > -1) {
        edgeParent.outEdges.splice(index, 1)
      }
      index = edgeParent.successors.indexOf(targetNode)
      if (index > -1) {
        edgeParent.successors.splice(index, 1)
      }
      erasePath(edge)
    })
    targetNode.outEdges.map(function (edge) {
      // Remove edge from children's inEdges and current node from child's parents list
      var edgeChild = document.getElementById(
        edge.id.slice(edge.id.lastIndexOf("n"))
      )
      index = edgeChild.inEdges.indexOf(edge)
      if (index > -1) {
        edgeChild.inEdges.splice(index, 1)
      }
      index = edgeChild.predecessors.indexOf(targetNode)
      if (index > -1) {
        edgeChild.predecessors.splice(index, 1)
      }
      erasePath(edge)
    })
    svgDoc.removeChild(targetNode.parentNode)
  } else if (mode === "change-mode") {
    var position = getClickPosition(e, targetNode)

    nodeMoving = targetNode
    prevX = position.x
    prevY = position.y

    // show which node has been selected
    select(targetNode)
  } else if (mode === "path-mode") {
    if (startNode === null) {
      startNode = targetNode
      select(targetNode)
    } else if (startNode === targetNode) {
      // this is the start node of the path about to be created, self loops not allowed
      if (curPath !== null) {
        curPath.elbows.map(function (item) {
          svgDoc.removeChild(item)
        })
        svgDoc.removeChild(curPath)
        curPath = null
      }
    } else {
      // make the path from startNode to current node then make startNode Null
      var pathId = startNode.id + targetNode.id
      if (document.getElementById(pathId) === null) {
        finishPath(pathId, targetNode)
        startNode = null
        curPath = null
      } else {
        // a path between these two nodes already, duplicates not allowed
        startNode = null
        if (curPath !== null) {
          curPath.elbows.map(function (item) {
            svgDoc.removeChild(item)
          })
          svgDoc.removeChild(curPath)
          curPath = null
        }
      }
    }
  }
}

/**
 * Selects the node newNode and unselects the old node nodeSelected.
 * @param {SVGElement} newNode The node to be selected.
 */
function select(newNode) {
  "use strict"

  if (nodeSelected !== null) {
    nodeSelected.parentNode.setAttribute("data-active", "unselected")
  }
  nodeSelected = newNode
  nodeSelected.parentNode.setAttribute("data-active", "active")
}

/**
 * In change-mode, moves the node or elbow that is currently being moved.
 * @param {object} e The mousemove Event.
 */
export function moveNodeElbow(e) {
  "use strict"

  if (mode === "change-mode") {
    var position

    if (nodeMoving !== null) {
      position = getClickPosition(e, nodeMoving)
      var rectX = parseFloat(nodeMoving.getAttribute("x"), 10)
      var rectY = parseFloat(nodeMoving.getAttribute("y"), 10)
      rectX += position.x - prevX
      rectY += position.y - prevY
      nodeMoving.setAttribute("x", rectX)
      nodeMoving.setAttribute("y", rectY)

      if (nodeMoving.parentNode.childNodes.length > 1) {
        // move text
        var textNode = nodeMoving.parentNode.childNodes[1]
        var textX = parseFloat(textNode.getAttribute("x"), 10)
        var textY = parseFloat(textNode.getAttribute("y"), 10)
        textX += position.x - prevX
        textY += position.y - prevY
        textNode.setAttribute("x", textX)
        textNode.setAttribute("y", textY)
      }

      // move in and out edges by the same amount
      nodeMoving.inEdges.map(function (item) {
        // modify last node in path
        movePath(item, position.x - prevX, position.y - prevY, "end", -1)
      })
      nodeMoving.outEdges.map(function (item) {
        // modify the first node in path
        movePath(item, position.x - prevX, position.y - prevY, "start", -1)
      })

      prevX = position.x
      prevY = position.y
    } else if (elbowMoving !== null) {
      position = getClickPosition(e, elbowMoving)
      moveElbow(elbowMoving, position)
      prevX = position.x
      prevY = position.y
    } else if (regionMoving !== null) {
      // move each elbow
      position = getClickPosition(e, regionMoving)
      regionMoving.elbows.map(function (elbow) {
        moveElbow(elbow, position)
      })
      prevX = position.x
      prevY = position.y
    }
  }
}

/**
 *
 * @param elbow
 * @param position
 */
function moveElbow(elbow, position) {
  "use strict"

  // move dummy node
  var elbowX = parseFloat(elbow.getAttribute("cx"), 10)
  var elbowY = parseFloat(elbow.getAttribute("cy"), 10)
  elbowX += position.x - prevX
  elbowY += position.y - prevY
  elbow.setAttribute("cx", elbowX)
  elbow.setAttribute("cy", elbowY)

  // move actual elbow in path
  var partOfPath = "elbow"
  if (elbow.getAttribute("class") === "rElbow") {
    partOfPath = elbow.partOfPath
  }

  movePath(
    document.getElementById(elbow.path),
    position.x - prevX,
    position.y - prevY,
    "elbow",
    document.getElementById(elbow.path).elbows.indexOf(elbow)
  )
}

/**
 * Reinitializes global variables associated with mousedown and mousemove event.
 * @param {object} e The mouseup event.
 */
export function unclickAll(e) {
  "use strict"

  if (mode === "change-mode") {
    nodeMoving = null
    prevX = -1
    prevY = -1
    elbowMoving = null
    regionMoving = null
  }
}

/**
 *
 */
function finishRegion() {
  "use strict"

  if (
    mode === "region-mode" &&
    curPath !== null &&
    curPath.elbows.length >= 3
  ) {
    curPath.setAttributeNS(null, "d", curPath.getAttribute("d") + "Z")
    curPath.setAttributeNS(null, "style", "fill:#" + $("#select-colour").val())
    curPath.addEventListener("mousedown", regionClicked, false)
    curPath.setAttributeNS(null, "pointer-events", "boundingBox") // to solve point in polygon problem
    curPath.setAttributeNS(null, "class", "region")
    curPath.setAttributeNS(null, "data-active", "region")

    curPath.elbows.map(function (item) {
      item.path = "r" + regionId
      item.setAttributeNS(null, "class", "rElbow")
    })

    regionId += 1
    curPath = null
    startPoint = null
  } else if (curPath !== null && curPath.elbows.length < 3) {
    curPath.elbows.map(function (item) {
      svgDoc.removeChild(item)
    })
    document.getElementById("regions").removeChild(curPath)
  }
}

/**
 *
 * @param e
 */
function regionClicked(e) {
  "use strict"
  if (mode === "erase-mode") {
    // delete the dummy nodes and the path itself
    e.currentTarget.elbows.map(function (item) {
      svgDoc.removeChild(item)
    })
    document.getElementById("regions").removeChild(e.currentTarget)
  } else if (mode === "change-mode") {
    var position = getClickPosition(e, e.currentTarget)
    regionMoving = e.currentTarget
    prevX = position.x
    prevY = position.y
  }
}

/**
 * Parse the SVG canvas and convert all the elements of the graph into a JSON list.
 * Note: The graph ID is set to a "dummy" value of -1.
 * @return {JSON} A JSON string which stores the parsed SVG elements.
 */
function convertSvgToJson() {
  "use strict"

  var gId = -1
  var texts = []
  var shapes = []
  var paths = []
  var svgElements = document.getElementById("mySVG").children
  for (var i = 0; i < svgElements.length; i++) {
    if (svgElements[i].tagName === "g" && svgElements[i].id !== "regions") {
      var gText = svgElements[i].querySelector("text")
      if (gText !== null) {
        texts.push({
          graph: gId,
          rId: "",
          pos: [
            parseFloat(gText.getAttribute("x")),
            parseFloat(gText.getAttribute("y")),
          ],
          text: gText.textContent,
          align: "begin",
          fill: "",
        })
      }

      var gRect = svgElements[i].querySelector("rect")
      shapes.push({
        graph: gId,
        id_: "",
        pos: [
          parseFloat(gRect.getAttribute("x")),
          parseFloat(gRect.getAttribute("y")),
        ],
        width: parseFloat(gRect.getAttribute("width")),
        height: parseFloat(gRect.getAttribute("height")),
        fill: "none",
        stroke: "",
        text: gText !== null ? texts.slice(-1) : [],
        tolerance: 9,
        type_: "Node",
      })
    } else if (svgElements[i].tagName === "path") {
      paths.push({
        graph: gId,
        id_: "",
        points: getPathCoords(svgElements[i].getAttribute("d")),
        fill: "none",
        stroke: "",
        isRegion: false,
        source: "",
        target: "",
      })
    }
  }

  return JSON.stringify({ texts: texts, shapes: shapes, paths: paths })
}

/**
 * Convert path tag coordinates into a list of tuples.
 * @param {String} pathCoords The set of path coordinates within the "d" attribute of a <path> HTML tag.
 * @return {[String]} A list of tuples.
 */
function getPathCoords(pathCoords) {
  "use strict"

  return pathCoords
    .split(" ")
    .filter(function (entry) {
      return entry !== ""
    })
    .map(function (coord) {
      return coord.slice(1).split(",").map(parseFloat)
    })
}

/**
 * Find the closest node (to the given coordinates), and return its text value.
 * @param {[Float]} coords A single set of XY-coordinates.
 * @return {String} The text value of the closest node.
 */
function getClosestText(coords, nodeList) {
  "use strict"

  return nodeList
    .map(function (node) {
      var a = node.pos[0] - coords[0]
      var b = node.pos[1] - coords[1]
      return [node.text[0].text, Math.sqrt(a * a + b * b)]
    })
    .sort(function (a, b) {
      return a[1] === b[1] ? 0 : a[1] < b[1] ? -1 : 1
    })[0][0]
}

/*
 * Convert JSON data into SVG elements then populate the canvas with them.
 */
function renderJson(jsonStr) {
  "use strict"

  var jsonData = JSON.parse(jsonStr)

  // Parse Path data
  jsonData.paths.forEach(function (jsonPath) {
    // Create Paths and Regions
    var path = document.createElementNS(xmlns, "path")
    path.setAttributeNS(null, "id", "pathp" + nodeId)
    path.setAttributeNS(null, "d", "M" + jsonPath.points.join(" "))
    path.setAttributeNS(null, "data-active", "inactive")
    if (jsonPath.isRegion) {
      path.setAttributeNS(null, "class", "region")
      path.setAttributeNS(null, "style", "fill:" + jsonPath.fill)
    } else {
      path.setAttributeNS(null, "stroke", "black")
      path.setAttributeNS(null, "marker-end", "url(#arrow)")
      path.setAttributeNS(null, "class", "path")
      path.setAttributeNS(null, "data-active", "drawn")
    }

    svgDoc.appendChild(path)
    nodeId += 1
  })

  // generate SVG nodes
  jsonData.shapes.forEach(function (jsonShape) {
    makeNode(jsonShape.pos[0], jsonShape.pos[1], jsonShape)
  })

  // Parse Text data
  jsonData.texts.forEach(function (jsonText) {
    if (jsonText.rId.indexOf("tspan") > -1) {
      var code = document.createElementNS(xmlns, "text")
      code.setAttributeNS(null, "id", "tspan" + nodeId)
      code.setAttributeNS(null, "fill", jsonText.fill)
      code.setAttributeNS(null, "align", jsonText.align)
      code.setAttributeNS(null, "x", jsonText.pos[0])
      code.setAttributeNS(null, "y", jsonText.pos[1])
      code.setAttributeNS(null, "class", "region-label")
      var textNode = document.createTextNode(jsonText.text)
      code.appendChild(textNode)
      svgDoc.appendChild(code)

      nodeId += 1
    }
  })
}
