(function () {
  "use strict";

  // --- Data ---
  var data = BIPLOT_DATA;
  var elements = data.elements;
  var constructs = data.constructs;
  var meta = data.meta;
  var ratings = data.ratings;

  // --- State: per-item visibility ---
  var elementVisible = elements.map(function () { return true; });
  var constructVisible = constructs.map(function () { return true; });

  // --- Scene setup ---
  var sceneContainer = document.getElementById("scene-container");
  var width = sceneContainer.clientWidth;
  var height = sceneContainer.clientHeight;

  var scene = new THREE.Scene();
  scene.background = new THREE.Color(0xffffff);

  var camera = new THREE.PerspectiveCamera(50, width / height, 0.01, 100);
  camera.position.set(2.2, 1.5, 2.2);
  camera.lookAt(0, 0, 0);

  var renderer = new THREE.WebGLRenderer({ antialias: true });
  renderer.setSize(width, height);
  renderer.setPixelRatio(window.devicePixelRatio);
  sceneContainer.appendChild(renderer.domElement);

  // CSS2D renderer for labels
  var labelRenderer = new THREE.CSS2DRenderer();
  labelRenderer.setSize(width, height);
  labelRenderer.domElement.style.position = "absolute";
  labelRenderer.domElement.style.top = "0";
  labelRenderer.domElement.style.left = "0";
  labelRenderer.domElement.style.pointerEvents = "none";
  sceneContainer.appendChild(labelRenderer.domElement);

  // Orbit controls
  var controls = new THREE.OrbitControls(camera, renderer.domElement);
  controls.enableDamping = true;
  controls.dampingFactor = 0.08;
  controls.rotateSpeed = 0.8;
  controls.zoomSpeed = 1.0;
  controls.panSpeed = 0.8;

  var initialCameraPosition = camera.position.clone();
  var initialControlsTarget = controls.target.clone();

  // --- Groups ---
  var sphereGroup = new THREE.Group();        // wireframe sphere
  var elementPointsGroup = new THREE.Group();
  var elementLabelsGroup = new THREE.Group();
  var constructPointsGroup = new THREE.Group(); // small markers at pole positions
  var constructLinesGroup = new THREE.Group();
  var constructLabelsGroup = new THREE.Group();
  var axesGroup = new THREE.Group();
  var projectionsGroup = new THREE.Group();
  projectionsGroup.visible = false;

  scene.add(sphereGroup);
  scene.add(elementPointsGroup);
  scene.add(elementLabelsGroup);
  scene.add(constructPointsGroup);
  scene.add(constructLinesGroup);
  scene.add(constructLabelsGroup);
  scene.add(axesGroup);
  scene.add(projectionsGroup);

  // --- Raycaster ---
  var raycaster = new THREE.Raycaster();
  var mouse = new THREE.Vector2();
  var tooltip = document.getElementById("tooltip");
  var hoverTargets = [];
  var hoveredElementIndex = -1;

  // --- Colors (matching the reference image) ---
  var elementColor = 0x2c3e50;
  var rightPoleColor = 0x226644;  // green
  var leftPoleColor = 0xaa4422;   // red/brown

  // =============================================
  // 1. WIREFRAME SPHERE
  // =============================================
  var sphereRadius = 1.0;
  var wireGeom = new THREE.SphereBufferGeometry(sphereRadius, 32, 24);
  var wireMat = new THREE.MeshBasicMaterial({
    color: 0xcccccc,
    wireframe: true,
    transparent: true,
    opacity: 0.15
  });
  var wireSphere = new THREE.Mesh(wireGeom, wireMat);
  sphereGroup.add(wireSphere);

  // =============================================
  // 2. CONSTRUCT POLES on sphere surface
  // =============================================
  // Normalize construct vectors to unit length (sphere surface)
  var constructSphereCoords = [];
  for (var i = 0; i < constructs.length; i++) {
    var con = constructs[i];
    var len = Math.sqrt(con.x * con.x + con.y * con.y + con.z * con.z);
    if (len === 0) len = 1;
    constructSphereCoords.push({
      rx: con.x / len * sphereRadius,
      ry: con.y / len * sphereRadius,
      rz: con.z / len * sphereRadius,
      lx: -con.x / len * sphereRadius,
      ly: -con.y / len * sphereRadius,
      lz: -con.z / len * sphereRadius
    });
  }

  // Per-construct scene objects
  var constructObjects = []; // {line, rightLabel, leftLabel, rightMarker, leftMarker}

  var crossGeom = new THREE.SphereBufferGeometry(0.015, 6, 4);

  for (var i = 0; i < constructs.length; i++) {
    var con = constructs[i];
    var sc = constructSphereCoords[i];

    // Line from left pole through origin to right pole (on sphere)
    var lineGeom = new THREE.BufferGeometry().setFromPoints([
      new THREE.Vector3(sc.lx, sc.ly, sc.lz),
      new THREE.Vector3(sc.rx, sc.ry, sc.rz)
    ]);
    var lineMat = new THREE.LineDashedMaterial({
      color: 0xaabbaa,
      opacity: 0.3,
      transparent: true,
      dashSize: 0.03,
      gapSize: 0.02
    });
    var line = new THREE.Line(lineGeom, lineMat);
    line.computeLineDistances();
    constructLinesGroup.add(line);

    // Right pole marker (green)
    var rMat = new THREE.MeshBasicMaterial({ color: rightPoleColor });
    var rMarker = new THREE.Mesh(crossGeom, rMat);
    rMarker.position.set(sc.rx, sc.ry, sc.rz);
    rMarker.userData = { type: "construct", index: i, pole: "right",
      name: con.right_pole + " - " + con.left_pole, quality: con.quality };
    constructPointsGroup.add(rMarker);
    hoverTargets.push(rMarker);

    // Left pole marker (red)
    var lMat = new THREE.MeshBasicMaterial({ color: leftPoleColor });
    var lMarker = new THREE.Mesh(crossGeom, lMat);
    lMarker.position.set(sc.lx, sc.ly, sc.lz);
    lMarker.userData = { type: "construct", index: i, pole: "left",
      name: con.left_pole + " - " + con.right_pole, quality: con.quality };
    constructPointsGroup.add(lMarker);
    hoverTargets.push(lMarker);

    // Right pole label
    var rightDiv = document.createElement("div");
    rightDiv.className = "label-construct-right";
    rightDiv.textContent = con.right_pole;
    var rightLabel = new THREE.CSS2DObject(rightDiv);
    rightLabel.position.set(sc.rx, sc.ry, sc.rz);
    constructLabelsGroup.add(rightLabel);

    // Left pole label
    var leftDiv = document.createElement("div");
    leftDiv.className = "label-construct-left";
    leftDiv.textContent = con.left_pole;
    var leftLabel = new THREE.CSS2DObject(leftDiv);
    leftLabel.position.set(sc.lx, sc.ly, sc.lz);
    constructLabelsGroup.add(leftLabel);

    constructObjects.push({
      line: line, rightLabel: rightLabel, leftLabel: leftLabel,
      rightMarker: rMarker, leftMarker: lMarker
    });
  }

  // =============================================
  // 3. ELEMENTS (spheres inside + labels)
  // =============================================
  var elSphereGeom = new THREE.SphereBufferGeometry(0.03, 16, 12);
  var elementObjects = []; // {sphere, label}

  for (var i = 0; i < elements.length; i++) {
    var el = elements[i];
    var mat = new THREE.MeshPhongMaterial({
      color: elementColor,
      shininess: 60,
      specular: 0x444444
    });
    var sphere = new THREE.Mesh(elSphereGeom, mat);
    sphere.position.set(el.x, el.y, el.z);
    sphere.userData = { type: "element", index: i, name: el.name, quality: el.quality };
    elementPointsGroup.add(sphere);
    hoverTargets.push(sphere);

    var labelDiv = document.createElement("div");
    labelDiv.className = "label-element";
    labelDiv.textContent = el.name;
    var label = new THREE.CSS2DObject(labelDiv);
    label.position.set(el.x, el.y + 0.05, el.z);
    elementLabelsGroup.add(label);

    elementObjects.push({ sphere: sphere, label: label });
  }

  // Simple ambient + directional light for Phong material
  scene.add(new THREE.AmbientLight(0xffffff, 0.6));
  var dirLight = new THREE.DirectionalLight(0xffffff, 0.4);
  dirLight.position.set(2, 3, 2);
  scene.add(dirLight);

  // =============================================
  // 4. AXES
  // =============================================
  var axisLength = 1.3;
  var axisColors = [0xcc4444, 0x44aa44, 0x4444cc];

  for (var a = 0; a < 3; a++) {
    var start = new THREE.Vector3(0, 0, 0);
    var end = new THREE.Vector3(0, 0, 0);
    var negEnd = new THREE.Vector3(0, 0, 0);
    end.setComponent(a, axisLength);
    negEnd.setComponent(a, -axisLength);

    var axGeom = new THREE.BufferGeometry().setFromPoints([start, end]);
    var axMat = new THREE.LineBasicMaterial({ color: axisColors[a], opacity: 0.4, transparent: true });
    axesGroup.add(new THREE.Line(axGeom, axMat));

    var negGeom = new THREE.BufferGeometry().setFromPoints([start, negEnd]);
    var negMat = new THREE.LineDashedMaterial({
      color: axisColors[a], opacity: 0.2, transparent: true,
      dashSize: 0.03, gapSize: 0.02
    });
    var negLine = new THREE.Line(negGeom, negMat);
    negLine.computeLineDistances();
    axesGroup.add(negLine);

    var varPct = meta.var_explained_selected[a];
    var pctStr = (varPct * 100).toFixed(1) + "%";
    var axLabelDiv = document.createElement("div");
    axLabelDiv.className = "label-axis";
    axLabelDiv.textContent = "Dim " + meta.dim[a] + " (" + pctStr + ")";
    var axLabel = new THREE.CSS2DObject(axLabelDiv);
    var labelPos = new THREE.Vector3(0, 0, 0);
    labelPos.setComponent(a, axisLength + 0.05);
    axLabel.position.copy(labelPos);
    axesGroup.add(axLabel);
  }

  // =============================================
  // 5. PROJECTIONS
  // =============================================
  function buildProjections() {
    while (projectionsGroup.children.length > 0) {
      projectionsGroup.remove(projectionsGroup.children[0]);
    }
    for (var i = 0; i < elements.length; i++) {
      if (!elementVisible[i]) continue;
      var el = elements[i];
      var eVec = new THREE.Vector3(el.x, el.y, el.z);
      for (var j = 0; j < constructs.length; j++) {
        if (!constructVisible[j]) continue;
        var con = constructs[j];
        var cVec = new THREE.Vector3(con.x, con.y, con.z);
        var cDir = cVec.clone().normalize();
        var foot = cDir.clone().multiplyScalar(eVec.dot(cDir));
        var projGeom = new THREE.BufferGeometry().setFromPoints([eVec, foot]);
        var projMat = new THREE.LineDashedMaterial({
          color: 0xaaaaaa, opacity: 0.3, transparent: true,
          dashSize: 0.02, gapSize: 0.015
        });
        var projLine = new THREE.Line(projGeom, projMat);
        projLine.computeLineDistances();
        projectionsGroup.add(projLine);
      }
    }
  }
  buildProjections();

  // =============================================
  // 6. BACK-FACE CULLING for construct labels
  // =============================================
  // Only show labels on poles facing the camera
  var _camDir = new THREE.Vector3();
  var _poleDir = new THREE.Vector3();

  function updateLabelVisibility() {
    camera.getWorldDirection(_camDir);
    for (var i = 0; i < constructs.length; i++) {
      if (!constructVisible[i]) {
        constructObjects[i].rightLabel.visible = false;
        constructObjects[i].leftLabel.visible = false;
        constructObjects[i].rightMarker.visible = false;
        constructObjects[i].leftMarker.visible = false;
        constructObjects[i].line.visible = false;
        continue;
      }
      constructObjects[i].line.visible = constructLinesGroup.visible;

      var sc = constructSphereCoords[i];

      // Right pole: visible if facing camera
      _poleDir.set(sc.rx, sc.ry, sc.rz).normalize();
      var rightFacing = _poleDir.dot(_camDir) < 0;
      constructObjects[i].rightLabel.visible = rightFacing && constructLabelsGroup.visible;
      constructObjects[i].rightMarker.visible = rightFacing && constructPointsGroup.visible;

      // Left pole
      _poleDir.set(sc.lx, sc.ly, sc.lz).normalize();
      var leftFacing = _poleDir.dot(_camDir) < 0;
      constructObjects[i].leftLabel.visible = leftFacing && constructLabelsGroup.visible;
      constructObjects[i].leftMarker.visible = leftFacing && constructPointsGroup.visible;
    }
  }

  // =============================================
  // 7. GRID TABLE (left panel)
  // =============================================
  function buildGridTable() {
    var container = document.getElementById("grid-table-container");
    var title = document.createElement("h3");
    title.textContent = "Repertory Grid Data";
    container.appendChild(title);

    var table = document.createElement("table");
    table.className = "grid-table";
    table.id = "grid-table";

    // Header row: left pole | ratings... | right pole
    var thead = document.createElement("thead");
    var headerRow = document.createElement("tr");

    var thLeft = document.createElement("th");
    thLeft.textContent = "Left Pole";
    thLeft.className = "col-left-pole";
    headerRow.appendChild(thLeft);

    for (var e = 0; e < ratings.element_names.length; e++) {
      var th = document.createElement("th");
      th.className = "element-header";
      th.textContent = ratings.element_names[e];
      th.dataset.elementIndex = e;
      headerRow.appendChild(th);
    }

    var thRight = document.createElement("th");
    thRight.textContent = "Right Pole";
    thRight.className = "col-right-pole";
    headerRow.appendChild(thRight);

    thead.appendChild(headerRow);
    table.appendChild(thead);

    // Body rows: one per construct
    var tbody = document.createElement("tbody");

    for (var c = 0; c < ratings.left_poles.length; c++) {
      var tr = document.createElement("tr");
      tr.dataset.constructIndex = c;

      var tdLeft = document.createElement("td");
      tdLeft.className = "col-left-pole";
      tdLeft.textContent = ratings.left_poles[c];
      tr.appendChild(tdLeft);

      var rowValues = ratings.values[c]; // 2D array: ratings.values[construct][element]
      for (var e = 0; e < ratings.element_names.length; e++) {
        var td = document.createElement("td");
        td.className = "rating-cell";
        td.dataset.elementIndex = e;
        td.dataset.constructIndex = c;
        td.textContent = rowValues[e];
        tr.appendChild(td);
      }

      var tdRight = document.createElement("td");
      tdRight.className = "col-right-pole";
      tdRight.textContent = ratings.right_poles[c];
      tr.appendChild(tdRight);

      tbody.appendChild(tr);
    }

    table.appendChild(tbody);
    container.appendChild(table);
  }
  buildGridTable();

  // Highlight column in grid table
  function highlightGridColumn(elementIndex) {
    var table = document.getElementById("grid-table");
    if (!table) return;

    // Clear previous highlights
    var highlighted = table.querySelectorAll(".col-highlight");
    for (var k = 0; k < highlighted.length; k++) {
      highlighted[k].classList.remove("col-highlight");
    }

    if (elementIndex < 0) return;

    // Highlight header
    var headers = table.querySelectorAll("th.element-header");
    for (var k = 0; k < headers.length; k++) {
      if (parseInt(headers[k].dataset.elementIndex) === elementIndex) {
        headers[k].classList.add("col-highlight");
      }
    }

    // Highlight data cells
    var cells = table.querySelectorAll("td.rating-cell");
    for (var k = 0; k < cells.length; k++) {
      if (parseInt(cells[k].dataset.elementIndex) === elementIndex) {
        cells[k].classList.add("col-highlight");
      }
    }
  }

  // Highlight row in grid table
  function highlightGridRow(constructIndex) {
    var table = document.getElementById("grid-table");
    if (!table) return;

    var highlighted = table.querySelectorAll(".row-highlight");
    for (var k = 0; k < highlighted.length; k++) {
      highlighted[k].classList.remove("row-highlight");
    }

    if (constructIndex < 0) return;

    var rows = table.querySelectorAll("tbody tr");
    for (var k = 0; k < rows.length; k++) {
      if (parseInt(rows[k].dataset.constructIndex) === constructIndex) {
        var cells = rows[k].querySelectorAll("td");
        for (var j = 0; j < cells.length; j++) {
          cells[j].classList.add("row-highlight");
        }
      }
    }
  }

  // =============================================
  // 8. GUI PANEL
  // =============================================
  var guiPanel = document.getElementById("gui-panel");

  var guiTitle = document.createElement("h3");
  guiTitle.textContent = "3D Biplot";
  guiPanel.appendChild(guiTitle);

  function addSectionTitle(text) {
    var div = document.createElement("div");
    div.className = "section-title";
    var span = document.createElement("span");
    span.textContent = text;
    div.appendChild(span);
    guiPanel.appendChild(div);
    return div;
  }

  function addToggle(parent, labelText, checked, onChange) {
    var label = document.createElement("label");
    var cb = document.createElement("input");
    cb.type = "checkbox";
    cb.checked = checked;
    cb.addEventListener("change", function () { onChange(cb.checked); });
    label.appendChild(cb);
    label.appendChild(document.createTextNode(" " + labelText));
    parent.appendChild(label);
    return cb;
  }

  function addToggleAllButton(sectionDiv, getCheckboxes) {
    var btn = document.createElement("button");
    btn.className = "toggle-all-btn";
    btn.textContent = "all/none";
    btn.addEventListener("click", function () {
      var cbs = getCheckboxes();
      var allChecked = cbs.every(function (c) { return c.checked; });
      var newVal = !allChecked;
      cbs.forEach(function (c) {
        if (c.checked !== newVal) {
          c.checked = newVal;
          c.dispatchEvent(new Event("change"));
        }
      });
    });
    sectionDiv.appendChild(btn);
  }

  // --- Display section ---
  var displaySection = addSectionTitle("Display");
  addToggle(guiPanel, "Wireframe Sphere", true, function (v) { sphereGroup.visible = v; });
  addToggle(guiPanel, "Axes", true, function (v) { axesGroup.visible = v; });
  addToggle(guiPanel, "Projections", false, function (v) {
    projectionsGroup.visible = v;
    if (v) buildProjections();
  });

  // --- Elements section ---
  var elemSection = addSectionTitle("Elements");
  var elemCheckboxes = [];
  var elemListDiv = document.createElement("div");
  elemListDiv.className = "item-list";

  addToggleAllButton(elemSection, function () { return elemCheckboxes; });

  for (var i = 0; i < elements.length; i++) {
    (function (idx) {
      var cb = addToggle(elemListDiv, elements[idx].name, true, function (v) {
        elementVisible[idx] = v;
        elementObjects[idx].sphere.visible = v;
        elementObjects[idx].label.visible = v;
        if (projectionsGroup.visible) buildProjections();
      });
      elemCheckboxes.push(cb);
    })(i);
  }
  guiPanel.appendChild(elemListDiv);

  // --- Constructs section ---
  var conSection = addSectionTitle("Constructs");
  var conCheckboxes = [];
  var conListDiv = document.createElement("div");
  conListDiv.className = "item-list";

  addToggleAllButton(conSection, function () { return conCheckboxes; });

  for (var i = 0; i < constructs.length; i++) {
    (function (idx) {
      var label = constructs[idx].left_pole + " - " + constructs[idx].right_pole;
      var cb = addToggle(conListDiv, label, true, function (v) {
        constructVisible[idx] = v;
        // visibility will be managed by updateLabelVisibility()
        if (projectionsGroup.visible) buildProjections();
      });
      conCheckboxes.push(cb);
    })(i);
  }
  guiPanel.appendChild(conListDiv);

  // Reset camera
  var resetBtn = document.createElement("button");
  resetBtn.className = "action-btn";
  resetBtn.textContent = "Reset Camera";
  resetBtn.addEventListener("click", function () {
    camera.position.copy(initialCameraPosition);
    controls.target.copy(initialControlsTarget);
    controls.update();
  });
  guiPanel.appendChild(resetBtn);

  // =============================================
  // 9. HOVER + TOOLTIP + GRID HIGHLIGHT
  // =============================================
  function onMouseMove(event) {
    var rect = renderer.domElement.getBoundingClientRect();
    mouse.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
    mouse.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;

    raycaster.setFromCamera(mouse, camera);
    var intersects = raycaster.intersectObjects(hoverTargets);

    var newHoveredElement = -1;
    var newHoveredConstruct = -1;

    if (intersects.length > 0) {
      var obj = intersects[0].object;
      var ud = obj.userData;
      if (ud && ud.name) {
        var qualStr = ud.quality !== undefined
          ? " (quality: " + (ud.quality * 100).toFixed(1) + "%)"
          : "";
        tooltip.textContent = ud.name + qualStr;
        tooltip.style.display = "block";
        tooltip.style.left = (event.clientX + 12) + "px";
        tooltip.style.top = (event.clientY - 8) + "px";

        if (ud.type === "element") newHoveredElement = ud.index;
        if (ud.type === "construct") newHoveredConstruct = ud.index;
      }
    } else {
      tooltip.style.display = "none";
    }

    if (newHoveredElement !== hoveredElementIndex) {
      hoveredElementIndex = newHoveredElement;
      highlightGridColumn(hoveredElementIndex);
    }
    highlightGridRow(newHoveredConstruct);
  }

  renderer.domElement.addEventListener("mousemove", onMouseMove, false);
  renderer.domElement.addEventListener("mouseleave", function () {
    tooltip.style.display = "none";
    hoveredElementIndex = -1;
    highlightGridColumn(-1);
    highlightGridRow(-1);
  });

  // =============================================
  // 10. RESIZE
  // =============================================
  function onResize() {
    width = sceneContainer.clientWidth;
    height = sceneContainer.clientHeight;
    camera.aspect = width / height;
    camera.updateProjectionMatrix();
    renderer.setSize(width, height);
    labelRenderer.setSize(width, height);
  }
  window.addEventListener("resize", onResize);

  // =============================================
  // 11. RENDER LOOP
  // =============================================
  function animate() {
    requestAnimationFrame(animate);
    controls.update();
    updateLabelVisibility();
    renderer.render(scene, camera);
    labelRenderer.render(scene, camera);
  }
  animate();

})();
