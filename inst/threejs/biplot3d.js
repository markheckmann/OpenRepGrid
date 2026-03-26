(function () {
  "use strict";

  // --- Data ---
  var data = BIPLOT_DATA;
  var elements = data.elements;
  var constructs = data.constructs;
  var meta = data.meta;
  var ratings = data.ratings;
  var calibration = data.calibration;

  // --- State ---
  var elementVisible = elements.map(function () { return true; });
  var constructVisible = constructs.map(function () { return true; });
  var elementProjections = elements.map(function () { return false; }); // per-element projection toggle
  var constructLineVisible = constructs.map(function () { return false; }); // per-construct line toggle
  var calibrationLabelsVisible = true;

  // --- Scene setup ---
  var sceneContainer = document.getElementById("scene-container");
  var width = sceneContainer.clientWidth;
  var height = sceneContainer.clientHeight;

  var scene = new THREE.Scene();
  scene.background = new THREE.Color(0xffffff);

  var camera = new THREE.PerspectiveCamera(50, width / height, 0.01, 100);
  camera.position.set(0, 0, 3.5);
  camera.lookAt(0, 0, 0);

  var renderer = new THREE.WebGLRenderer({ antialias: true });
  renderer.setSize(width, height);
  renderer.setPixelRatio(window.devicePixelRatio);
  sceneContainer.appendChild(renderer.domElement);

  var labelRenderer = new THREE.CSS2DRenderer();
  labelRenderer.setSize(width, height);
  labelRenderer.domElement.style.position = "absolute";
  labelRenderer.domElement.style.top = "0";
  labelRenderer.domElement.style.left = "0";
  labelRenderer.domElement.style.pointerEvents = "none";
  sceneContainer.appendChild(labelRenderer.domElement);

  var controls = new THREE.OrbitControls(camera, renderer.domElement);
  controls.enableDamping = true;
  controls.dampingFactor = 0.08;
  controls.rotateSpeed = 0.8;
  controls.zoomSpeed = 1.0;
  controls.panSpeed = 0.8;

  var initialCameraPosition = camera.position.clone();
  var initialControlsTarget = controls.target.clone();

  // --- Groups ---
  var sphereGroup = new THREE.Group();
  var elementPointsGroup = new THREE.Group();
  var elementLabelsGroup = new THREE.Group();
  var constructPointsGroup = new THREE.Group();
  var constructLinesGroup = new THREE.Group();
  var constructLabelsGroup = new THREE.Group();
  var axesGroup = new THREE.Group();
  var projectionsGroup = new THREE.Group();
  var calibrationGroup = new THREE.Group();
  calibrationGroup.visible = true;
  var calibrationLabelsGroup = new THREE.Group();
  calibrationGroup.add(calibrationLabelsGroup);

  scene.add(sphereGroup);
  scene.add(elementPointsGroup);
  scene.add(elementLabelsGroup);
  scene.add(constructPointsGroup);
  scene.add(constructLinesGroup);
  scene.add(constructLabelsGroup);
  scene.add(axesGroup);
  scene.add(projectionsGroup);
  scene.add(calibrationGroup);

  // --- Raycaster ---
  var raycaster = new THREE.Raycaster();
  var mouse = new THREE.Vector2();
  var tooltip = document.getElementById("tooltip");
  var hoverTargets = [];
  var hoveredElementIndex = -1;
  var hoveredConstructIndex = -1;

  // --- Colors ---
  var elementColor = 0x2c3e50;
  var preferredPoleColor = 0x226644;   // green for preferred pole
  var nonpreferredPoleColor = 0xaa4422; // red for non-preferred pole
  var neutralPoleColor = 0x888888;      // gray when no preference set
  // Palette for per-element projection lines
  var projColors = [
    0xe6194b, 0x3cb44b, 0x4363d8, 0xf58231, 0x911eb4,
    0x42d4f4, 0xf032e6, 0xbfef45, 0xfabed4, 0x469990,
    0xdcbeff, 0x9a6324, 0x800000, 0xaaffc3, 0x808000
  ];

  // =============================================
  // 1. WIREFRAME SPHERE
  // =============================================
  var sphereRadius = 1.0;
  var wireGeom = new THREE.SphereBufferGeometry(sphereRadius, 32, 24);
  var wireMat = new THREE.MeshBasicMaterial({
    color: 0xcccccc, wireframe: true, transparent: true, opacity: 0.15
  });
  sphereGroup.add(new THREE.Mesh(wireGeom, wireMat));

  // =============================================
  // 2. CONSTRUCT POLES on sphere surface
  // =============================================
  var constructSphereCoords = [];
  for (var i = 0; i < constructs.length; i++) {
    var con = constructs[i];
    var len = Math.sqrt(con.x * con.x + con.y * con.y + con.z * con.z);
    if (len === 0) len = 1;
    constructSphereCoords.push({
      rx: con.x / len * sphereRadius, ry: con.y / len * sphereRadius, rz: con.z / len * sphereRadius,
      lx: -con.x / len * sphereRadius, ly: -con.y / len * sphereRadius, lz: -con.z / len * sphereRadius
    });
  }

  var constructObjects = [];
  var crossGeom = new THREE.SphereBufferGeometry(0.015, 6, 4);

  for (var i = 0; i < constructs.length; i++) {
    var con = constructs[i];
    var sc = constructSphereCoords[i];

    // Line from left to right pole (on sphere) - initially hidden, toggled by double-click
    var lineGeom = new THREE.BufferGeometry().setFromPoints([
      new THREE.Vector3(sc.lx, sc.ly, sc.lz),
      new THREE.Vector3(sc.rx, sc.ry, sc.rz)
    ]);
    var lineMat = new THREE.LineBasicMaterial({
      color: 0x888888, opacity: 0.6, transparent: true
    });
    var line = new THREE.Line(lineGeom, lineMat);
    line.visible = false;
    constructLinesGroup.add(line);

    // Determine colors based on which pole is preferred
    var pref = con.preferred;  // "left", "right", "both", "none", or null
    var rColor, lColor, rClass, lClass;
    if (pref === "right") {
      rColor = preferredPoleColor;
      lColor = nonpreferredPoleColor;
      rClass = "label-construct-preferred";
      lClass = "label-construct-nonpreferred";
    } else if (pref === "left") {
      rColor = nonpreferredPoleColor;
      lColor = preferredPoleColor;
      rClass = "label-construct-nonpreferred";
      lClass = "label-construct-preferred";
    } else if (pref === "both") {
      rColor = preferredPoleColor;
      lColor = preferredPoleColor;
      rClass = "label-construct-preferred";
      lClass = "label-construct-preferred";
    } else {
      rColor = neutralPoleColor;
      lColor = neutralPoleColor;
      rClass = "label-construct-neutral";
      lClass = "label-construct-neutral";
    }

    // Right pole marker
    var rMat = new THREE.MeshBasicMaterial({ color: rColor });
    var rMarker = new THREE.Mesh(crossGeom, rMat);
    rMarker.position.set(sc.rx, sc.ry, sc.rz);
    rMarker.userData = { type: "construct", index: i, pole: "right",
      name: con.right_pole + " \u2014 " + con.left_pole, quality: con.quality };
    constructPointsGroup.add(rMarker);
    hoverTargets.push(rMarker);

    // Left pole marker
    var lMat = new THREE.MeshBasicMaterial({ color: lColor });
    var lMarker = new THREE.Mesh(crossGeom, lMat);
    lMarker.position.set(sc.lx, sc.ly, sc.lz);
    lMarker.userData = { type: "construct", index: i, pole: "left",
      name: con.left_pole + " \u2014 " + con.right_pole, quality: con.quality };
    constructPointsGroup.add(lMarker);
    hoverTargets.push(lMarker);

    // Right pole label
    var rightDiv = document.createElement("div");
    rightDiv.className = rClass;
    rightDiv.textContent = con.right_pole;
    var rightLabel = new THREE.CSS2DObject(rightDiv);
    rightLabel.position.set(sc.rx, sc.ry, sc.rz);
    constructLabelsGroup.add(rightLabel);

    // Left pole label
    var leftDiv = document.createElement("div");
    leftDiv.className = lClass;
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
  // 3. ELEMENTS
  // =============================================
  var elSphereGeom = new THREE.SphereBufferGeometry(0.03, 16, 12);
  var elementObjects = [];

  for (var i = 0; i < elements.length; i++) {
    var el = elements[i];
    var mat = new THREE.MeshPhongMaterial({
      color: elementColor, shininess: 60, specular: 0x444444
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

  scene.add(new THREE.AmbientLight(0xffffff, 0.6));
  var dirLight = new THREE.DirectionalLight(0xffffff, 0.4);
  dirLight.position.set(2, 3, 2);
  scene.add(dirLight);

  // =============================================
  // 4. AXES
  // =============================================
  var axisLength = 1.3;
  var axisColors = [0xcc4444, 0x44aa44, 0x4444cc];
  var axisLabels = [];

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
    axisLabels.push(axLabel);
  }

  // =============================================
  // 5. PROJECTIONS (per-element, toggled by double-click)
  // =============================================
  // Per-element projection groups stored here
  var elementProjectionGroups = [];
  for (var i = 0; i < elements.length; i++) {
    var g = new THREE.Group();
    g.visible = false;
    projectionsGroup.add(g);
    elementProjectionGroups.push(g);
  }

  function buildProjectionsForElement(idx) {
    var group = elementProjectionGroups[idx];
    // Clear existing
    while (group.children.length > 0) {
      group.remove(group.children[0]);
    }
    if (!elementProjections[idx] || !elementVisible[idx]) {
      group.visible = false;
      return;
    }
    group.visible = true;
    var el = elements[idx];
    var eVec = new THREE.Vector3(el.x, el.y, el.z);
    var projColor = projColors[idx % projColors.length];

    for (var j = 0; j < constructs.length; j++) {
      if (!constructVisible[j] || !constructLineVisible[j]) continue;
      var con = constructs[j];
      var cVec = new THREE.Vector3(con.x, con.y, con.z);
      var cDir = cVec.clone().normalize();
      var foot = cDir.clone().multiplyScalar(eVec.dot(cDir));

      // Projection line from element to foot
      var projGeom = new THREE.BufferGeometry().setFromPoints([eVec, foot]);
      var projMat = new THREE.LineBasicMaterial({
        color: projColor, opacity: 0.7, transparent: true
      });
      var projLine = new THREE.Line(projGeom, projMat);
      group.add(projLine);

      // Small dot at projection foot
      var dotGeom = new THREE.SphereBufferGeometry(0.008, 6, 4);
      var dotMat = new THREE.MeshBasicMaterial({ color: projColor });
      var dot = new THREE.Mesh(dotGeom, dotMat);
      dot.position.copy(foot);
      group.add(dot);
    }
  }

  function rebuildAllProjections() {
    for (var i = 0; i < elements.length; i++) {
      buildProjectionsForElement(i);
    }
  }

  function toggleElementProjection(idx) {
    elementProjections[idx] = !elementProjections[idx];
    buildProjectionsForElement(idx);
  }

  // =============================================
  // 6. CALIBRATED AXES
  // =============================================
  function buildCalibration() {
    // Clear tick lines (direct children of calibrationGroup, skip labelsGroup)
    for (var k = calibrationGroup.children.length - 1; k >= 0; k--) {
      if (calibrationGroup.children[k] !== calibrationLabelsGroup) {
        calibrationGroup.remove(calibrationGroup.children[k]);
      }
    }
    // Clear tick labels
    while (calibrationLabelsGroup.children.length > 0) {
      calibrationLabelsGroup.remove(calibrationLabelsGroup.children[0]);
    }
    if (!calibration) return;

    var se = calibration.se;
    var offsets = calibration.offsets;
    var cCoords = calibration.construct_coords; // [nc][3] unscaled
    var vMin = meta.scale_min;
    var vMax = meta.scale_max;

    for (var i = 0; i < constructs.length; i++) {
      if (!constructVisible[i] || !constructLineVisible[i]) continue;

      var Ci = [cCoords[i][0], cCoords[i][1], cCoords[i][2]];
      var norm2 = Ci[0] * Ci[0] + Ci[1] * Ci[1] + Ci[2] * Ci[2];
      if (norm2 < 1e-10) continue;

      // Direction vector for tick perpendicular (we pick a world-up cross product)
      var cDir = new THREE.Vector3(Ci[0], Ci[1], Ci[2]).normalize();
      var up = new THREE.Vector3(0, 1, 0);
      var perp1 = new THREE.Vector3().crossVectors(cDir, up);
      if (perp1.length() < 0.01) {
        up.set(1, 0, 0);
        perp1.crossVectors(cDir, up);
      }
      perp1.normalize();
      var tickLen = 0.02;

      for (var v = vMin; v <= vMax; v++) {
        var vc = v - offsets[i];
        var factor = se * vc / norm2;
        var tx = factor * Ci[0];
        var ty = factor * Ci[1];
        var tz = factor * Ci[2];

        // Skip ticks too close to origin
        var dist = Math.sqrt(tx * tx + ty * ty + tz * tz);
        if (dist < 0.03) continue;

        // Tick line
        var t1 = new THREE.Vector3(
          tx - perp1.x * tickLen, ty - perp1.y * tickLen, tz - perp1.z * tickLen
        );
        var t2 = new THREE.Vector3(
          tx + perp1.x * tickLen, ty + perp1.y * tickLen, tz + perp1.z * tickLen
        );
        var tickGeom = new THREE.BufferGeometry().setFromPoints([t1, t2]);
        var tickMat = new THREE.LineBasicMaterial({ color: 0x666666, opacity: 0.5, transparent: true });
        calibrationGroup.add(new THREE.Line(tickGeom, tickMat));

        // Tick label
        var tickLabelDiv = document.createElement("div");
        tickLabelDiv.className = "label-calibration";
        tickLabelDiv.textContent = v;
        var tickLabel = new THREE.CSS2DObject(tickLabelDiv);
        tickLabel.position.set(
          tx + perp1.x * tickLen * 2.5,
          ty + perp1.y * tickLen * 2.5,
          tz + perp1.z * tickLen * 2.5
        );
        tickLabel.visible = calibrationLabelsVisible;
        calibrationLabelsGroup.add(tickLabel);
      }
    }
  }

  // =============================================
  // 7. BACK-FACE CULLING for construct labels
  // =============================================
  var _camDir = new THREE.Vector3();
  var _poleDir = new THREE.Vector3();
  // Allow poles up to 10° behind the view plane to remain visible
  var facingThreshold = Math.sin(10 * Math.PI / 180); // ~0.174

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
      // Construct line: visible if permanently toggled (double-click) or hovered
      var isHovered = (hoveredConstructIndex === i);
      constructObjects[i].line.visible = constructLineVisible[i] || isHovered;

      var sc = constructSphereCoords[i];

      // When hovered, force both poles visible; otherwise back-face cull with 10° tolerance
      if (isHovered) {
        constructObjects[i].rightLabel.visible = constructLabelsGroup.visible;
        constructObjects[i].rightMarker.visible = constructPointsGroup.visible;
        constructObjects[i].leftLabel.visible = constructLabelsGroup.visible;
        constructObjects[i].leftMarker.visible = constructPointsGroup.visible;
      } else {
        _poleDir.set(sc.rx, sc.ry, sc.rz).normalize();
        var rightFacing = _poleDir.dot(_camDir) < facingThreshold;
        constructObjects[i].rightLabel.visible = rightFacing && constructLabelsGroup.visible;
        constructObjects[i].rightMarker.visible = rightFacing && constructPointsGroup.visible;

        _poleDir.set(sc.lx, sc.ly, sc.lz).normalize();
        var leftFacing = _poleDir.dot(_camDir) < facingThreshold;
        constructObjects[i].leftLabel.visible = leftFacing && constructLabelsGroup.visible;
        constructObjects[i].leftMarker.visible = leftFacing && constructPointsGroup.visible;
      }
    }
  }

  // =============================================
  // 8. GRID TABLE
  // =============================================
  function buildGridTable() {
    var container = document.getElementById("grid-table-container");
    var title = document.createElement("h3");
    title.textContent = "Repertory Grid Data";
    container.appendChild(title);

    var table = document.createElement("table");
    table.className = "grid-table";
    table.id = "grid-table";

    var thead = document.createElement("thead");
    var headerRow = document.createElement("tr");

    var thLeft = document.createElement("th");
    thLeft.textContent = "Left Pole";
    thLeft.className = "col-neutral-pole";
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
    thRight.className = "col-neutral-pole";
    headerRow.appendChild(thRight);

    thead.appendChild(headerRow);
    table.appendChild(thead);

    var tbody = document.createElement("tbody");
    for (var c = 0; c < ratings.left_poles.length; c++) {
      var tr = document.createElement("tr");
      tr.dataset.constructIndex = c;

      var cPref = constructs[c] ? constructs[c].preferred : null;
      var leftPoleClass, rightPoleClass;
      if (cPref === "left") {
        leftPoleClass = "col-preferred-pole";
        rightPoleClass = "col-nonpreferred-pole";
      } else if (cPref === "right") {
        leftPoleClass = "col-nonpreferred-pole";
        rightPoleClass = "col-preferred-pole";
      } else if (cPref === "both") {
        leftPoleClass = "col-preferred-pole";
        rightPoleClass = "col-preferred-pole";
      } else {
        leftPoleClass = "col-neutral-pole";
        rightPoleClass = "col-neutral-pole";
      }

      var tdLeft = document.createElement("td");
      tdLeft.className = leftPoleClass;
      tdLeft.textContent = ratings.left_poles[c];
      tr.appendChild(tdLeft);

      var rowValues = ratings.values[c];
      for (var e = 0; e < ratings.element_names.length; e++) {
        var td = document.createElement("td");
        td.className = "rating-cell";
        td.dataset.elementIndex = e;
        td.dataset.constructIndex = c;
        td.textContent = rowValues[e];
        tr.appendChild(td);
      }

      var tdRight = document.createElement("td");
      tdRight.className = rightPoleClass;
      tdRight.textContent = ratings.right_poles[c];
      tr.appendChild(tdRight);

      tbody.appendChild(tr);
    }

    table.appendChild(tbody);
    container.appendChild(table);
  }
  buildGridTable();

  function highlightGridColumn(elementIndex) {
    var table = document.getElementById("grid-table");
    if (!table) return;
    var highlighted = table.querySelectorAll(".col-highlight");
    for (var k = 0; k < highlighted.length; k++) highlighted[k].classList.remove("col-highlight");
    if (elementIndex < 0) return;
    var headers = table.querySelectorAll("th.element-header");
    for (var k = 0; k < headers.length; k++) {
      if (parseInt(headers[k].dataset.elementIndex) === elementIndex) headers[k].classList.add("col-highlight");
    }
    var cells = table.querySelectorAll("td.rating-cell");
    for (var k = 0; k < cells.length; k++) {
      if (parseInt(cells[k].dataset.elementIndex) === elementIndex) cells[k].classList.add("col-highlight");
    }
  }

  function highlightGridRow(constructIndex) {
    var table = document.getElementById("grid-table");
    if (!table) return;
    var highlighted = table.querySelectorAll(".row-highlight");
    for (var k = 0; k < highlighted.length; k++) highlighted[k].classList.remove("row-highlight");
    if (constructIndex < 0) return;
    var rows = table.querySelectorAll("tbody tr");
    for (var k = 0; k < rows.length; k++) {
      if (parseInt(rows[k].dataset.constructIndex) === constructIndex) {
        var cells = rows[k].querySelectorAll("td");
        for (var j = 0; j < cells.length; j++) cells[j].classList.add("row-highlight");
      }
    }
  }

  // =============================================
  // 9. GUI PANEL
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
  addSectionTitle("Display");
  addToggle(guiPanel, "Wireframe Sphere", true, function (v) { sphereGroup.visible = v; });
  addToggle(guiPanel, "Axes", true, function (v) {
    axesGroup.visible = v;
    for (var a = 0; a < axisLabels.length; a++) {
      axisLabels[a].visible = v;
    }
  });
  addToggle(guiPanel, "Calibration Labels", true, function (v) {
    calibrationLabelsVisible = v;
    for (var k = 0; k < calibrationLabelsGroup.children.length; k++) {
      calibrationLabelsGroup.children[k].visible = v;
    }
  });

  // Hint text
  var hint = document.createElement("div");
  hint.className = "hint-text";
  hint.textContent = "Double-click element \u2192 projections. Double-click construct \u2192 axis + calibration.";
  guiPanel.appendChild(hint);

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
        buildProjectionsForElement(idx);
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
      var labelText = constructs[idx].left_pole + " \u2014 " + constructs[idx].right_pole;
      var cb = addToggle(conListDiv, labelText, true, function (v) {
        constructVisible[idx] = v;
        rebuildAllProjections();
        buildCalibration();
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
  // 10. HOVER + TOOLTIP + DOUBLE-CLICK
  // =============================================
  function highlightElement(idx) {
    if (idx < 0) return;
    elementObjects[idx].sphere.scale.setScalar(1.8);
    elementObjects[idx].sphere.material.emissive.setHex(0x444444);
    elementObjects[idx].label.element.style.fontWeight = "800";
    elementObjects[idx].label.element.style.color = "#000";
  }

  function unhighlightElement(idx) {
    if (idx < 0) return;
    elementObjects[idx].sphere.scale.setScalar(1.0);
    elementObjects[idx].sphere.material.emissive.setHex(0x000000);
    elementObjects[idx].label.element.style.fontWeight = "";
    elementObjects[idx].label.element.style.color = "";
  }

  function highlightConstruct(idx) {
    if (idx < 0) return;
    var obj = constructObjects[idx];
    obj.rightMarker.scale.setScalar(2.5);
    obj.leftMarker.scale.setScalar(2.5);
    obj.rightLabel.element.style.fontWeight = "800";
    obj.leftLabel.element.style.fontWeight = "800";
    obj.rightLabel.element.style.textShadow = "0 0 3px rgba(0,0,0,0.3)";
    obj.leftLabel.element.style.textShadow = "0 0 3px rgba(0,0,0,0.3)";
    // Force both poles visible while hovering (including back-facing one)
    obj.rightLabel.visible = true;
    obj.leftLabel.visible = true;
    obj.rightMarker.visible = true;
    obj.leftMarker.visible = true;
  }

  function unhighlightConstruct(idx) {
    if (idx < 0) return;
    var obj = constructObjects[idx];
    obj.rightMarker.scale.setScalar(1.0);
    obj.leftMarker.scale.setScalar(1.0);
    obj.rightLabel.element.style.fontWeight = "";
    obj.leftLabel.element.style.fontWeight = "";
    obj.rightLabel.element.style.textShadow = "";
    obj.leftLabel.element.style.textShadow = "";
  }

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
          ? " (quality: " + (ud.quality * 100).toFixed(1) + "%)" : "";
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
      unhighlightElement(hoveredElementIndex);
      hoveredElementIndex = newHoveredElement;
      highlightElement(hoveredElementIndex);
      highlightGridColumn(hoveredElementIndex);
    }
    if (newHoveredConstruct !== hoveredConstructIndex) {
      unhighlightConstruct(hoveredConstructIndex);
      hoveredConstructIndex = newHoveredConstruct;
      highlightConstruct(hoveredConstructIndex);
      highlightGridRow(hoveredConstructIndex);
    }
    renderer.domElement.style.cursor = (newHoveredElement >= 0 || newHoveredConstruct >= 0) ? "pointer" : "default";
  }

  // Double-click: toggle projections for element, toggle line for construct
  function onDblClick(event) {
    var rect = renderer.domElement.getBoundingClientRect();
    mouse.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
    mouse.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;

    raycaster.setFromCamera(mouse, camera);
    var intersects = raycaster.intersectObjects(hoverTargets);

    if (intersects.length > 0) {
      var ud = intersects[0].object.userData;
      if (ud.type === "element") {
        toggleElementProjection(ud.index);
      } else if (ud.type === "construct") {
        constructLineVisible[ud.index] = !constructLineVisible[ud.index];
        buildCalibration();
        rebuildAllProjections();
      }
    }
  }

  renderer.domElement.addEventListener("mousemove", onMouseMove, false);
  renderer.domElement.addEventListener("dblclick", onDblClick, false);
  renderer.domElement.addEventListener("mouseleave", function () {
    tooltip.style.display = "none";
    unhighlightElement(hoveredElementIndex);
    unhighlightConstruct(hoveredConstructIndex);
    hoveredElementIndex = -1;
    hoveredConstructIndex = -1;
    highlightGridColumn(-1);
    highlightGridRow(-1);
    renderer.domElement.style.cursor = "default";
  });

  // =============================================
  // 11. RESIZE
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
  // 12. PANEL TOGGLES + RESIZABLE DIVIDERS
  // =============================================
  var leftPanel = document.getElementById("left-panel");
  var rightPanel = document.getElementById("right-panel");
  var toggleLeftBtn = document.getElementById("toggle-left");
  var toggleRightBtn = document.getElementById("toggle-right");
  var resizeLeft = document.getElementById("resize-left");
  var resizeRight = document.getElementById("resize-right");

  toggleLeftBtn.addEventListener("click", function () {
    if (leftPanel.classList.contains("collapsed")) {
      leftPanel.classList.remove("collapsed");
      leftPanel.style.width = leftPanel._savedWidth || "";
      resizeLeft.style.display = "";
    } else {
      leftPanel._savedWidth = leftPanel.style.width;
      leftPanel.style.width = "";
      leftPanel.classList.add("collapsed");
      resizeLeft.style.display = "none";
    }
    onResize();
  });

  toggleRightBtn.addEventListener("click", function () {
    if (rightPanel.classList.contains("collapsed")) {
      rightPanel.classList.remove("collapsed");
      rightPanel.style.width = rightPanel._savedWidth || "";
      resizeRight.style.display = "";
    } else {
      rightPanel._savedWidth = rightPanel.style.width;
      rightPanel.style.width = "";
      rightPanel.classList.add("collapsed");
      resizeRight.style.display = "none";
    }
    onResize();
  });

  // Draggable dividers to resize panels
  function makeResizable(handle, panel, side) {
    var isDragging = false;

    handle.addEventListener("mousedown", function (e) {
      isDragging = true;
      handle.classList.add("active");
      document.body.style.cursor = "col-resize";
      document.body.style.userSelect = "none";
      e.preventDefault();
    });

    document.addEventListener("mousemove", function (e) {
      if (!isDragging) return;
      if (side === "left") {
        var newWidth = Math.max(150, Math.min(e.clientX, window.innerWidth - 400));
        panel.style.width = newWidth + "px";
      } else {
        var newWidth = Math.max(150, Math.min(window.innerWidth - e.clientX, window.innerWidth - 400));
        panel.style.width = newWidth + "px";
      }
      onResize();
    });

    document.addEventListener("mouseup", function () {
      if (!isDragging) return;
      isDragging = false;
      handle.classList.remove("active");
      document.body.style.cursor = "";
      document.body.style.userSelect = "";
    });
  }

  makeResizable(resizeLeft, leftPanel, "left");
  makeResizable(resizeRight, rightPanel, "right");

  // =============================================
  // 13. CONSTRUCT LABEL ALIGNMENT (outward-facing)
  // =============================================
  var _projVec = new THREE.Vector3();

  function updateLabelAlignment() {
    // Project globe center to screen (NDC x)
    _projVec.set(0, 0, 0).project(camera);
    var centerX = _projVec.x;
    // Regex handles browser normalization: translate(-50%,-50%) may become translate(-50%, -50%)
    var alignRe = /translate\(-50%,\s*-50%\)/;

    for (var i = 0; i < constructs.length; i++) {
      if (!constructVisible[i]) continue;
      var sc = constructSphereCoords[i];

      // Right pole label — nudge outward by a few pixels so text doesn't overlap the marker
      if (constructObjects[i].rightLabel.visible) {
        _projVec.set(sc.rx, sc.ry, sc.rz).project(camera);
        var rLeft = _projVec.x < centerX;
        var rAlign = rLeft ? "-100%" : "0%";
        var rNudge = rLeft ? -3 : 3;
        var rEl = constructObjects[i].rightLabel.element;
        rEl.style.transform = rEl.style.transform.replace(alignRe, "translate(" + rAlign + ", -50%) translate(" + rNudge + "px, 0px)");
      }

      // Left pole label
      if (constructObjects[i].leftLabel.visible) {
        _projVec.set(sc.lx, sc.ly, sc.lz).project(camera);
        var lLeft = _projVec.x < centerX;
        var lAlign = lLeft ? "-100%" : "0%";
        var lNudge = lLeft ? -3 : 3;
        var lEl = constructObjects[i].leftLabel.element;
        lEl.style.transform = lEl.style.transform.replace(alignRe, "translate(" + lAlign + ", -50%) translate(" + lNudge + "px, 0px)");
      }
    }
  }

  // =============================================
  // 14. RENDER LOOP
  // =============================================
  function animate() {
    requestAnimationFrame(animate);
    controls.update();
    updateLabelVisibility();
    renderer.render(scene, camera);
    labelRenderer.render(scene, camera);
    updateLabelAlignment();
  }
  animate();

})();
