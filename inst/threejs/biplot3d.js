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
  var elementLabelVisible = elements.map(function () { return true; });
  var constructVisible = constructs.map(function () { return true; });
  var elementProjections = elements.map(function () { return false; }); // per-element projection toggle
  var constructLineVisible = constructs.map(function () { return false; }); // per-construct line toggle
  var constructLabelVisible = constructs.map(function () { return true; }); // per-construct label toggle
  var calibrationLabelsVisible = true;

  // --- Scene setup ---
  var sceneContainer = document.getElementById("scene-container");
  var width = sceneContainer.clientWidth;
  var height = sceneContainer.clientHeight;

  var scene = new THREE.Scene();
  document.body.classList.add("dark");
  scene.background = new THREE.Color(0x1a1a1a);

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
  var elementColor = 0xbbbbbb;
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
  var gridPointsPerCurve = 96; // smooth curves regardless of line count
  var wireUniforms = {
    uColor: { value: new THREE.Color(0x4a4a4a) },
    uOpacityFront: { value: 0.25 },
    uOpacityBack: { value: 0.04 },
    uCamDir: { value: new THREE.Vector3(0, 0, -1) },
    uDepthFade: { value: 0.0 }
  };
  var wireMat = new THREE.ShaderMaterial({
    transparent: true,
    uniforms: wireUniforms,
    vertexShader: [
      "varying vec3 vWorldNormal;",
      "void main() {",
      "  vWorldNormal = normalize((modelMatrix * vec4(position, 1.0)).xyz);",
      "  gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);",
      "}"
    ].join("\n"),
    fragmentShader: [
      "uniform vec3 uColor;",
      "uniform float uOpacityFront;",
      "uniform float uOpacityBack;",
      "uniform vec3 uCamDir;",
      "uniform float uDepthFade;",
      "varying vec3 vWorldNormal;",
      "void main() {",
      "  float facing = dot(vWorldNormal, -uCamDir);",
      "  float t = clamp(facing * 0.5 + 0.5, 0.0, 1.0);",
      "  float opacity = mix(uOpacityFront, mix(uOpacityBack, uOpacityFront, t), uDepthFade);",
      "  gl_FragColor = vec4(uColor, opacity);",
      "}"
    ].join("\n")
  });

  function buildGridLines(nLon, nLat) {
    // Clear existing lines
    while (sphereGroup.children.length > 0) {
      sphereGroup.children[0].geometry.dispose();
      sphereGroup.remove(sphereGroup.children[0]);
    }
    // Longitude lines (vertical great circles)
    for (var i = 0; i < nLon; i++) {
      var phi = (i / nLon) * Math.PI * 2;
      var pts = [];
      for (var j = 0; j <= gridPointsPerCurve; j++) {
        var theta = (j / gridPointsPerCurve) * Math.PI;
        pts.push(new THREE.Vector3(
          sphereRadius * Math.sin(theta) * Math.cos(phi),
          sphereRadius * Math.cos(theta),
          sphereRadius * Math.sin(theta) * Math.sin(phi)
        ));
      }
      var geom = new THREE.BufferGeometry().setFromPoints(pts);
      sphereGroup.add(new THREE.Line(geom, wireMat));
    }
    // Latitude lines (horizontal circles)
    for (var i = 1; i < nLat; i++) {
      var theta = (i / nLat) * Math.PI;
      var r = sphereRadius * Math.sin(theta);
      var y = sphereRadius * Math.cos(theta);
      var pts = [];
      for (var j = 0; j <= gridPointsPerCurve; j++) {
        var phi = (j / gridPointsPerCurve) * Math.PI * 2;
        pts.push(new THREE.Vector3(r * Math.cos(phi), y, r * Math.sin(phi)));
      }
      var geom = new THREE.BufferGeometry().setFromPoints(pts);
      sphereGroup.add(new THREE.Line(geom, wireMat));
    }
  }
  var defaultLon = 32;
  var defaultLat = 24;
  buildGridLines(defaultLon, defaultLat);

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

    // Line from left to right pole (on sphere) — uses cylinder mesh for variable thickness
    var from = new THREE.Vector3(sc.lx, sc.ly, sc.lz);
    var to = new THREE.Vector3(sc.rx, sc.ry, sc.rz);
    var mid = new THREE.Vector3().addVectors(from, to).multiplyScalar(0.5);
    var lineLen = from.distanceTo(to);
    var cylGeom = new THREE.CylinderBufferGeometry(0.002, 0.002, lineLen, 4, 1);
    var lineMat = new THREE.MeshBasicMaterial({
      color: 0x888888, opacity: 0.6, transparent: true
    });
    var line = new THREE.Mesh(cylGeom, lineMat);
    line.position.copy(mid);
    // Orient cylinder from default Y-axis to the from→to direction
    var lineDir = new THREE.Vector3().subVectors(to, from).normalize();
    var quat = new THREE.Quaternion().setFromUnitVectors(new THREE.Vector3(0, 1, 0), lineDir);
    line.quaternion.copy(quat);
    line.visible = false;
    line.scale.set(2, 1, 2);
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
  var glowSphereGeom = new THREE.SphereBufferGeometry(0.055, 16, 12);
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

    var glowMat = new THREE.MeshBasicMaterial({
      color: elementColor, transparent: true, opacity: 0.25,
      blending: THREE.AdditiveBlending, depthWrite: false
    });
    var glow = new THREE.Mesh(glowSphereGeom, glowMat);
    glow.position.set(el.x, el.y, el.z);
    glow.visible = false;
    elementPointsGroup.add(glow);

    var labelDiv = document.createElement("div");
    labelDiv.className = "label-element";
    labelDiv.textContent = el.name;
    labelDiv.style.color = "#bbbbbb";
    var label = new THREE.CSS2DObject(labelDiv);
    label.position.set(el.x, el.y + 0.05, el.z);
    elementLabelsGroup.add(label);

    elementObjects.push({ sphere: sphere, label: label, glow: glow });
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
  var pcAxisMeshes = [];

  for (var a = 0; a < 3; a++) {
    var end = new THREE.Vector3(0, 0, 0);
    var negEnd = new THREE.Vector3(0, 0, 0);
    end.setComponent(a, axisLength);
    negEnd.setComponent(a, -axisLength);

    // Positive half — solid cylinder
    var posMid = end.clone().multiplyScalar(0.5);
    var posCyl = new THREE.CylinderBufferGeometry(0.003, 0.003, axisLength, 4, 1);
    var posMat = new THREE.MeshBasicMaterial({ color: axisColors[a], opacity: 0.4, transparent: true });
    var posMesh = new THREE.Mesh(posCyl, posMat);
    posMesh.position.copy(posMid);
    var posDir = end.clone().normalize();
    posMesh.quaternion.setFromUnitVectors(new THREE.Vector3(0, 1, 0), posDir);
    axesGroup.add(posMesh);
    pcAxisMeshes.push(posMesh);

    // Negative half — dashed line (keep as line, thickness not critical for dashed)
    var negGeom = new THREE.BufferGeometry().setFromPoints([new THREE.Vector3(0,0,0), negEnd]);
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
  var projLineScale = 2;
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

      // Projection line from element to foot (cylinder mesh for variable thickness)
      var projDist = eVec.distanceTo(foot);
      if (projDist < 0.0001) continue;
      var projMid = new THREE.Vector3().addVectors(eVec, foot).multiplyScalar(0.5);
      var projCylGeom = new THREE.CylinderBufferGeometry(0.0015, 0.0015, projDist, 4, 1);
      var projMat = new THREE.MeshBasicMaterial({
        color: projColor, opacity: 0.7, transparent: true
      });
      var projLine = new THREE.Mesh(projCylGeom, projMat);
      projLine.position.copy(projMid);
      var projDir = new THREE.Vector3().subVectors(foot, eVec).normalize();
      projLine.quaternion.setFromUnitVectors(new THREE.Vector3(0, 1, 0), projDir);
      projLine.scale.set(projLineScale, 1, projLineScale);
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
        if (typeof calSizeRange !== "undefined") {
          var calVal = parseInt(calSizeRange.value);
          tickLabelDiv.style.fontSize = calVal + "px";
          if (calVal === 0) tickLabelDiv.style.display = "none";
        }
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
      var isTempVisible = (profileTempVisibleConstruct === i);
      if (!constructVisible[i] && !isTempVisible) {
        constructObjects[i].rightLabel.visible = false;
        constructObjects[i].leftLabel.visible = false;
        constructObjects[i].rightMarker.visible = false;
        constructObjects[i].leftMarker.visible = false;
        constructObjects[i].line.visible = false;
        continue;
      }
      // Construct line: visible if permanently toggled (double-click), hovered, or temp-visible
      var isHovered = (hoveredConstructIndex === i);
      constructObjects[i].line.visible = constructLineVisible[i] || isHovered;

      var sc = constructSphereCoords[i];

      // When hovered, force both poles visible; otherwise back-face cull with 10° tolerance
      var clv = constructLabelVisible[i];
      if (isHovered) {
        constructObjects[i].rightLabel.visible = true;
        constructObjects[i].rightMarker.visible = true;
        constructObjects[i].leftLabel.visible = true;
        constructObjects[i].leftMarker.visible = true;
      } else {
        _poleDir.set(sc.rx, sc.ry, sc.rz).normalize();
        var rightFacing = _poleDir.dot(_camDir) < facingThreshold;
        constructObjects[i].rightLabel.visible = rightFacing && clv;
        constructObjects[i].rightMarker.visible = rightFacing && constructPointsGroup.visible;

        _poleDir.set(sc.lx, sc.ly, sc.lz).normalize();
        var leftFacing = _poleDir.dot(_camDir) < facingThreshold;
        constructObjects[i].leftLabel.visible = leftFacing && clv;
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

  // --- Grid table construct interactions ---
  var gridHoveredConstruct = -1;

  function setupGridTableConstructEvents() {
    var rows = document.querySelectorAll("#grid-table tbody tr");
    for (var r = 0; r < rows.length; r++) {
      (function (row) {
        var ci = parseInt(row.dataset.constructIndex);

        row.addEventListener("mouseenter", function () {
          if (ci === gridHoveredConstruct) return;
          if (gridHoveredConstruct >= 0) {
            unhighlightConstruct(gridHoveredConstruct);
          }
          gridHoveredConstruct = ci;
          profileTempVisibleConstruct = ci;
          hoveredConstructIndex = ci;
          highlightConstruct(ci);
          highlightGridRow(ci);
          if (selectedElementIndex >= 0) updateProfilePlot(selectedElementIndex);
        });

        row.addEventListener("mouseleave", function () {
          if (gridHoveredConstruct >= 0) {
            unhighlightConstruct(gridHoveredConstruct);
            highlightGridRow(-1);
            profileTempVisibleConstruct = -1;
            hoveredConstructIndex = -1;
            gridHoveredConstruct = -1;
            if (selectedElementIndex >= 0) updateProfilePlot(selectedElementIndex);
          }
        });

        row.addEventListener("click", function () {
          conCheckboxes[ci].checked = !conCheckboxes[ci].checked;
          conCheckboxes[ci].dispatchEvent(new Event("change"));
        });

        row.addEventListener("dblclick", function () {
          if (!constructVisible[ci]) {
            conCheckboxes[ci].checked = true;
            conCheckboxes[ci].dispatchEvent(new Event("change"));
          }
          constructLineVisible[ci] = !constructLineVisible[ci];
          buildCalibration();
          rebuildAllProjections();
        });

        row.style.cursor = "pointer";
      })(rows[r]);
    }
  }
  setupGridTableConstructEvents();

  // --- Grid table cell double-click → show projection ---
  (function () {
    var cells = document.querySelectorAll("#grid-table td.rating-cell");
    for (var c = 0; c < cells.length; c++) {
      (function (td) {
        td.addEventListener("dblclick", function (e) {
          e.stopPropagation(); // prevent row dblclick
          var ei = parseInt(td.dataset.elementIndex);
          var ci = parseInt(td.dataset.constructIndex);
          // Ensure element is visible
          if (!elemCheckboxes[ei].checked) {
            elemCheckboxes[ei].checked = true;
            elemCheckboxes[ei].dispatchEvent(new Event("change"));
          }
          // Ensure construct axis is visible
          if (!constructVisible[ci]) {
            conCheckboxes[ci].checked = true;
            conCheckboxes[ci].dispatchEvent(new Event("change"));
          }
          if (!constructLineVisible[ci]) {
            constructLineVisible[ci] = true;
            buildCalibration();
          }
          // Enable projections for this element
          if (!elementProjections[ei]) {
            elementProjections[ei] = true;
            buildProjectionsForElement(ei);
          } else {
            rebuildAllProjections();
          }
        });
      })(cells[c]);
    }
  })();

  // --- Grid table element header interactions ---
  var gridHoveredElement = -1;
  (function () {
    var headers = document.querySelectorAll("#grid-table th.element-header");
    for (var h = 0; h < headers.length; h++) {
      (function (th) {
        var ei = parseInt(th.dataset.elementIndex);
        th.style.cursor = "pointer";

        th.addEventListener("mouseenter", function () {
          if (ei === gridHoveredElement) return;
          if (gridHoveredElement >= 0) unhighlightElement(gridHoveredElement);
          gridHoveredElement = ei;
          highlightElement(ei);
          highlightGridColumn(ei);
          updateProfilePlot(ei);
        });

        th.addEventListener("mouseleave", function () {
          if (gridHoveredElement >= 0) {
            unhighlightElement(gridHoveredElement);
            highlightGridColumn(-1);
            gridHoveredElement = -1;
          }
        });
      })(headers[h]);
    }
  })();

  // --- Tab switching ---
  var tabBtns = document.querySelectorAll(".tab-btn");
  var tabContents = document.querySelectorAll(".tab-content");
  for (var t = 0; t < tabBtns.length; t++) {
    (function (btn) {
      btn.addEventListener("click", function () {
        for (var k = 0; k < tabBtns.length; k++) tabBtns[k].classList.remove("active");
        for (var k = 0; k < tabContents.length; k++) tabContents[k].classList.remove("active");
        btn.classList.add("active");
        var tab = btn.dataset.tab;
        var content = document.querySelector('.tab-content[data-tab="' + tab + '"]');
        if (content) content.classList.add("active");
        if (tab === "profile" && selectedElementIndex >= 0) drawProfilePlot(selectedElementIndex);
      });
    })(tabBtns[t]);
  }

  // --- Profile plot ---
  var profileCanvas = document.getElementById("profile-canvas");
  var profileCtx = profileCanvas.getContext("2d");
  var profileHint = document.querySelector("#profile-container .profile-hint");
  profileCanvas.style.display = "none";
  var removeBenchmarksBtn = document.getElementById("remove-benchmarks-btn");
  removeBenchmarksBtn.addEventListener("click", function () {
    benchmarkElements = [];
    removeBenchmarksBtn.style.display = "none";
    updateElementGlows();
    if (selectedElementIndex >= 0) drawProfilePlot(selectedElementIndex);
  });
  var selectedElementIndex = -1;
  var profileLayout = { topPad: 0, rowHeight: 0, nc: 0 };
  var profileHoveredConstruct = -1;
  var profileTempVisibleConstruct = -1; // construct temporarily made visible by profile hover
  var benchmarkElements = []; // indices of benchmark elements

  // Compute construct order by angle in PC1-PC2 plane
  var constructOrder = constructs.map(function (c, i) {
    return { index: i, angle: Math.atan2(c.y, c.x) };
  });
  constructOrder.sort(function (a, b) { return a.angle - b.angle; });

  // Word-wrap text into up to maxLines lines fitting within maxWidth.
  // Last line is truncated with "…" if it still overflows.
  function wrapText(ctx, text, maxWidth, maxLines) {
    var words = text.split(/\s+/);
    var lines = [];
    var current = words[0] || "";
    for (var i = 1; i < words.length; i++) {
      var test = current + " " + words[i];
      if (ctx.measureText(test).width <= maxWidth) {
        current = test;
      } else {
        lines.push(current);
        current = words[i];
        if (lines.length >= maxLines) { current = ""; break; }
      }
    }
    if (current) lines.push(current);
    if (lines.length === 0) lines.push(text);
    // Truncate last line if needed
    var last = lines[lines.length - 1];
    if (ctx.measureText(last).width > maxWidth) {
      while (last.length > 1 && ctx.measureText(last + "\u2026").width > maxWidth) {
        last = last.slice(0, -1);
      }
      lines[lines.length - 1] = last + "\u2026";
    }
    return lines.slice(0, maxLines);
  }

  function drawProfilePlot(elemIdx) {
    if (elemIdx < 0) return;
    var isDark = document.body.classList.contains("dark");
    var container = document.getElementById("profile-container");
    var containerWidth = container.clientWidth - 24; // subtract padding
    if (containerWidth < 100) containerWidth = 280;

    var nc = constructs.length;
    var scaleMin = meta.scale_min;
    var scaleMax = meta.scale_max;
    var scaleRange = scaleMax - scaleMin;

    // Layout — give poles ~35% of width each, at least 60px center
    var marginFrac = 0.35;
    var leftMargin = Math.max(60, Math.floor(containerWidth * marginFrac));
    var rightMargin = leftMargin;
    var plotWidth = containerWidth - leftMargin - rightMargin;
    if (plotWidth < 60) { leftMargin = Math.floor((containerWidth - 60) / 2); rightMargin = leftMargin; plotWidth = containerWidth - leftMargin - rightMargin; }
    var poleFont = "10px -apple-system, BlinkMacSystemFont, sans-serif";
    var lineHeight = 12;
    var rowHeight = 32;
    var topPad = 42;
    var bottomPad = benchmarkElements.length > 0 ? 36 : 20;
    var canvasHeight = topPad + nc * rowHeight + bottomPad;

    var dpr = window.devicePixelRatio || 1;
    profileCanvas.width = containerWidth * dpr;
    profileCanvas.height = canvasHeight * dpr;
    profileCanvas.style.width = containerWidth + "px";
    profileCanvas.style.height = canvasHeight + "px";
    profileCtx.setTransform(dpr, 0, 0, dpr, 0, 0);

    // Clear
    profileCtx.clearRect(0, 0, containerWidth, canvasHeight);

    // Colors
    var textColor = isDark ? "#ccc" : "#333";
    var lineColor = elemColorInput ? elemColorInput.value : (isDark ? "#5599dd" : "#2266aa");
    var gridColor = isDark ? "#444" : "#e0e0e0";
    var bgColor = isDark ? "transparent" : "transparent";
    var prefColor = isDark ? "#44bb77" : "#226644";
    var nonprefColor = isDark ? "#dd7755" : "#aa4422";
    var neutralColor = isDark ? "#999" : "#777";
    var dotFill = lineColor;
    var midColor = isDark ? "#555" : "#ccc";

    // Title
    profileCtx.font = "bold 14px -apple-system, BlinkMacSystemFont, sans-serif";
    profileCtx.fillStyle = isDark ? "#ddd" : "#222";
    profileCtx.textAlign = "center";
    profileCtx.fillText(elements[elemIdx].name, containerWidth / 2, 18);

    // Scale ticks at top
    profileCtx.font = "9px -apple-system, BlinkMacSystemFont, sans-serif";
    profileCtx.fillStyle = isDark ? "#888" : "#999";
    profileCtx.textAlign = "center";
    for (var s = scaleMin; s <= scaleMax; s++) {
      var sx = leftMargin + (s - scaleMin) / scaleRange * plotWidth;
      profileCtx.fillText(s, sx, topPad - 4);
    }

    // Draw rows
    var points = [];
    for (var r = 0; r < nc; r++) {
      var ci = constructOrder[r].index;
      var y = topPad + r * rowHeight + rowHeight / 2;
      var rating = ratings.values[ci][elemIdx];

      // Row highlight when construct is hovered
      if (ci === hoveredConstructIndex) {
        profileCtx.fillStyle = isDark ? "rgba(80,160,255,0.12)" : "rgba(0,100,200,0.08)";
        profileCtx.fillRect(0, y - rowHeight / 2, containerWidth, rowHeight);
      }

      // Horizontal grid line
      profileCtx.strokeStyle = gridColor;
      profileCtx.lineWidth = 0.5;
      profileCtx.beginPath();
      profileCtx.moveTo(leftMargin, y);
      profileCtx.lineTo(leftMargin + plotWidth, y);
      profileCtx.stroke();

      // Midpoint marker
      var midX = leftMargin + ((scaleMin + scaleMax) / 2 - scaleMin) / scaleRange * plotWidth;
      profileCtx.strokeStyle = midColor;
      profileCtx.lineWidth = 0.5;
      profileCtx.beginPath();
      profileCtx.moveTo(midX, y - rowHeight / 2 + 2);
      profileCtx.lineTo(midX, y + rowHeight / 2 - 2);
      profileCtx.stroke();

      // Pole labels
      var cPref = constructs[ci].preferred;
      var leftPoleColor, rightPoleColor;
      if (cPref === "left") {
        leftPoleColor = prefColor;
        rightPoleColor = nonprefColor;
      } else if (cPref === "right") {
        leftPoleColor = nonprefColor;
        rightPoleColor = prefColor;
      } else if (cPref === "both") {
        leftPoleColor = prefColor;
        rightPoleColor = prefColor;
      } else {
        leftPoleColor = neutralColor;
        rightPoleColor = neutralColor;
      }

      // Left pole — word-wrap up to 2 lines, then truncate
      profileCtx.font = poleFont;
      profileCtx.textAlign = "right";
      profileCtx.fillStyle = leftPoleColor;
      var leftLines = wrapText(profileCtx, constructs[ci].left_pole, leftMargin - 10, 2);
      var lyStart = y - (leftLines.length - 1) * lineHeight / 2 + 3;
      for (var li = 0; li < leftLines.length; li++) {
        profileCtx.fillText(leftLines[li], leftMargin - 6, lyStart + li * lineHeight);
      }

      // Right pole
      profileCtx.textAlign = "left";
      profileCtx.fillStyle = rightPoleColor;
      var rightLines = wrapText(profileCtx, constructs[ci].right_pole, rightMargin - 10, 2);
      var ryStart = y - (rightLines.length - 1) * lineHeight / 2 + 3;
      for (var ri = 0; ri < rightLines.length; ri++) {
        profileCtx.fillText(rightLines[ri], leftMargin + plotWidth + 6, ryStart + ri * lineHeight);
      }

      // Compute dot position
      if (rating != null && !isNaN(rating)) {
        var dx = leftMargin + (rating - scaleMin) / scaleRange * plotWidth;
        points.push({ x: dx, y: y, rating: rating });
      }
    }

    // Draw benchmark profiles (behind main profile)
    var benchColors = ["#e6194b", "#f58231", "#911eb4", "#42d4f4", "#3cb44b"];
    for (var bi = 0; bi < benchmarkElements.length; bi++) {
      var bIdx = benchmarkElements[bi];
      if (bIdx === elemIdx) continue; // skip if same as main
      var bPoints = [];
      for (var r = 0; r < nc; r++) {
        var ci = constructOrder[r].index;
        var bRating = ratings.values[ci][bIdx];
        var by = topPad + r * rowHeight + rowHeight / 2;
        if (bRating != null && !isNaN(bRating)) {
          var bx = leftMargin + (bRating - scaleMin) / scaleRange * plotWidth;
          bPoints.push({ x: bx, y: by });
        }
      }
      var bColor = benchColors[bi % benchColors.length];
      // Dashed line
      if (bPoints.length > 1) {
        profileCtx.strokeStyle = bColor;
        profileCtx.lineWidth = 1.2;
        profileCtx.setLineDash([4, 3]);
        profileCtx.globalAlpha = 0.7;
        profileCtx.beginPath();
        profileCtx.moveTo(bPoints[0].x, bPoints[0].y);
        for (var bp = 1; bp < bPoints.length; bp++) {
          profileCtx.lineTo(bPoints[bp].x, bPoints[bp].y);
        }
        profileCtx.stroke();
        profileCtx.setLineDash([]);
        profileCtx.globalAlpha = 1.0;
      }
      // Small dots
      for (var bp = 0; bp < bPoints.length; bp++) {
        profileCtx.beginPath();
        profileCtx.arc(bPoints[bp].x, bPoints[bp].y, 2.5, 0, Math.PI * 2);
        profileCtx.fillStyle = bColor;
        profileCtx.globalAlpha = 0.7;
        profileCtx.fill();
        profileCtx.globalAlpha = 1.0;
      }
    }

    // Draw connecting line (main element)
    if (points.length > 1) {
      profileCtx.strokeStyle = lineColor;
      profileCtx.lineWidth = 1.5;
      profileCtx.beginPath();
      profileCtx.moveTo(points[0].x, points[0].y);
      for (var p = 1; p < points.length; p++) {
        profileCtx.lineTo(points[p].x, points[p].y);
      }
      profileCtx.stroke();
    }

    // Draw dots (main element)
    for (var p = 0; p < points.length; p++) {
      profileCtx.beginPath();
      profileCtx.arc(points[p].x, points[p].y, 3.5, 0, Math.PI * 2);
      profileCtx.fillStyle = dotFill;
      profileCtx.fill();
      profileCtx.strokeStyle = isDark ? "#222" : "#fff";
      profileCtx.lineWidth = 1;
      profileCtx.stroke();
    }

    // Legend for benchmarks
    if (benchmarkElements.length > 0) {
      var legendY = topPad + nc * rowHeight + 10;
      profileCtx.font = "9px -apple-system, BlinkMacSystemFont, sans-serif";
      var legendX = leftMargin;
      for (var bi = 0; bi < benchmarkElements.length; bi++) {
        var bIdx = benchmarkElements[bi];
        var bColor = benchColors[bi % benchColors.length];
        profileCtx.setLineDash([4, 3]);
        profileCtx.strokeStyle = bColor;
        profileCtx.lineWidth = 1.2;
        profileCtx.globalAlpha = 0.7;
        profileCtx.beginPath();
        profileCtx.moveTo(legendX, legendY);
        profileCtx.lineTo(legendX + 16, legendY);
        profileCtx.stroke();
        profileCtx.setLineDash([]);
        profileCtx.globalAlpha = 1.0;
        profileCtx.fillStyle = isDark ? "#bbb" : "#555";
        profileCtx.textAlign = "left";
        profileCtx.fillText(elements[bIdx].name, legendX + 20, legendY + 3);
        legendX += 24 + profileCtx.measureText(elements[bIdx].name).width + 10;
      }
    }

    // Vertical border lines at scale edges
    profileCtx.strokeStyle = gridColor;
    profileCtx.lineWidth = 0.5;
    profileCtx.beginPath();
    profileCtx.moveTo(leftMargin, topPad);
    profileCtx.lineTo(leftMargin, topPad + nc * rowHeight);
    profileCtx.stroke();
    profileCtx.beginPath();
    profileCtx.moveTo(leftMargin + plotWidth, topPad);
    profileCtx.lineTo(leftMargin + plotWidth, topPad + nc * rowHeight);
    profileCtx.stroke();

    profileHint.style.display = "none";
    profileCanvas.style.display = "block";
    removeBenchmarksBtn.style.display = benchmarkElements.length > 0 ? "block" : "none";
    profileLayout = { topPad: topPad, rowHeight: rowHeight, nc: nc };
  }

  function updateElementGlows() {
    for (var i = 0; i < elementObjects.length; i++) {
      var active = (i === selectedElementIndex) || (benchmarkElements.indexOf(i) >= 0);
      elementObjects[i].glow.visible = active && elementVisible[i];
    }
  }

  function updateProfilePlot(elemIdx) {
    selectedElementIndex = elemIdx;
    updateElementGlows();
    var profileTab = document.querySelector('.tab-content[data-tab="profile"]');
    if (profileTab && profileTab.classList.contains("active")) {
      if (elemIdx >= 0) {
        drawProfilePlot(elemIdx);
      }
    }
  }

  // --- Profile plot hover → highlight construct in 3D ---
  profileCanvas.addEventListener("mousemove", function (e) {
    var rect = profileCanvas.getBoundingClientRect();
    var scaleY = profileCanvas.height / (window.devicePixelRatio || 1) / rect.height;
    var y = (e.clientY - rect.top) * scaleY;
    var row = Math.floor((y - profileLayout.topPad) / profileLayout.rowHeight);
    var newIdx = -1;
    if (row >= 0 && row < profileLayout.nc) {
      newIdx = constructOrder[row].index;
    }
    if (newIdx !== profileHoveredConstruct) {
      if (profileHoveredConstruct >= 0) {
        unhighlightConstruct(profileHoveredConstruct);
        highlightGridRow(-1);
      }
      profileTempVisibleConstruct = newIdx;
      profileHoveredConstruct = newIdx;
      hoveredConstructIndex = newIdx;
      if (profileHoveredConstruct >= 0) {
        highlightConstruct(profileHoveredConstruct);
        highlightGridRow(profileHoveredConstruct);
      }
      if (selectedElementIndex >= 0) drawProfilePlot(selectedElementIndex);
    }
    profileCanvas.style.cursor = newIdx >= 0 ? "pointer" : "default";
  });

  profileCanvas.addEventListener("click", function (e) {
    var rect = profileCanvas.getBoundingClientRect();
    var scaleY = profileCanvas.height / (window.devicePixelRatio || 1) / rect.height;
    var y = (e.clientY - rect.top) * scaleY;
    var row = Math.floor((y - profileLayout.topPad) / profileLayout.rowHeight);
    if (row >= 0 && row < profileLayout.nc) {
      var ci = constructOrder[row].index;
      conCheckboxes[ci].checked = !conCheckboxes[ci].checked;
      conCheckboxes[ci].dispatchEvent(new Event("change"));
    }
  });

  profileCanvas.addEventListener("dblclick", function (e) {
    var rect = profileCanvas.getBoundingClientRect();
    var scaleY = profileCanvas.height / (window.devicePixelRatio || 1) / rect.height;
    var y = (e.clientY - rect.top) * scaleY;
    var row = Math.floor((y - profileLayout.topPad) / profileLayout.rowHeight);
    if (row >= 0 && row < profileLayout.nc) {
      var ci = constructOrder[row].index;
      // Make construct visible if it's not
      if (!constructVisible[ci]) {
        conCheckboxes[ci].checked = true;
        conCheckboxes[ci].dispatchEvent(new Event("change"));
      }
      constructLineVisible[ci] = !constructLineVisible[ci];
      buildCalibration();
      rebuildAllProjections();
    }
  });

  profileCanvas.addEventListener("mouseleave", function () {
    if (profileHoveredConstruct >= 0) {
      unhighlightConstruct(profileHoveredConstruct);
      highlightGridRow(-1);
      profileTempVisibleConstruct = -1;
      profileHoveredConstruct = -1;
      hoveredConstructIndex = -1;
      if (selectedElementIndex >= 0) drawProfilePlot(selectedElementIndex);
    }
    profileCanvas.style.cursor = "default";
  });

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

  function addSectionTitle(text, startCollapsed) {
    var div = document.createElement("div");
    div.className = "section-title" + (startCollapsed ? " collapsed" : "");
    var arrow = document.createElement("span");
    arrow.className = "fold-arrow";
    arrow.textContent = "\u25BC";
    var span = document.createElement("span");
    span.textContent = text;
    var leftPart = document.createElement("span");
    leftPart.appendChild(arrow);
    leftPart.appendChild(span);
    div.appendChild(leftPart);
    guiPanel.appendChild(div);

    var body = document.createElement("div");
    body.className = "section-body" + (startCollapsed ? " collapsed" : "");
    guiPanel.appendChild(body);

    div.addEventListener("click", function (e) {
      // Don't fold when clicking toggle-all button
      if (e.target.classList.contains("toggle-all-btn")) return;
      div.classList.toggle("collapsed");
      body.classList.toggle("collapsed");
    });

    return { title: div, body: body };
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
  var displaySection = addSectionTitle("Display", true);
  var displayBody = displaySection.body;
  addToggle(displayBody, "Dark Mode", true, function (v) {
    document.body.classList.toggle("dark", v);
    scene.background = new THREE.Color(v ? 0x1a1a1a : 0xffffff);
    wireUniforms.uOpacityFront.value = v ? 0.25 : 0.15;
  });
  addToggle(displayBody, "Wireframe Sphere", true, function (v) { sphereGroup.visible = v; });
  addToggle(displayBody, "Depth Fade", false, function (v) {
    wireUniforms.uDepthFade.value = v ? 1.0 : 0.0;
  });

  // Sphere color chooser
  var sphereColorLabel = document.createElement("label");
  var sphereColorInput = document.createElement("input");
  sphereColorInput.type = "color";
  sphereColorInput.value = "#4a4a4a";
  sphereColorInput.addEventListener("input", function () {
    wireUniforms.uColor.value.set(sphereColorInput.value);
  });
  sphereColorLabel.appendChild(sphereColorInput);
  sphereColorLabel.appendChild(document.createTextNode(" Sphere Color"));
  displayBody.appendChild(sphereColorLabel);

  // Sphere grid density
  var gridDensityLabel = document.createElement("label");
  var gridDensityRange = document.createElement("input");
  gridDensityRange.type = "range";
  gridDensityRange.min = "0";
  gridDensityRange.max = "72";
  gridDensityRange.step = "2";
  gridDensityRange.value = "32";
  gridDensityRange.addEventListener("input", function () {
    var seg = parseInt(gridDensityRange.value);
    buildGridLines(seg, Math.round(seg * 0.75));
  });
  gridDensityLabel.appendChild(gridDensityRange);
  gridDensityLabel.appendChild(document.createTextNode(" Grid Lines"));
  displayBody.appendChild(gridDensityLabel);

  // Element color chooser
  var elemColorLabel = document.createElement("label");
  var elemColorInput = document.createElement("input");
  elemColorInput.type = "color";
  elemColorInput.value = "#bbbbbb";
  elemColorInput.addEventListener("input", function () {
    var c = elemColorInput.value;
    for (var i = 0; i < elementObjects.length; i++) {
      elementObjects[i].sphere.material.color.set(c);
      elementObjects[i].glow.material.color.set(c);
      elementObjects[i].label.element.style.color = c;
    }
    if (selectedElementIndex >= 0) updateProfilePlot(selectedElementIndex);
  });
  elemColorLabel.appendChild(elemColorInput);
  elemColorLabel.appendChild(document.createTextNode(" Element Color"));
  displayBody.appendChild(elemColorLabel);

  // Element label size
  var elemSizeLabel = document.createElement("label");
  var elemSizeRange = document.createElement("input");
  elemSizeRange.type = "range";
  elemSizeRange.min = "7";
  elemSizeRange.max = "18";
  elemSizeRange.value = "11";
  elemSizeRange.addEventListener("input", function () {
    var sz = elemSizeRange.value + "px";
    for (var i = 0; i < elementObjects.length; i++) {
      elementObjects[i].label.element.style.fontSize = sz;
    }
  });
  elemSizeLabel.appendChild(elemSizeRange);
  elemSizeLabel.appendChild(document.createTextNode(" Element Labels"));
  displayBody.appendChild(elemSizeLabel);

  // Construct label size
  var conSizeLabel = document.createElement("label");
  var conSizeRange = document.createElement("input");
  conSizeRange.type = "range";
  conSizeRange.min = "7";
  conSizeRange.max = "18";
  conSizeRange.value = "10";
  conSizeRange.addEventListener("input", function () {
    var sz = conSizeRange.value + "px";
    for (var i = 0; i < constructObjects.length; i++) {
      constructObjects[i].rightLabel.element.style.fontSize = sz;
      constructObjects[i].leftLabel.element.style.fontSize = sz;
    }
  });
  conSizeLabel.appendChild(conSizeRange);
  conSizeLabel.appendChild(document.createTextNode(" Construct Labels"));
  displayBody.appendChild(conSizeLabel);

  // Construct axis color chooser
  var axisColorLabel = document.createElement("label");
  var axisColorInput = document.createElement("input");
  axisColorInput.type = "color";
  axisColorInput.value = "#888888";
  axisColorInput.addEventListener("input", function () {
    for (var i = 0; i < constructObjects.length; i++) {
      constructObjects[i].line.material.color.set(axisColorInput.value);
    }
  });
  axisColorLabel.appendChild(axisColorInput);
  axisColorLabel.appendChild(document.createTextNode(" Construct Axis Color"));
  displayBody.appendChild(axisColorLabel);

  // Construct axis thickness
  var axisWidthLabel = document.createElement("label");
  var axisWidthRange = document.createElement("input");
  axisWidthRange.type = "range";
  axisWidthRange.min = "0.5";
  axisWidthRange.max = "5";
  axisWidthRange.step = "0.5";
  axisWidthRange.value = "2";
  axisWidthRange.addEventListener("input", function () {
    var s = parseFloat(axisWidthRange.value);
    for (var i = 0; i < constructObjects.length; i++) {
      var ln = constructObjects[i].line;
      ln.scale.set(s, 1, s);
    }
  });
  axisWidthLabel.appendChild(axisWidthRange);
  axisWidthLabel.appendChild(document.createTextNode(" Axis Thickness"));
  displayBody.appendChild(axisWidthLabel);

  // Projection line thickness
  var projWidthLabel = document.createElement("label");
  var projWidthRange = document.createElement("input");
  projWidthRange.type = "range";
  projWidthRange.min = "0.5";
  projWidthRange.max = "5";
  projWidthRange.step = "0.5";
  projWidthRange.value = "2";
  projWidthRange.addEventListener("input", function () {
    projLineScale = parseFloat(projWidthRange.value);
    rebuildAllProjections();
  });
  projWidthLabel.appendChild(projWidthRange);
  projWidthLabel.appendChild(document.createTextNode(" Projection Thickness"));
  displayBody.appendChild(projWidthLabel);

  // PC axis thickness
  var pcAxisLabel = document.createElement("label");
  var pcAxisRange = document.createElement("input");
  pcAxisRange.type = "range";
  pcAxisRange.min = "0.5";
  pcAxisRange.max = "5";
  pcAxisRange.step = "0.5";
  pcAxisRange.value = "1";
  pcAxisRange.addEventListener("input", function () {
    var s = parseFloat(pcAxisRange.value);
    for (var i = 0; i < pcAxisMeshes.length; i++) {
      pcAxisMeshes[i].scale.set(s, 1, s);
    }
  });
  pcAxisLabel.appendChild(pcAxisRange);
  pcAxisLabel.appendChild(document.createTextNode(" PC Axis Thickness"));
  displayBody.appendChild(pcAxisLabel);

  addToggle(displayBody, "Axes", true, function (v) {
    axesGroup.visible = v;
    for (var a = 0; a < axisLabels.length; a++) {
      axisLabels[a].visible = v;
    }
  });
  addToggle(displayBody, "Calibration Labels", true, function (v) {
    calibrationLabelsVisible = v;
    for (var k = 0; k < calibrationLabelsGroup.children.length; k++) {
      calibrationLabelsGroup.children[k].visible = v;
    }
  });

  // Calibration label size
  var calSizeLabel = document.createElement("label");
  var calSizeRange = document.createElement("input");
  calSizeRange.type = "range";
  calSizeRange.min = "0";
  calSizeRange.max = "14";
  calSizeRange.value = "8";
  calSizeRange.addEventListener("input", function () {
    var val = parseInt(calSizeRange.value);
    for (var k = 0; k < calibrationLabelsGroup.children.length; k++) {
      calibrationLabelsGroup.children[k].element.style.fontSize = val + "px";
      calibrationLabelsGroup.children[k].element.style.display = val === 0 ? "none" : "";
    }
  });
  calSizeLabel.appendChild(calSizeRange);
  calSizeLabel.appendChild(document.createTextNode(" Calibration Size"));
  displayBody.appendChild(calSizeLabel);

  addToggle(displayBody, "Element Labels", true, function (v) {
    for (var i = 0; i < elements.length; i++) {
      elementLabelVisible[i] = v;
      elementObjects[i].label.visible = v && elementVisible[i];
    }
  });
  addToggle(displayBody, "Construct Labels", true, function (v) {
    for (var i = 0; i < constructs.length; i++) {
      constructLabelVisible[i] = v;
    }
  });
  addToggle(displayBody, "Deconflict Labels", true, function (v) {
    labelDeconflict = v;
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
  addToggleAllButton(elemSection.title, function () { return elemCheckboxes; });

  for (var i = 0; i < elements.length; i++) {
    (function (idx) {
      var cb = addToggle(elemListDiv, elements[idx].name, true, function (v) {
        elementVisible[idx] = v;
        elementObjects[idx].sphere.visible = v;
        elementObjects[idx].label.visible = v && elementLabelVisible[idx];
        buildProjectionsForElement(idx);
      });
      elemCheckboxes.push(cb);
    })(i);
  }
  elemSection.body.appendChild(elemListDiv);

  // --- Constructs section ---
  var conSection = addSectionTitle("Constructs");
  var conCheckboxes = [];
  var conListDiv = document.createElement("div");
  conListDiv.className = "item-list";
  addToggleAllButton(conSection.title, function () { return conCheckboxes; });

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
  conSection.body.appendChild(conListDiv);

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
  }

  function unhighlightElement(idx) {
    if (idx < 0) return;
    elementObjects[idx].sphere.scale.setScalar(1.0);
    elementObjects[idx].sphere.material.emissive.setHex(0x000000);
    elementObjects[idx].label.element.style.fontWeight = "";
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
      if (newHoveredElement >= 0) updateProfilePlot(newHoveredElement);
    }
    if (newHoveredConstruct !== hoveredConstructIndex) {
      unhighlightConstruct(hoveredConstructIndex);
      hoveredConstructIndex = newHoveredConstruct;
      highlightConstruct(hoveredConstructIndex);
      highlightGridRow(hoveredConstructIndex);
      if (selectedElementIndex >= 0) updateProfilePlot(selectedElementIndex);
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

  // Single click: toggle element label visibility
  function onClick(event) {
    var rect = renderer.domElement.getBoundingClientRect();
    mouse.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
    mouse.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;

    raycaster.setFromCamera(mouse, camera);
    var intersects = raycaster.intersectObjects(hoverTargets);

    if (intersects.length > 0) {
      var ud = intersects[0].object.userData;
      if (ud.type === "element") {
        elementLabelVisible[ud.index] = !elementLabelVisible[ud.index];
        elementObjects[ud.index].label.visible = elementLabelVisible[ud.index] && elementVisible[ud.index];
        updateProfilePlot(ud.index);
      } else if (ud.type === "construct") {
        constructLabelVisible[ud.index] = !constructLabelVisible[ud.index];
      }
    }
  }

  // --- Context menu ---
  var contextMenu = document.getElementById("context-menu");
  var contextTargetElement = -1;

  function hideContextMenu() {
    contextMenu.style.display = "none";
    contextTargetElement = -1;
  }

  function addMenuItem(text, onClick) {
    var item = document.createElement("div");
    item.className = "menu-item";
    item.textContent = text;
    item.addEventListener("click", function () {
      onClick();
      hideContextMenu();
    });
    contextMenu.appendChild(item);
  }

  function showContextMenuAt(x, y) {
    contextMenu.style.display = "block";
    contextMenu.style.left = x + "px";
    contextMenu.style.top = y + "px";
    var menuRect = contextMenu.getBoundingClientRect();
    if (menuRect.right > window.innerWidth) {
      contextMenu.style.left = (x - menuRect.width) + "px";
    }
    if (menuRect.bottom > window.innerHeight) {
      contextMenu.style.top = (y - menuRect.height) + "px";
    }
  }

  function showElementContextMenu(x, y, elemIdx) {
    contextTargetElement = elemIdx;
    contextMenu.innerHTML = "";
    var isBenchmark = benchmarkElements.indexOf(elemIdx) >= 0;
    addMenuItem(isBenchmark ? "Remove benchmark" : "Add as benchmark", function () {
      if (isBenchmark) {
        benchmarkElements = benchmarkElements.filter(function (i) { return i !== elemIdx; });
      } else {
        benchmarkElements.push(elemIdx);
      }
      if (selectedElementIndex >= 0) updateProfilePlot(selectedElementIndex);
    });
    addMenuItem("Hide element", function () {
      elemCheckboxes[elemIdx].checked = false;
      elemCheckboxes[elemIdx].dispatchEvent(new Event("change"));
    });
    showContextMenuAt(x, y);
  }

  function showConstructContextMenu(x, y, conIdx) {
    contextMenu.innerHTML = "";
    var hasAxis = constructLineVisible[conIdx];
    addMenuItem(hasAxis ? "Hide calibrated axis" : "Show calibrated axis", function () {
      constructLineVisible[conIdx] = !constructLineVisible[conIdx];
      buildCalibration();
      rebuildAllProjections();
    });
    addMenuItem("Hide construct", function () {
      conCheckboxes[conIdx].checked = false;
      conCheckboxes[conIdx].dispatchEvent(new Event("change"));
    });
    showContextMenuAt(x, y);
  }

  function showBackgroundContextMenu(x, y) {
    contextMenu.innerHTML = "";
    addMenuItem("Hide all projections", function () {
      for (var i = 0; i < elements.length; i++) {
        elementProjections[i] = false;
        buildProjectionsForElement(i);
      }
    });
    addMenuItem("Hide all calibrated axes", function () {
      for (var i = 0; i < constructs.length; i++) {
        constructLineVisible[i] = false;
      }
      buildCalibration();
      rebuildAllProjections();
    });
    addMenuItem("Hide all elements", function () {
      for (var i = 0; i < elements.length; i++) {
        if (elemCheckboxes[i].checked) {
          elemCheckboxes[i].checked = false;
          elemCheckboxes[i].dispatchEvent(new Event("change"));
        }
      }
    });
    addMenuItem("Hide all constructs", function () {
      for (var i = 0; i < constructs.length; i++) {
        if (conCheckboxes[i].checked) {
          conCheckboxes[i].checked = false;
          conCheckboxes[i].dispatchEvent(new Event("change"));
        }
      }
    });
    addMenuItem("Show all elements", function () {
      for (var i = 0; i < elements.length; i++) {
        if (!elemCheckboxes[i].checked) {
          elemCheckboxes[i].checked = true;
          elemCheckboxes[i].dispatchEvent(new Event("change"));
        }
      }
    });
    addMenuItem("Show all constructs", function () {
      for (var i = 0; i < constructs.length; i++) {
        if (!conCheckboxes[i].checked) {
          conCheckboxes[i].checked = true;
          conCheckboxes[i].dispatchEvent(new Event("change"));
        }
      }
    });
    addMenuItem("Reset to initial state", function () {
      camera.position.copy(initialCameraPosition);
      controls.target.copy(initialControlsTarget);
      controls.update();
      for (var i = 0; i < elements.length; i++) {
        if (!elemCheckboxes[i].checked) {
          elemCheckboxes[i].checked = true;
          elemCheckboxes[i].dispatchEvent(new Event("change"));
        }
        elementProjections[i] = false;
        elementLabelVisible[i] = true;
        elementObjects[i].label.visible = true;
      }
      for (var i = 0; i < constructs.length; i++) {
        if (!conCheckboxes[i].checked) {
          conCheckboxes[i].checked = true;
          conCheckboxes[i].dispatchEvent(new Event("change"));
        }
        constructLineVisible[i] = false;
        constructLabelVisible[i] = true;
      }
      benchmarkElements = [];
      selectedElementIndex = -1;
      updateElementGlows();
      buildCalibration();
      rebuildAllProjections();
      profileCanvas.style.display = "none";
      profileHint.style.display = "";
      removeBenchmarksBtn.style.display = "none";
    });
    showContextMenuAt(x, y);
  }

  renderer.domElement.addEventListener("contextmenu", function (event) {
    event.preventDefault();
    var rect = renderer.domElement.getBoundingClientRect();
    mouse.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
    mouse.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;

    raycaster.setFromCamera(mouse, camera);
    var intersects = raycaster.intersectObjects(hoverTargets);

    if (intersects.length > 0) {
      var ud = intersects[0].object.userData;
      if (ud.type === "element") {
        showElementContextMenu(event.clientX, event.clientY, ud.index);
        return;
      }
      if (ud.type === "construct") {
        showConstructContextMenu(event.clientX, event.clientY, ud.index);
        return;
      }
    }
    showBackgroundContextMenu(event.clientX, event.clientY);
  });

  document.addEventListener("click", function () { hideContextMenu(); });
  document.addEventListener("keydown", function (e) { if (e.key === "Escape") hideContextMenu(); });

  renderer.domElement.addEventListener("click", onClick, false);
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
    if (selectedElementIndex >= 0) updateProfilePlot(selectedElementIndex);
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
    if (selectedElementIndex >= 0) updateProfilePlot(selectedElementIndex);
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
  // 14. LABEL DECONFLICTION
  // =============================================
  var labelDeconflict = true;

  function deconflictLabels() {
    if (!labelDeconflict) return;

    var items = [];

    // Collect visible element labels
    for (var i = 0; i < elementObjects.length; i++) {
      if (!elementVisible[i] || !elementLabelVisible[i] || !elementObjects[i].label.visible) continue;
      items.push(elementObjects[i].label.element);
    }

    // Collect visible construct labels
    for (var i = 0; i < constructObjects.length; i++) {
      if (!constructVisible[i]) continue;
      if (constructObjects[i].rightLabel.visible)
        items.push(constructObjects[i].rightLabel.element);
      if (constructObjects[i].leftLabel.visible)
        items.push(constructObjects[i].leftLabel.element);
    }

    if (items.length < 2) return;

    // Batch-read all bounding rects
    var rects = [];
    for (var i = 0; i < items.length; i++) {
      var r = items[i].getBoundingClientRect();
      if (r.width === 0 && r.height === 0) continue;
      rects.push({
        el: items[i],
        cx: r.left + r.width / 2,
        cy: r.top + r.height / 2,
        hw: r.width / 2,
        hh: r.height / 2,
        dx: 0,
        dy: 0
      });
    }

    // Iteratively resolve overlaps
    for (var iter = 0; iter < 4; iter++) {
      for (var i = 0; i < rects.length; i++) {
        for (var j = i + 1; j < rects.length; j++) {
          var a = rects[i], b = rects[j];
          var ax = a.cx + a.dx, ay = a.cy + a.dy;
          var bx = b.cx + b.dx, by = b.cy + b.dy;
          var ox = (a.hw + b.hw) - Math.abs(ax - bx);
          var oy = (a.hh + b.hh) - Math.abs(ay - by);
          if (ox > 0 && oy > 0) {
            // Push along smaller overlap axis
            if (oy < ox) {
              var py = oy / 2 + 0.5;
              if (ay <= by) { a.dy -= py; b.dy += py; }
              else { a.dy += py; b.dy -= py; }
            } else {
              var px = ox / 2 + 0.5;
              if (ax <= bx) { a.dx -= px; b.dx += px; }
              else { a.dx += px; b.dx -= px; }
            }
          }
        }
      }
    }

    // Batch-write nudge offsets
    for (var i = 0; i < rects.length; i++) {
      var r = rects[i];
      if (r.dx !== 0 || r.dy !== 0) {
        r.el.style.transform += " translate(" + r.dx.toFixed(1) + "px," + r.dy.toFixed(1) + "px)";
      }
    }
  }

  // =============================================
  // 15. RENDER LOOP
  // =============================================
  function animate() {
    requestAnimationFrame(animate);
    controls.update();
    updateLabelVisibility();
    camera.getWorldDirection(wireUniforms.uCamDir.value);
    renderer.render(scene, camera);
    labelRenderer.render(scene, camera);
    updateLabelAlignment();
    deconflictLabels();
  }
  animate();

})();
