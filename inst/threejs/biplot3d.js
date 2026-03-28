(function () {
  "use strict";

  // Capture original HTML before any DOM manipulation for Save As
  var _savedHTML = "<!DOCTYPE html>\n" + document.documentElement.outerHTML;

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
  var calibrationTickSize = 0.02;
  var calibrationTickWidth = 2.0;
  var elevationFilterAngle = 90; // max elevation from view plane in degrees
  var ringProjectionMode = false;
  var showProjectionErrors = false;

  // --- Scene setup ---
  var sceneContainer = document.getElementById("scene-container");
  var width = sceneContainer.clientWidth;
  var height = sceneContainer.clientHeight;

  var scene = new THREE.Scene();
  document.body.classList.add("dark");
  scene.background = new THREE.Color(0x1a1a1a);

  var perspCamera = new THREE.PerspectiveCamera(50, width / height, 0.01, 100);
  perspCamera.position.set(0, 0, 3.5);
  perspCamera.lookAt(0, 0, 0);

  var orthoHalfSize = 2.0;
  var aspect = width / height;
  var orthoCamera = new THREE.OrthographicCamera(
    -orthoHalfSize * aspect, orthoHalfSize * aspect,
    orthoHalfSize, -orthoHalfSize, 0.01, 100
  );
  orthoCamera.position.set(0, 0, 3.5);
  orthoCamera.lookAt(0, 0, 0);

  var camera = perspCamera;
  var isOrthographic = false;

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

  function switchCamera(toOrtho) {
    isOrthographic = toOrtho;
    var oldCam = camera;
    camera = toOrtho ? orthoCamera : perspCamera;

    // Copy position and orientation from old camera
    camera.position.copy(oldCam.position);
    camera.quaternion.copy(oldCam.quaternion);
    camera.up.copy(oldCam.up);

    if (toOrtho) {
      // Match ortho frustum to current perspective view
      var dist = oldCam.position.length();
      var halfH = dist * Math.tan(THREE.Math.degToRad(perspCamera.fov / 2));
      var asp = width / height;
      orthoCamera.left = -halfH * asp;
      orthoCamera.right = halfH * asp;
      orthoCamera.top = halfH;
      orthoCamera.bottom = -halfH;
      orthoCamera.updateProjectionMatrix();
    }

    controls.object = camera;
    controls.update();
  }

  var initialCameraPosition = camera.position.clone();
  var initialControlsTarget = controls.target.clone();

  // Save original coordinates for reset after rotation
  var initialElements = elements.map(function (e) { return { x: e.x, y: e.y, z: e.z }; });
  var initialConstructs = constructs.map(function (c) { return { x: c.x, y: c.y, z: c.z }; });
  var initialCalibCoords = calibration ? calibration.construct_coords.map(function (c) { return [c[0], c[1], c[2]]; }) : null;

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

  // Silhouette ring — billboard circle at sphere edge
  var silhouetteGroup = new THREE.Group();
  silhouetteGroup.visible = false;

  var silhouetteRingGeom = new THREE.TorusBufferGeometry(1, 0.002, 8, 128);
  var silhouetteRingMat = new THREE.MeshBasicMaterial({ color: 0x888888, opacity: 0.6, transparent: true });
  var silhouetteRingMesh = new THREE.Mesh(silhouetteRingGeom, silhouetteRingMat);
  silhouetteGroup.add(silhouetteRingMesh);

  var silhouetteGlowGeom = new THREE.TorusBufferGeometry(1, 0.02, 32, 128);
  var silhouetteGlowUniforms = {
    uColor: { value: new THREE.Color(0x888888) },
    uOpacity: { value: 0.0 }
  };
  var silhouetteGlowMat = new THREE.ShaderMaterial({
    uniforms: silhouetteGlowUniforms,
    transparent: true,
    depthWrite: false,
    side: THREE.DoubleSide,
    vertexShader: [
      "varying vec3 vNormal;",
      "varying vec3 vViewDir;",
      "void main() {",
      "  vNormal = normalize(normalMatrix * normal);",
      "  vec4 mvPos = modelViewMatrix * vec4(position, 1.0);",
      "  vViewDir = normalize(-mvPos.xyz);",
      "  gl_Position = projectionMatrix * mvPos;",
      "}"
    ].join("\n"),
    fragmentShader: [
      "uniform vec3 uColor;",
      "uniform float uOpacity;",
      "varying vec3 vNormal;",
      "varying vec3 vViewDir;",
      "void main() {",
      "  float facing = abs(dot(vNormal, vViewDir));",
      "  float alpha = pow(facing, 1.5) * uOpacity;",
      "  gl_FragColor = vec4(uColor, alpha);",
      "}"
    ].join("\n")
  });
  var silhouetteGlowMesh = new THREE.Mesh(silhouetteGlowGeom, silhouetteGlowMat);
  silhouetteGroup.add(silhouetteGlowMesh);

  scene.add(silhouetteGroup);

  scene.add(sphereGroup);
  scene.add(elementPointsGroup);
  scene.add(elementLabelsGroup);
  scene.add(constructPointsGroup);
  scene.add(constructLinesGroup);
  scene.add(constructLabelsGroup);
  axesGroup.visible = false;
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
  var hoveredFootConstructIndex = -1; // construct index when hovering a projection foot in 3D
  var hoveredFootElementIndex = -1;   // element index of the hovered projection foot

  // --- Selection ---
  var selectedElements = []; // indices of selected elements
  var selectedConstructs = []; // indices of selected constructs

  // --- Colors ---
  var elementColor = 0xbbbbbb;
  var selectionColor = 0x44aaff; // bright blue for selected elements
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
    uColor: { value: new THREE.Color(0xededed) },
    uOpacityFront: { value: 0.25 },
    uOpacityBack: { value: 0.04 },
    uCamDir: { value: new THREE.Vector3(0, 0, -1) },
    uDepthFade: { value: 1.0 }
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
      rightMarker: rMarker, leftMarker: lMarker,
      _origLineLen: lineLen,
      _origLinePos: mid.clone(),
      _origLineQuat: quat.clone()
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
      color: elementColor, transparent: true, opacity: 0.35,
      depthWrite: false
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
    axLabel.visible = false;
    axesGroup.add(axLabel);
    axisLabels.push(axLabel);
  }

  // =============================================
  // 5. PROJECTIONS (per-element, toggled by double-click)
  // =============================================
  // Per-element projection groups stored here
  var projLineScale = 3;
  var elementProjectionGroups = [];
  for (var i = 0; i < elements.length; i++) {
    var g = new THREE.Group();
    g.visible = false;
    projectionsGroup.add(g);
    elementProjectionGroups.push(g);
  }

  function buildProjectionsForElement(idx) {
    var group = elementProjectionGroups[idx];
    // Remove old foot dots from hoverTargets
    for (var k = group.children.length - 1; k >= 0; k--) {
      var child = group.children[k];
      if (child.userData && child.userData.type === "projectionFoot") {
        var htIdx = hoverTargets.indexOf(child);
        if (htIdx >= 0) hoverTargets.splice(htIdx, 1);
      }
    }
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
      if (!constructVisible[j] || !constructLineVisible[j] || isConstructElevationFiltered(j)) continue;
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
      var dotGeom = new THREE.SphereBufferGeometry(0.012, 8, 6);
      var dotMat = new THREE.MeshBasicMaterial({ color: projColor });
      var dot = new THREE.Mesh(dotGeom, dotMat);
      dot.position.copy(foot);
      dot.userData = { type: "projectionFoot", elementIndex: idx, constructIndex: j };
      group.add(dot);
      hoverTargets.push(dot);

      // Projection error segment: foot → actual value position on axis
      if (showProjectionErrors && calibration) {
        var rating = ratings.values[j][idx];
        if (rating != null && !isNaN(rating)) {
          var Ci = calibration.construct_coords[j];
          var norm2 = Ci[0] * Ci[0] + Ci[1] * Ci[1] + Ci[2] * Ci[2];
          if (norm2 > 1e-10) {
            var vc = rating - calibration.offsets[j];
            var factor = calibration.se * vc / norm2;
            var actual = new THREE.Vector3(factor * Ci[0], factor * Ci[1], factor * Ci[2]);
            var errDist = foot.distanceTo(actual);
            if (errDist > 0.001) {
              var errMid = new THREE.Vector3().addVectors(foot, actual).multiplyScalar(0.5);
              var errGeom = new THREE.CylinderBufferGeometry(0.003, 0.003, errDist, 4, 1);
              var errMat = new THREE.MeshBasicMaterial({ color: 0xff4444, opacity: 0.85, transparent: true });
              var errMesh = new THREE.Mesh(errGeom, errMat);
              errMesh.position.copy(errMid);
              errMesh.quaternion.setFromUnitVectors(
                new THREE.Vector3(0, 1, 0),
                new THREE.Vector3().subVectors(actual, foot).normalize()
              );
              errMesh.scale.set(projLineScale, 1, projLineScale);
              group.add(errMesh);

              // Small dot at actual value position
              var actDotGeom = new THREE.SphereBufferGeometry(0.012, 8, 6);
              var actDotMat = new THREE.MeshBasicMaterial({ color: 0xff4444 });
              var actDot = new THREE.Mesh(actDotGeom, actDotMat);
              actDot.position.copy(actual);
              group.add(actDot);
            }
          }
        }
      }
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
      var child = calibrationGroup.children[k];
      if (child !== calibrationLabelsGroup) {
        if (child.geometry) child.geometry.dispose();
        if (child.material) child.material.dispose();
        calibrationGroup.remove(child);
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

    // Count visible calibrated axes to decide center-skip threshold
    var visibleAxisCount = 0;
    for (var i = 0; i < constructs.length; i++) {
      if (constructVisible[i] && constructLineVisible[i] && !isConstructElevationFiltered(i)) {
        visibleAxisCount++;
      }
    }
    var centerSkip = visibleAxisCount <= 1 ? 0 : 0.03;

    for (var i = 0; i < constructs.length; i++) {
      if (!constructVisible[i] || !constructLineVisible[i] || isConstructElevationFiltered(i)) continue;

      var Ci = [cCoords[i][0], cCoords[i][1], cCoords[i][2]];
      var norm2 = Ci[0] * Ci[0] + Ci[1] * Ci[1] + Ci[2] * Ci[2];
      if (norm2 < 1e-10) continue;

      // Direction vector for tick perpendicular (use camera direction for view-facing ticks)
      var cDir = new THREE.Vector3(Ci[0], Ci[1], Ci[2]).normalize();
      var camDir = new THREE.Vector3();
      camera.getWorldDirection(camDir);
      var perp1 = new THREE.Vector3().crossVectors(cDir, camDir);
      if (perp1.length() < 0.01) {
        // Axis aligned with camera — fall back to camera up
        perp1.crossVectors(cDir, camera.up);
      }
      perp1.normalize();
      var tickLen = calibrationTickSize;

      for (var v = vMin; v <= vMax; v++) {
        var vc = v - offsets[i];
        var factor = se * vc / norm2;
        var tx = factor * Ci[0];
        var ty = factor * Ci[1];
        var tz = factor * Ci[2];

        // Skip ticks outside the sphere, and near center only when multiple axes shown
        var dist = Math.sqrt(tx * tx + ty * ty + tz * tz);
        if (dist > 1.0 || dist < centerSkip) continue;

        // Tick line (cylinder for variable thickness)
        var t1 = new THREE.Vector3(
          tx - perp1.x * tickLen, ty - perp1.y * tickLen, tz - perp1.z * tickLen
        );
        var t2 = new THREE.Vector3(
          tx + perp1.x * tickLen, ty + perp1.y * tickLen, tz + perp1.z * tickLen
        );
        var tickDist = t1.distanceTo(t2);
        var tickCylGeom = new THREE.CylinderBufferGeometry(0.001, 0.001, tickDist, 4, 1);
        var axisCol = constructObjects[i].line.material.color;
        var tickMat = new THREE.MeshBasicMaterial({ color: axisCol.clone(), opacity: 0.5, transparent: true });
        var tickMesh = new THREE.Mesh(tickCylGeom, tickMat);
        var tickMid = new THREE.Vector3().addVectors(t1, t2).multiplyScalar(0.5);
        tickMesh.position.copy(tickMid);
        tickMesh.quaternion.setFromUnitVectors(
          new THREE.Vector3(0, 1, 0),
          new THREE.Vector3().subVectors(t2, t1).normalize()
        );
        tickMesh.scale.set(calibrationTickWidth, 1, calibrationTickWidth);
        calibrationGroup.add(tickMesh);

        // Tick label
        var tickLabelDiv = document.createElement("div");
        tickLabelDiv.className = "label-calibration";
        tickLabelDiv.style.color = "#" + axisCol.getHexString();
        tickLabelDiv.textContent = v;
        if (typeof calSizeRange !== "undefined") {
          var calVal = parseInt(calSizeRange.value);
          tickLabelDiv.style.fontSize = calVal + "px";
          if (calVal === 0) tickLabelDiv.style.display = "none";
        }
        var tickLabel = new THREE.CSS2DObject(tickLabelDiv);
        tickLabel.position.set(
          tx + perp1.x * tickLen * 1.4,
          ty + perp1.y * tickLen * 1.4,
          tz + perp1.z * tickLen * 1.4
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

  function isConstructElevationFiltered(i) {
    if (elevationFilterAngle >= 90) return false; // fast path: no filtering
    camera.getWorldDirection(_camDir);
    var c = constructs[i];
    _poleDir.set(c.x, c.y, c.z).normalize();
    var dot = Math.abs(_poleDir.dot(_camDir));
    var elevDeg = Math.asin(Math.min(dot, 1)) * (180 / Math.PI);
    return elevDeg > elevationFilterAngle;
  }

  var _ringRight = new THREE.Vector3();
  var _ringUp = new THREE.Vector3();
  var _ringProj = new THREE.Vector3();

  function updateLabelVisibility() {
    camera.getWorldDirection(_camDir);

    // Precompute view-plane basis for ring projection
    if (ringProjectionMode) {
      _ringRight.crossVectors(_camDir, camera.up).normalize();
      _ringUp.crossVectors(_ringRight, _camDir).normalize();
    }

    for (var i = 0; i < constructs.length; i++) {
      var isTempVisible = (profileTempVisibleConstruct === i);
      if ((!constructVisible[i] || isConstructElevationFiltered(i)) && !isTempVisible) {
        constructObjects[i].rightLabel.visible = false;
        constructObjects[i].leftLabel.visible = false;
        constructObjects[i].rightMarker.visible = false;
        constructObjects[i].leftMarker.visible = false;
        constructObjects[i].line.visible = false;
        continue;
      }

      var sc = constructSphereCoords[i];
      var obj = constructObjects[i];

      if (ringProjectionMode) {
        // Project construct direction onto view plane and normalize to ring
        var px = sc.rx * _ringRight.x + sc.ry * _ringRight.y + sc.rz * _ringRight.z;
        var py = sc.rx * _ringUp.x + sc.ry * _ringUp.y + sc.rz * _ringUp.z;
        var pLen = Math.sqrt(px * px + py * py);
        if (pLen > 0.001) {
          px /= pLen;
          py /= pLen;
        }
        // Position on the ring (radius 1) in world coords
        var rpx = _ringRight.x * px + _ringUp.x * py;
        var rpy = _ringRight.y * px + _ringUp.y * py;
        var rpz = _ringRight.z * px + _ringUp.z * py;

        obj.rightMarker.position.set(rpx, rpy, rpz);
        obj.rightLabel.position.set(rpx, rpy, rpz);
        obj.leftMarker.position.set(-rpx, -rpy, -rpz);
        obj.leftLabel.position.set(-rpx, -rpy, -rpz);

        // In ring mode, show both poles (no back-face culling)
        var clv = constructLabelVisible[i];
        obj.rightLabel.visible = clv;
        obj.rightMarker.visible = constructPointsGroup.visible;
        obj.leftLabel.visible = clv;
        obj.leftMarker.visible = constructPointsGroup.visible;
        // In ring mode, reposition axis line to go between ring-projected poles
        var isHovered = (hoveredConstructIndex === i);
        obj.line.visible = constructLineVisible[i] || isHovered;
        if (obj.line.visible) {
          obj.line.position.set(0, 0, 0); // midpoint of diameter is origin
          var ringDir = new THREE.Vector3(rpx, rpy, rpz).normalize();
          obj.line.quaternion.setFromUnitVectors(new THREE.Vector3(0, 1, 0), ringDir);
          // Scale Y to match ring diameter (2), relative to original line length
          var origLen = obj._origLineLen;
          obj.line.scale.y = 2 / origLen;
        }
      } else {
        // Restore original sphere positions
        obj.rightMarker.position.set(sc.rx, sc.ry, sc.rz);
        obj.rightLabel.position.set(sc.rx, sc.ry, sc.rz);
        obj.leftMarker.position.set(sc.lx, sc.ly, sc.lz);
        obj.leftLabel.position.set(sc.lx, sc.ly, sc.lz);

        // Restore original line position, orientation, and scale
        obj.line.position.copy(obj._origLinePos);
        obj.line.quaternion.copy(obj._origLineQuat);
        obj.line.scale.y = 1;

        // Construct line: visible if permanently toggled (double-click), hovered, or temp-visible
        var isHovered = (hoveredConstructIndex === i);
        obj.line.visible = constructLineVisible[i] || isHovered;

        // When hovered, force both poles visible; otherwise back-face cull with 10° tolerance
        var clv = constructLabelVisible[i];
        if (isHovered) {
          obj.rightLabel.visible = true;
          obj.rightMarker.visible = true;
          obj.leftLabel.visible = true;
          obj.leftMarker.visible = true;
        } else {
          _poleDir.set(sc.rx, sc.ry, sc.rz).normalize();
          var rightFacing = _poleDir.dot(_camDir) < facingThreshold;
          obj.rightLabel.visible = rightFacing && clv;
          obj.rightMarker.visible = rightFacing && constructPointsGroup.visible;

          _poleDir.set(sc.lx, sc.ly, sc.lz).normalize();
          var leftFacing = _poleDir.dot(_camDir) < facingThreshold;
          obj.leftLabel.visible = leftFacing && clv;
          obj.leftMarker.visible = leftFacing && constructPointsGroup.visible;
        }
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
          cellHoverLeave();
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

  // --- Grid table cell interactions (hover + double-click) ---
  var cellHoverState = null; // tracks transient state for cleanup

  function cellHoverEnter(ei, ci) {
    // Save prior state
    var prev = {
      ei: ei, ci: ci,
      constructLineVisible: constructLineVisible[ci],
      constructVisible: constructVisible[ci],
      elementProjections: elementProjections[ei],
      elementVisible: elementVisible[ei]
    };
    cellHoverState = prev;

    // Temporarily show element (if hidden)
    if (!elementVisible[ei]) {
      elementVisible[ei] = true;
      elementObjects[ei].sphere.visible = true;
      elementObjects[ei].label.visible = elementLabelVisible[ei];
    }

    // Temporarily show construct (if hidden) and its axis
    if (!constructVisible[ci]) {
      constructVisible[ci] = true;
    }
    if (!constructLineVisible[ci]) {
      constructLineVisible[ci] = true;
    }
    buildCalibration();

    // Temporarily show projections for this element
    if (!elementProjections[ei]) {
      elementProjections[ei] = true;
    }
    buildProjectionsForElement(ei);

    // Highlight element and cell
    highlightElement(ei);
    highlightGridCell(ei, ci);
  }

  function cellHoverLeave() {
    if (!cellHoverState) return;
    var prev = cellHoverState;
    cellHoverState = null;

    // Restore element visibility
    if (!prev.elementVisible) {
      elementVisible[prev.ei] = false;
      elementObjects[prev.ei].sphere.visible = false;
      elementObjects[prev.ei].label.visible = false;
    }

    // Restore construct visibility
    if (!prev.constructVisible) {
      constructVisible[prev.ci] = false;
    }
    if (!prev.constructLineVisible) {
      constructLineVisible[prev.ci] = false;
    }
    buildCalibration();

    // Restore element projection state
    if (!prev.elementProjections) {
      elementProjections[prev.ei] = false;
    }
    buildProjectionsForElement(prev.ei);

    // Un-highlight element and cell
    unhighlightElement(prev.ei);
    unhighlightGridCell();
  }

  (function () {
    var cells = document.querySelectorAll("#grid-table td.rating-cell");
    for (var c = 0; c < cells.length; c++) {
      (function (td) {
        td.addEventListener("mouseenter", function () {
          var ei = parseInt(td.dataset.elementIndex);
          var ci = parseInt(td.dataset.constructIndex);
          // Clean up previous cell hover if moving between cells
          if (cellHoverState && (cellHoverState.ei !== ei || cellHoverState.ci !== ci)) {
            cellHoverLeave();
          }
          if (!cellHoverState) {
            cellHoverEnter(ei, ci);
          }
        });

        td.addEventListener("mouseleave", function (e) {
          // Only leave if moving to a non-rating-cell (row leave handles cleanup too)
          var related = e.relatedTarget;
          if (related && related.classList && related.classList.contains("rating-cell")) return;
          cellHoverLeave();
        });

        td.addEventListener("dblclick", function (e) {
          e.stopPropagation(); // prevent row dblclick
          var ei = parseInt(td.dataset.elementIndex);
          var ci = parseInt(td.dataset.constructIndex);

          // On double-click, make the state permanent — clear transient tracking
          if (cellHoverState) {
            cellHoverState.constructLineVisible = true;
            cellHoverState.elementProjections = true;
          }

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
  var gridHoverElemWasHidden = false;
  (function () {
    var headers = document.querySelectorAll("#grid-table th.element-header");
    for (var h = 0; h < headers.length; h++) {
      (function (th) {
        var ei = parseInt(th.dataset.elementIndex);
        th.style.cursor = "pointer";

        th.addEventListener("mouseenter", function () {
          if (ei === gridHoveredElement) return;
          // Clean up previous
          if (gridHoveredElement >= 0) {
            unhighlightElement(gridHoveredElement);
            if (gridHoverElemWasHidden) {
              elementVisible[gridHoveredElement] = false;
              elementObjects[gridHoveredElement].sphere.visible = false;
              elementObjects[gridHoveredElement].label.visible = false;
              gridHoverElemWasHidden = false;
            }
          }
          gridHoveredElement = ei;
          // Temporarily show hidden element
          if (!elementVisible[ei]) {
            gridHoverElemWasHidden = true;
            elementVisible[ei] = true;
            elementObjects[ei].sphere.visible = true;
            elementObjects[ei].label.visible = elementLabelVisible[ei];
          }
          highlightElement(ei);
          highlightGridColumn(ei);
          updateProfilePlot(ei);
        });

        th.addEventListener("mouseleave", function () {
          if (gridHoveredElement >= 0) {
            unhighlightElement(gridHoveredElement);
            if (gridHoverElemWasHidden) {
              elementVisible[gridHoveredElement] = false;
              elementObjects[gridHoveredElement].sphere.visible = false;
              elementObjects[gridHoveredElement].label.visible = false;
              gridHoverElemWasHidden = false;
            }
            highlightGridColumn(-1);
            gridHoveredElement = -1;
          }
        });

        th.addEventListener("click", function () {
          // If making permanent via click, don't revert on leave
          if (gridHoverElemWasHidden) gridHoverElemWasHidden = false;
          elemCheckboxes[ei].checked = !elemCheckboxes[ei].checked;
          elemCheckboxes[ei].dispatchEvent(new Event("change"));
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

  // Profile font size control
  var profileFontSize = 10;
  var profileFontControl = document.createElement("div");
  profileFontControl.className = "profile-font-control";
  var pfRange = document.createElement("input");
  pfRange.type = "range";
  pfRange.min = "7";
  pfRange.max = "14";
  pfRange.step = "1";
  pfRange.value = "10";
  var pfLabel = document.createElement("span");
  pfLabel.textContent = "Font";
  profileFontControl.appendChild(pfLabel);
  profileFontControl.appendChild(pfRange);
  var profileContainer = document.getElementById("profile-container");
  profileContainer.insertBefore(profileFontControl, profileCanvas);
  pfRange.addEventListener("input", function () {
    profileFontSize = parseInt(pfRange.value);
    if (selectedElementIndex >= 0) drawProfilePlot(selectedElementIndex);
  });
  var removeBenchmarksBtn = document.getElementById("remove-benchmarks-btn");
  removeBenchmarksBtn.addEventListener("click", function () {
    benchmarkElements = [];
    removeBenchmarksBtn.style.display = "none";
    updateElementGlows();
    updateDynamicSortVisibility();
    if (selectedElementIndex >= 0) drawProfilePlot(selectedElementIndex);
  });
  var selectedElementIndex = -1;
  var profileLayout = { topPad: 0, rowHeight: 0, nc: 0 };
  var profileHoveredConstruct = -1;
  var profileTempVisibleConstruct = -1; // construct temporarily made visible by profile hover
  var benchmarkElements = []; // indices of benchmark elements
  var benchColors = ["#e6194b", "#f58231", "#911eb4", "#42d4f4", "#3cb44b"];

  // Construct sort orders
  var constructOrderOriginal = constructs.map(function (c, i) {
    return { index: i };
  });
  var constructOrderAngular = constructs.map(function (c, i) {
    return { index: i, angle: Math.atan2(c.y, c.x) };
  });
  constructOrderAngular.sort(function (a, b) { return a.angle - b.angle; });
  var constructSortMode = "angular"; // "original", "angular", or "difference"
  var constructOrder = constructOrderAngular;

  // Add dynamic sort radio buttons
  var sortToggle = document.querySelector(".sort-toggle");

  // "Value" radio — visible when one element selected, no benchmark
  var valueSortLabel = document.createElement("label");
  valueSortLabel.title = "Sort constructs by rating value (descending)";
  var valueSortRadio = document.createElement("input");
  valueSortRadio.type = "radio";
  valueSortRadio.name = "construct-sort";
  valueSortRadio.value = "value";
  valueSortLabel.appendChild(valueSortRadio);
  valueSortLabel.appendChild(document.createTextNode(" Value"));
  valueSortLabel.style.display = "none";
  sortToggle.appendChild(valueSortLabel);

  // "Difference" radio — visible when two elements selected or one benchmark
  var diffSortLabel = document.createElement("label");
  diffSortLabel.title = "Sort constructs by rating difference (descending)";
  var diffSortRadio = document.createElement("input");
  diffSortRadio.type = "radio";
  diffSortRadio.name = "construct-sort";
  diffSortRadio.value = "difference";
  diffSortLabel.appendChild(diffSortRadio);
  diffSortLabel.appendChild(document.createTextNode(" Difference"));
  diffSortLabel.style.display = "none";
  sortToggle.appendChild(diffSortLabel);

  function computeValueOrder(elemIdx) {
    return constructs.map(function (c, i) {
      var val = ratings.values[i][elemIdx];
      return { index: i, val: (val != null) ? val : 0 };
    }).sort(function (a, b) { return b.val - a.val; });
  }

  function computeDifferenceOrder(elemIdxA, elemIdxB) {
    return constructs.map(function (c, i) {
      var rA = ratings.values[i][elemIdxA];
      var rB = ratings.values[i][elemIdxB];
      var diff = (rA != null && rB != null) ? Math.abs(rA - rB) : 0;
      return { index: i, diff: diff };
    }).sort(function (a, b) { return b.diff - a.diff; });
  }

  function getDifferenceTargets() {
    // Two selected elements → compare them
    if (selectedElements.length === 2) {
      return { a: selectedElements[0], b: selectedElements[1] };
    }
    // One benchmark + current profile element → compare profile element to benchmark
    if (benchmarkElements.length === 1 && selectedElementIndex >= 0 &&
        selectedElementIndex !== benchmarkElements[0]) {
      return { a: selectedElementIndex, b: benchmarkElements[0] };
    }
    return null;
  }

  function getValueSortTarget() {
    // Available when exactly one profile element, no benchmark
    if (selectedElementIndex >= 0 && benchmarkElements.length === 0) {
      return selectedElementIndex;
    }
    return -1;
  }

  function updateDynamicSortVisibility() {
    // Value sort
    var valueTarget = getValueSortTarget();
    var valueAvailable = valueTarget >= 0;
    valueSortLabel.style.display = valueAvailable ? "" : "none";
    if (!valueAvailable && constructSortMode === "value") {
      valueSortRadio.checked = false;
      var angularRadio = document.querySelector('input[name="construct-sort"][value="angular"]');
      if (angularRadio) angularRadio.checked = true;
      setConstructSort("angular");
    }
    if (valueAvailable) {
      valueSortLabel.title = "Sort by " + elements[valueTarget].name + " rating (descending)";
    }
    if (valueAvailable && constructSortMode === "value") {
      setConstructSort("value");
    }

    // Difference sort
    var targets = getDifferenceTargets();
    var diffAvailable = targets !== null;
    diffSortLabel.style.display = diffAvailable ? "" : "none";
    if (!diffAvailable && constructSortMode === "difference") {
      diffSortRadio.checked = false;
      var angularRadio = document.querySelector('input[name="construct-sort"][value="angular"]');
      if (angularRadio) angularRadio.checked = true;
      setConstructSort("angular");
    }
    if (diffAvailable) {
      var nameA = elements[targets.a].name;
      var nameB = elements[targets.b].name;
      diffSortLabel.title = "Sort by |" + nameA + " − " + nameB + "| (descending)";
    }
    if (diffAvailable && constructSortMode === "difference") {
      setConstructSort("difference");
    }
  }

  function setConstructSort(mode) {
    constructSortMode = mode;
    if (mode === "value") {
      var vt = getValueSortTarget();
      if (vt >= 0) {
        constructOrder = computeValueOrder(vt);
      } else {
        constructOrder = constructOrderAngular;
      }
    } else if (mode === "difference") {
      var targets = getDifferenceTargets();
      if (targets) {
        constructOrder = computeDifferenceOrder(targets.a, targets.b);
      } else {
        constructOrder = constructOrderAngular;
      }
    } else {
      constructOrder = (mode === "angular") ? constructOrderAngular : constructOrderOriginal;
    }
    // Reorder grid table rows
    var tbody = document.querySelector("#grid-table tbody");
    if (tbody) {
      var rows = Array.prototype.slice.call(tbody.querySelectorAll("tr"));
      var rowMap = {};
      for (var r = 0; r < rows.length; r++) {
        rowMap[rows[r].dataset.constructIndex] = rows[r];
      }
      for (var r = 0; r < constructOrder.length; r++) {
        tbody.appendChild(rowMap[constructOrder[r].index]);
      }
    }
    // Redraw profile plot
    if (selectedElementIndex >= 0) drawProfilePlot(selectedElementIndex);
  }

  // Wire up sort radio buttons
  var sortRadios = document.querySelectorAll('input[name="construct-sort"]');
  for (var sr = 0; sr < sortRadios.length; sr++) {
    sortRadios[sr].addEventListener("change", function () {
      setConstructSort(this.value);
    });
  }
  // Apply initial angular sort to grid table
  setConstructSort("angular");

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
    var poleFont = profileFontSize + "px -apple-system, BlinkMacSystemFont, sans-serif";
    var lineHeight = Math.round(profileFontSize * 1.2);
    var rowHeight = 32;
    var topPad = 42;
    var bottomPad = benchmarkElements.length > 0 ? 42 : 20;
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
    profileCtx.font = "11px -apple-system, BlinkMacSystemFont, sans-serif";
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
          bPoints.push({ x: bx, y: by, ci: ci });
        }
      }
      var bColor = benchColors[bi % benchColors.length];
      var bFootHovered = hoveredFootConstructIndex >= 0 && hoveredFootElementIndex === bIdx;
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
        var bIsHovered = bFootHovered && bPoints[bp].ci === hoveredFootConstructIndex;
        if (bIsHovered) {
          profileCtx.beginPath();
          profileCtx.arc(bPoints[bp].x, bPoints[bp].y, 10, 0, Math.PI * 2);
          profileCtx.fillStyle = isDark ? "rgba(80,160,255,0.2)" : "rgba(0,100,200,0.15)";
          profileCtx.fill();
        }
        profileCtx.beginPath();
        profileCtx.arc(bPoints[bp].x, bPoints[bp].y, bIsHovered ? 5 : 2.5, 0, Math.PI * 2);
        profileCtx.fillStyle = bColor;
        profileCtx.globalAlpha = bIsHovered ? 1.0 : 0.7;
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
      var isHoveredPoint = (profilePointHover && constructOrder[p].index === profilePointHover.ci) ||
        (hoveredFootConstructIndex >= 0 && hoveredFootElementIndex === elemIdx && constructOrder[p].index === hoveredFootConstructIndex);
      var dotRadius = isHoveredPoint ? 6 : 3.5;
      if (isHoveredPoint) {
        // Glow ring behind hovered point
        profileCtx.beginPath();
        profileCtx.arc(points[p].x, points[p].y, 10, 0, Math.PI * 2);
        profileCtx.fillStyle = isDark ? "rgba(80,160,255,0.2)" : "rgba(0,100,200,0.15)";
        profileCtx.fill();
      }
      profileCtx.beginPath();
      profileCtx.arc(points[p].x, points[p].y, dotRadius, 0, Math.PI * 2);
      profileCtx.fillStyle = isHoveredPoint ? (isDark ? "#66bbff" : "#2266cc") : dotFill;
      profileCtx.fill();
      profileCtx.strokeStyle = isDark ? "#222" : "#fff";
      profileCtx.lineWidth = isHoveredPoint ? 2 : 1;
      profileCtx.stroke();
    }

    // Legend for benchmarks
    if (benchmarkElements.length > 0) {
      var legendY = topPad + nc * rowHeight + 12;
      profileCtx.font = "bold 12px -apple-system, BlinkMacSystemFont, sans-serif";
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
    profileLayout = {
      topPad: topPad, rowHeight: rowHeight, nc: nc,
      leftMargin: leftMargin, plotWidth: plotWidth,
      scaleMin: scaleMin, scaleRange: scaleRange,
      points: points
    };
  }

  function updateElementGlows() {
    var baseColor = elemColorInput ? elemColorInput.value : "#bbbbbb";
    for (var i = 0; i < elementObjects.length; i++) {
      var bi = benchmarkElements.indexOf(i);
      var isBenchmark = bi >= 0;
      var isSelected = selectedElements.indexOf(i) >= 0;
      var active = (i === selectedElementIndex) || isBenchmark || isSelected;
      elementObjects[i].glow.visible = active && elementVisible[i];
      if (isBenchmark) {
        elementObjects[i].glow.material.color.set(benchColors[bi % benchColors.length]);
      } else if (isSelected) {
        elementObjects[i].glow.material.color.set(selectionColor);
      } else {
        elementObjects[i].glow.material.color.set(baseColor);
      }
      // Update sphere and label color based on selection
      if (isSelected) {
        elementObjects[i].sphere.material.color.set(selectionColor);
        elementObjects[i].label.element.style.color = "#" + new THREE.Color(selectionColor).getHexString();
      } else {
        elementObjects[i].sphere.material.color.set(baseColor);
        elementObjects[i].label.element.style.color = baseColor;
      }
    }
  }

  function selectElement(idx, multiSelect) {
    if (multiSelect) {
      var pos = selectedElements.indexOf(idx);
      if (pos >= 0) {
        selectedElements.splice(pos, 1);
      } else {
        selectedElements.push(idx);
      }
    } else {
      if (selectedElements.length === 1 && selectedElements[0] === idx) {
        selectedElements = [];
      } else {
        selectedElements = [idx];
      }
    }
    updateElementGlows();
    updateDynamicSortVisibility();
  }

  function selectConstruct(idx, multiSelect) {
    if (multiSelect) {
      var pos = selectedConstructs.indexOf(idx);
      if (pos >= 0) {
        selectedConstructs.splice(pos, 1);
      } else {
        selectedConstructs.push(idx);
      }
    } else {
      if (selectedConstructs.length === 1 && selectedConstructs[0] === idx) {
        selectedConstructs = [];
      } else {
        selectedConstructs = [idx];
      }
    }
    updateConstructSelection();
  }

  function updateConstructSelection() {
    var selHex = "#" + new THREE.Color(selectionColor).getHexString();
    for (var i = 0; i < constructObjects.length; i++) {
      var obj = constructObjects[i];
      var isSelected = selectedConstructs.indexOf(i) >= 0;
      if (isSelected) {
        obj.rightMarker.material.color.set(selectionColor);
        obj.leftMarker.material.color.set(selectionColor);
        obj.rightLabel.element.style.color = selHex;
        obj.leftLabel.element.style.color = selHex;
        obj.line.material.color.set(selectionColor);
      } else {
        // Restore original colors
        var con = constructs[i];
        var pref = con.preferred;
        var rColor, lColor;
        if (pref === "right") { rColor = preferredPoleColor; lColor = nonpreferredPoleColor; }
        else if (pref === "left") { rColor = nonpreferredPoleColor; lColor = preferredPoleColor; }
        else if (pref === "both") { rColor = preferredPoleColor; lColor = preferredPoleColor; }
        else { rColor = neutralPoleColor; lColor = neutralPoleColor; }
        obj.rightMarker.material.color.set(rColor);
        obj.leftMarker.material.color.set(lColor);
        obj.rightLabel.element.style.color = "";
        obj.leftLabel.element.style.color = "";
        obj.line.material.color.set(axisColorInput ? axisColorInput.value : "#888888");
      }
    }
  }

  function updateProfilePlot(elemIdx) {
    selectedElementIndex = elemIdx;
    updateElementGlows();
    updateDynamicSortVisibility();
    var profileTab = document.querySelector('.tab-content[data-tab="profile"]');
    if (profileTab && profileTab.classList.contains("active")) {
      if (elemIdx >= 0) {
        drawProfilePlot(elemIdx);
      }
    }
  }

  // --- Profile plot hover → highlight construct in 3D ---
  var profilePointHover = null; // { ci, prevLineVisible, prevProjections }

  function profilePointHoverEnter(ci) {
    var ei = selectedElementIndex;
    if (ei < 0) return;
    profilePointHover = {
      ci: ci, ei: ei,
      constructLineVisible: constructLineVisible[ci],
      constructVisible: constructVisible[ci],
      elementProjections: elementProjections[ei]
    };
    if (!constructVisible[ci]) constructVisible[ci] = true;
    if (!constructLineVisible[ci]) constructLineVisible[ci] = true;
    buildCalibration();
    if (!elementProjections[ei]) elementProjections[ei] = true;
    buildProjectionsForElement(ei);
    highlightGridCell(ei, ci);
  }

  function profilePointHoverLeave() {
    if (!profilePointHover) return;
    var p = profilePointHover;
    profilePointHover = null;
    if (!p.constructVisible) constructVisible[p.ci] = false;
    if (!p.constructLineVisible) constructLineVisible[p.ci] = false;
    buildCalibration();
    if (!p.elementProjections) elementProjections[p.ei] = false;
    buildProjectionsForElement(p.ei);
    unhighlightGridCell();
  }

  profileCanvas.addEventListener("mousemove", function (e) {
    var rect = profileCanvas.getBoundingClientRect();
    var scaleX = profileCanvas.width / (window.devicePixelRatio || 1) / rect.width;
    var scaleY = profileCanvas.height / (window.devicePixelRatio || 1) / rect.height;
    var mx = (e.clientX - rect.left) * scaleX;
    var y = (e.clientY - rect.top) * scaleY;
    var row = Math.floor((y - profileLayout.topPad) / profileLayout.rowHeight);
    var newIdx = -1;
    if (row >= 0 && row < profileLayout.nc) {
      newIdx = constructOrder[row].index;
    }
    var constructChanged = newIdx !== profileHoveredConstruct;
    if (constructChanged) {
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
    }

    // Detect proximity to a profile point
    var nearPoint = -1;
    var hitRadius = 10;
    if (profileLayout.points) {
      for (var pi = 0; pi < profileLayout.points.length; pi++) {
        var pt = profileLayout.points[pi];
        var ddx = mx - pt.x;
        var ddy = y - pt.y;
        if (ddx * ddx + ddy * ddy <= hitRadius * hitRadius) {
          nearPoint = constructOrder[pi].index;
          break;
        }
      }
    }

    // Handle point hover state
    var pointChanged = false;
    if (nearPoint >= 0 && (!profilePointHover || profilePointHover.ci !== nearPoint)) {
      profilePointHoverLeave();
      profilePointHoverEnter(nearPoint);
      pointChanged = true;
    } else if (nearPoint < 0 && profilePointHover) {
      profilePointHoverLeave();
      pointChanged = true;
    }

    if (constructChanged || pointChanged) {
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
    profilePointHoverLeave();
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

  function highlightGridCell(elementIndex, constructIndex) {
    var table = document.getElementById("grid-table");
    if (!table) return;
    unhighlightGridCell();
    if (elementIndex < 0 || constructIndex < 0) return;
    var cell = table.querySelector(
      'td.rating-cell[data-element-index="' + elementIndex + '"][data-construct-index="' + constructIndex + '"]'
    );
    if (cell) cell.classList.add("cell-highlight");
  }

  function unhighlightGridCell() {
    var table = document.getElementById("grid-table");
    if (!table) return;
    var highlighted = table.querySelectorAll(".cell-highlight");
    for (var k = 0; k < highlighted.length; k++) highlighted[k].classList.remove("cell-highlight");
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

  // --- Display section with sub-tabs ---
  var displaySection = addSectionTitle("Display", true);
  var displayBody = displaySection.body;

  // Sub-tab bar
  var displayTabBar = document.createElement("div");
  displayTabBar.className = "sub-tab-bar";
  var subTabNames = ["General", "Elements", "Constructs", "Axes"];
  var subTabBtns = [];
  var subTabPanels = [];

  subTabNames.forEach(function (name, ti) {
    var btn = document.createElement("button");
    btn.className = "sub-tab-btn" + (ti === 0 ? " active" : "");
    btn.textContent = name;
    btn.addEventListener("click", function () {
      subTabBtns.forEach(function (b) { b.classList.remove("active"); });
      subTabPanels.forEach(function (p) { p.classList.remove("active"); });
      btn.classList.add("active");
      subTabPanels[ti].classList.add("active");
    });
    displayTabBar.appendChild(btn);
    subTabBtns.push(btn);

    var panel = document.createElement("div");
    panel.className = "sub-tab-panel" + (ti === 0 ? " active" : "");
    subTabPanels.push(panel);
  });

  displayBody.appendChild(displayTabBar);
  subTabPanels.forEach(function (p) { displayBody.appendChild(p); });

  var tabGeneral = subTabPanels[0];
  var tabElements = subTabPanels[1];
  var tabConstructs = subTabPanels[2];
  var tabAxes = subTabPanels[3];

  // === General tab ===
  var cbOrtho = addToggle(tabGeneral, "Orthographic", false, function (v) {
    switchCamera(v);
  });
  var cbDarkMode = addToggle(tabGeneral, "Dark Mode", true, function (v) {
    document.body.classList.toggle("dark", v);
    scene.background = new THREE.Color(v ? 0x1a1a1a : 0xffffff);
    wireUniforms.uOpacityFront.value = v ? 0.25 : 0.15;
    var sphereCol = v ? "#ededed" : "#000000";
    wireUniforms.uColor.value.set(sphereCol);
    sphereColorInput.value = sphereCol;
    // Switch element color: light colors for dark mode, dark colors for light mode
    var curElem = new THREE.Color(elemColorInput.value);
    var brightness = curElem.r * 0.299 + curElem.g * 0.587 + curElem.b * 0.114;
    if (v && brightness <= 0.5) {
      elemColorInput.value = "#bbbbbb";
      updateElementGlows();
    } else if (!v && brightness > 0.5) {
      elemColorInput.value = "#505050";
      updateElementGlows();
    }
  });
  var cbWireframe = addToggle(tabGeneral, "Wireframe Sphere", true, function (v) { sphereGroup.visible = v; });
  var cbDepthFade = addToggle(tabGeneral, "Depth Fade", true, function (v) {
    wireUniforms.uDepthFade.value = v ? 1.0 : 0.0;
  });
  var cbSilhouette = addToggle(tabGeneral, "Silhouette Ring", false, function (v) {
    silhouetteGroup.visible = v;
  });

  // Silhouette ring color
  var silColorLabel = document.createElement("label");
  var silColorInput = document.createElement("input");
  silColorInput.type = "color";
  silColorInput.value = "#888888";
  silColorInput.addEventListener("input", function () {
    silhouetteRingMat.color.set(silColorInput.value);
    silhouetteGlowUniforms.uColor.value.set(silColorInput.value);
  });
  silColorLabel.appendChild(silColorInput);
  silColorLabel.appendChild(document.createTextNode(" Ring Color"));
  tabGeneral.appendChild(silColorLabel);

  // Silhouette ring thickness
  var silThickLabel = document.createElement("label");
  var silThickRange = document.createElement("input");
  silThickRange.type = "range";
  silThickRange.min = "1";
  silThickRange.max = "20";
  silThickRange.step = "1";
  silThickRange.value = "2";
  silThickRange.addEventListener("input", function () {
    var r = parseInt(silThickRange.value) / 1000;
    var newGeom = new THREE.TorusBufferGeometry(1, r, 8, 128);
    silhouetteRingMesh.geometry.dispose();
    silhouetteRingMesh.geometry = newGeom;
  });
  silThickLabel.appendChild(silThickRange);
  silThickLabel.appendChild(document.createTextNode(" Ring Thickness"));
  tabGeneral.appendChild(silThickLabel);

  // Silhouette glow strength
  var silGlowLabel = document.createElement("label");
  var silGlowRange = document.createElement("input");
  silGlowRange.type = "range";
  silGlowRange.min = "0";
  silGlowRange.max = "100";
  silGlowRange.step = "1";
  silGlowRange.value = "0";
  silGlowRange.addEventListener("input", function () {
    silhouetteGlowUniforms.uOpacity.value = parseInt(silGlowRange.value) / 100 * 0.5;
  });
  silGlowLabel.appendChild(silGlowRange);
  silGlowLabel.appendChild(document.createTextNode(" Ring Glow"));
  tabGeneral.appendChild(silGlowLabel);

  // Silhouette glow size
  var silGlowSizeLabel = document.createElement("label");
  var silGlowSizeRange = document.createElement("input");
  silGlowSizeRange.type = "range";
  silGlowSizeRange.min = "5";
  silGlowSizeRange.max = "80";
  silGlowSizeRange.step = "1";
  silGlowSizeRange.value = "20";
  silGlowSizeRange.addEventListener("input", function () {
    var r = parseInt(silGlowSizeRange.value) / 1000;
    var newGeom = new THREE.TorusBufferGeometry(1, r, 32, 128);
    silhouetteGlowMesh.geometry.dispose();
    silhouetteGlowMesh.geometry = newGeom;
  });
  silGlowSizeLabel.appendChild(silGlowSizeRange);
  silGlowSizeLabel.appendChild(document.createTextNode(" Glow Size"));
  tabGeneral.appendChild(silGlowSizeLabel);

  // Sphere color chooser
  var sphereColorLabel = document.createElement("label");
  var sphereColorInput = document.createElement("input");
  sphereColorInput.type = "color";
  sphereColorInput.value = "#ededed";
  sphereColorInput.addEventListener("input", function () {
    wireUniforms.uColor.value.set(sphereColorInput.value);
  });
  sphereColorLabel.appendChild(sphereColorInput);
  sphereColorLabel.appendChild(document.createTextNode(" Sphere Color"));
  tabGeneral.appendChild(sphereColorLabel);

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
  tabGeneral.appendChild(gridDensityLabel);

  var cbDeconflict = addToggle(tabGeneral, "Deconflict Labels", true, function (v) {
    labelDeconflict = v;
  });

  // === Elements tab ===
  // Element color chooser
  var elemColorLabel = document.createElement("label");
  var elemColorInput = document.createElement("input");
  elemColorInput.type = "color";
  elemColorInput.value = "#bbbbbb";
  elemColorInput.addEventListener("input", function () {
    updateElementGlows();
    if (selectedElementIndex >= 0) updateProfilePlot(selectedElementIndex);
  });
  elemColorLabel.appendChild(elemColorInput);
  elemColorLabel.appendChild(document.createTextNode(" Element Color"));
  tabElements.appendChild(elemColorLabel);

  var cbElemLabels = addToggle(tabElements, "Element Labels", true, function (v) {
    for (var i = 0; i < elements.length; i++) {
      elementLabelVisible[i] = v;
      elementObjects[i].label.visible = v && elementVisible[i];
    }
  });

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
  elemSizeLabel.appendChild(document.createTextNode(" Label Size"));
  tabElements.appendChild(elemSizeLabel);

  // Projection line thickness
  var projWidthLabel = document.createElement("label");
  var projWidthRange = document.createElement("input");
  projWidthRange.type = "range";
  projWidthRange.min = "0.5";
  projWidthRange.max = "5";
  projWidthRange.step = "0.5";
  projWidthRange.value = "3";
  projWidthRange.addEventListener("input", function () {
    projLineScale = parseFloat(projWidthRange.value);
    rebuildAllProjections();
  });
  projWidthLabel.appendChild(projWidthRange);
  projWidthLabel.appendChild(document.createTextNode(" Projection Thickness"));
  tabElements.appendChild(projWidthLabel);

  var cbProjError = addToggle(tabElements, "Projection Errors", false, function (v) {
    showProjectionErrors = v;
    rebuildAllProjections();
  });

  // === Constructs tab ===
  var cbConLabels = addToggle(tabConstructs, "Construct Labels", true, function (v) {
    for (var i = 0; i < constructs.length; i++) {
      constructLabelVisible[i] = v;
    }
  });
  var cbRingProjection = addToggle(tabConstructs, "Ring Projection", false, function (v) {
    ringProjectionMode = v;
  });

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
  conSizeLabel.appendChild(document.createTextNode(" Label Size"));
  tabConstructs.appendChild(conSizeLabel);

  // Construct axis color chooser
  var axisColorLabel = document.createElement("label");
  var axisColorInput = document.createElement("input");
  axisColorInput.type = "color";
  axisColorInput.value = "#888888";
  axisColorInput.addEventListener("input", function () {
    for (var i = 0; i < constructObjects.length; i++) {
      if (selectedConstructs.indexOf(i) < 0) {
        constructObjects[i].line.material.color.set(axisColorInput.value);
      }
    }
    buildCalibration();
  });
  axisColorLabel.appendChild(axisColorInput);
  axisColorLabel.appendChild(document.createTextNode(" Axis Color"));
  tabConstructs.appendChild(axisColorLabel);

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
  tabConstructs.appendChild(axisWidthLabel);

  var cbCalLabels = addToggle(tabConstructs, "Calibration Labels", true, function (v) {
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
  calSizeRange.max = "24";
  calSizeRange.value = "11";
  calSizeRange.addEventListener("input", function () {
    var val = parseInt(calSizeRange.value);
    for (var k = 0; k < calibrationLabelsGroup.children.length; k++) {
      calibrationLabelsGroup.children[k].element.style.fontSize = val + "px";
      calibrationLabelsGroup.children[k].element.style.display = val === 0 ? "none" : "";
    }
  });
  calSizeLabel.appendChild(calSizeRange);
  calSizeLabel.appendChild(document.createTextNode(" Calibration Size"));
  tabConstructs.appendChild(calSizeLabel);

  // Calibration tick size slider
  var tickSizeLabel = document.createElement("label");
  var tickSizeRange = document.createElement("input");
  tickSizeRange.type = "range";
  tickSizeRange.min = "0";
  tickSizeRange.max = "60";
  tickSizeRange.step = "1";
  tickSizeRange.value = "20";
  tickSizeRange.addEventListener("input", function () {
    calibrationTickSize = parseInt(tickSizeRange.value) / 1000;
    buildCalibration();
  });
  tickSizeLabel.appendChild(tickSizeRange);
  tickSizeLabel.appendChild(document.createTextNode(" Tick Size"));
  tabConstructs.appendChild(tickSizeLabel);

  // Calibration tick thickness slider
  var tickWidthLabel = document.createElement("label");
  var tickWidthRange = document.createElement("input");
  tickWidthRange.type = "range";
  tickWidthRange.min = "0.5";
  tickWidthRange.max = "5";
  tickWidthRange.step = "0.5";
  tickWidthRange.value = "2";
  tickWidthRange.addEventListener("input", function () {
    calibrationTickWidth = parseFloat(tickWidthRange.value);
    buildCalibration();
  });
  tickWidthLabel.appendChild(tickWidthRange);
  tickWidthLabel.appendChild(document.createTextNode(" Tick Thickness"));
  tabConstructs.appendChild(tickWidthLabel);

  // Elevation filter slider
  var elevLabel = document.createElement("label");
  var elevRange = document.createElement("input");
  elevRange.type = "range";
  elevRange.min = "0";
  elevRange.max = "90";
  elevRange.step = "1";
  elevRange.value = "90";
  elevRange.addEventListener("input", function () {
    elevationFilterAngle = parseInt(elevRange.value);
    buildCalibration();
    rebuildAllProjections();
  });
  elevLabel.appendChild(elevRange);
  elevLabel.appendChild(document.createTextNode(" Elevation Filter (" + elevRange.value + "\u00B0)"));
  elevRange.addEventListener("input", function () {
    elevLabel.lastChild.textContent = " Elevation Filter (" + elevRange.value + "\u00B0)";
  });
  tabConstructs.appendChild(elevLabel);

  // === Axes tab ===
  var cbAxes = addToggle(tabAxes, "Axes", false, function (v) {
    axesGroup.visible = v;
    for (var a = 0; a < axisLabels.length; a++) {
      axisLabels[a].visible = v;
    }
  });

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
  tabAxes.appendChild(pcAxisLabel);

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
    _camAnim = {
      startTime: performance.now(),
      duration: 800,
      posStart: camera.position.clone(),
      posEnd: initialCameraPosition.clone().normalize().multiplyScalar(camera.position.length()),
      upStart: camera.up.clone(),
      upEnd: new THREE.Vector3(0, 1, 0)
    };
  });
  guiPanel.appendChild(resetBtn);

  var saveBtn = document.createElement("button");
  saveBtn.className = "action-btn";
  saveBtn.textContent = "Save As\u2026";
  saveBtn.addEventListener("click", function () {
    var blob = new Blob([_savedHTML], { type: "text/html;charset=utf-8" });
    var url = URL.createObjectURL(blob);
    var a = document.createElement("a");
    a.href = url;
    a.download = "biplot3d.html";
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
  });
  guiPanel.appendChild(saveBtn);

  // --- Snapshots ---
  var snapshotSection = addSectionTitle("Snapshots");
  var snapshots = [];
  var snapshotListDiv = document.createElement("div");
  snapshotListDiv.className = "snapshot-list";
  snapshotSection.body.appendChild(snapshotListDiv);

  var snapshotBtn = document.createElement("button");
  snapshotBtn.className = "action-btn";
  snapshotBtn.textContent = "Take Snapshot";
  snapshotSection.body.appendChild(snapshotBtn);

  function captureState() {
    return {
      camera: { px: camera.position.x, py: camera.position.y, pz: camera.position.z,
                 tx: controls.target.x, ty: controls.target.y, tz: controls.target.z },
      elementVisible: elementVisible.slice(),
      elementLabelVisible: elementLabelVisible.slice(),
      constructVisible: constructVisible.slice(),
      constructLabelVisible: constructLabelVisible.slice(),
      elementProjections: elementProjections.slice(),
      constructLineVisible: constructLineVisible.slice(),
      selectedElements: selectedElements.slice(),
      selectedConstructs: selectedConstructs.slice(),
      benchmarkElements: benchmarkElements.slice(),
      selectedElementIndex: selectedElementIndex,
      orthographic: cbOrtho.checked,
      darkMode: cbDarkMode.checked,
      wireframe: cbWireframe.checked,
      depthFade: cbDepthFade.checked,
      silhouette: cbSilhouette.checked,
      silColor: silColorInput.value,
      silThickness: silThickRange.value,
      silGlow: silGlowRange.value,
      silGlowSize: silGlowSizeRange.value,
      axes: cbAxes.checked,
      calLabels: cbCalLabels.checked,
      elemLabels: cbElemLabels.checked,
      conLabels: cbConLabels.checked,
      ringProjection: cbRingProjection.checked,
      projErrors: cbProjError.checked,
      deconflict: cbDeconflict.checked,
      sphereColor: sphereColorInput.value,
      elemColor: elemColorInput.value,
      axisColor: axisColorInput.value,
      elemLabelSize: elemSizeRange.value,
      conLabelSize: conSizeRange.value,
      calLabelSize: calSizeRange.value,
      tickSize: tickSizeRange.value,
      tickWidth: tickWidthRange.value,
      axisWidth: axisWidthRange.value,
      projWidth: projWidthRange.value,
      pcAxisWidth: pcAxisRange.value,
      gridDensity: gridDensityRange.value,
      profileFontSize: pfRange.value,
      elevationFilter: elevRange.value
    };
  }

  function setCheckbox(cb, val) {
    if (cb.checked !== val) {
      cb.checked = val;
      cb.dispatchEvent(new Event("change"));
    }
  }

  function restoreState(s) {
    // Camera
    camera.position.set(s.camera.px, s.camera.py, s.camera.pz);
    controls.target.set(s.camera.tx, s.camera.ty, s.camera.tz);
    controls.update();

    // Display toggles
    if (s.orthographic !== undefined) setCheckbox(cbOrtho, s.orthographic);
    setCheckbox(cbDarkMode, s.darkMode);
    setCheckbox(cbWireframe, s.wireframe);
    setCheckbox(cbDepthFade, s.depthFade);
    if (s.silhouette !== undefined) setCheckbox(cbSilhouette, s.silhouette);
    if (s.silColor !== undefined) {
      silColorInput.value = s.silColor;
      silColorInput.dispatchEvent(new Event("input"));
    }
    if (s.silThickness !== undefined) {
      silThickRange.value = s.silThickness;
      silThickRange.dispatchEvent(new Event("input"));
    }
    if (s.silGlow !== undefined) {
      silGlowRange.value = s.silGlow;
      silGlowRange.dispatchEvent(new Event("input"));
    }
    if (s.silGlowSize !== undefined) {
      silGlowSizeRange.value = s.silGlowSize;
      silGlowSizeRange.dispatchEvent(new Event("input"));
    }
    setCheckbox(cbAxes, s.axes);
    setCheckbox(cbCalLabels, s.calLabels);
    setCheckbox(cbElemLabels, s.elemLabels);
    setCheckbox(cbConLabels, s.conLabels);
    if (s.ringProjection !== undefined) setCheckbox(cbRingProjection, s.ringProjection);
    if (s.projErrors !== undefined) setCheckbox(cbProjError, s.projErrors);
    setCheckbox(cbDeconflict, s.deconflict);

    // Colors
    sphereColorInput.value = s.sphereColor;
    wireUniforms.uColor.value.set(s.sphereColor);
    elemColorInput.value = s.elemColor;
    axisColorInput.value = s.axisColor;

    // Sliders
    elemSizeRange.value = s.elemLabelSize;
    elemSizeRange.dispatchEvent(new Event("input"));
    conSizeRange.value = s.conLabelSize;
    conSizeRange.dispatchEvent(new Event("input"));
    calSizeRange.value = s.calLabelSize;
    calSizeRange.dispatchEvent(new Event("input"));
    if (s.tickSize !== undefined) {
      tickSizeRange.value = s.tickSize;
      tickSizeRange.dispatchEvent(new Event("input"));
    }
    if (s.tickWidth !== undefined) {
      tickWidthRange.value = s.tickWidth;
      tickWidthRange.dispatchEvent(new Event("input"));
    }
    axisWidthRange.value = s.axisWidth;
    axisWidthRange.dispatchEvent(new Event("input"));
    projWidthRange.value = s.projWidth;
    projWidthRange.dispatchEvent(new Event("input"));
    pcAxisRange.value = s.pcAxisWidth;
    pcAxisRange.dispatchEvent(new Event("input"));
    gridDensityRange.value = s.gridDensity;
    gridDensityRange.dispatchEvent(new Event("input"));
    if (s.profileFontSize) {
      pfRange.value = s.profileFontSize;
      pfRange.dispatchEvent(new Event("input"));
    }
    if (s.elevationFilter !== undefined) {
      elevRange.value = s.elevationFilter;
      elevRange.dispatchEvent(new Event("input"));
    }

    // Element/construct visibility
    for (var i = 0; i < elements.length; i++) {
      setCheckbox(elemCheckboxes[i], s.elementVisible[i]);
      elementLabelVisible[i] = s.elementLabelVisible[i];
      elementObjects[i].label.visible = s.elementLabelVisible[i] && s.elementVisible[i];
      elementProjections[i] = s.elementProjections[i];
    }
    for (var i = 0; i < constructs.length; i++) {
      setCheckbox(conCheckboxes[i], s.constructVisible[i]);
      constructLabelVisible[i] = s.constructLabelVisible[i];
      constructLineVisible[i] = s.constructLineVisible[i];
    }
    buildCalibration();
    rebuildAllProjections();

    // Selection & benchmarks
    benchmarkElements = s.benchmarkElements.slice();
    selectedElements = s.selectedElements.slice();
    selectedConstructs = s.selectedConstructs.slice();
    selectedElementIndex = s.selectedElementIndex;
    updateElementGlows();
    updateConstructSelection();
    updateDynamicSortVisibility();
    if (selectedElementIndex >= 0) updateProfilePlot(selectedElementIndex);
    removeBenchmarksBtn.style.display = benchmarkElements.length > 0 ? "" : "none";
  }

  function renderSnapshotList() {
    snapshotListDiv.innerHTML = "";
    for (var i = 0; i < snapshots.length; i++) {
      (function (idx) {
        var row = document.createElement("div");
        row.className = "snapshot-row";

        var nameSpan = document.createElement("span");
        nameSpan.className = "snapshot-name";
        nameSpan.textContent = snapshots[idx].name;
        nameSpan.title = "Click to restore, double-click to rename";
        nameSpan.addEventListener("click", function () {
          restoreState(snapshots[idx].state);
        });
        nameSpan.addEventListener("dblclick", function (e) {
          e.stopPropagation();
          var input = document.createElement("input");
          input.type = "text";
          input.className = "snapshot-rename";
          input.value = snapshots[idx].name;
          row.replaceChild(input, nameSpan);
          input.focus();
          input.select();
          function finishRename() {
            var val = input.value.trim();
            if (val) snapshots[idx].name = val;
            nameSpan.textContent = snapshots[idx].name;
            row.replaceChild(nameSpan, input);
          }
          input.addEventListener("blur", finishRename);
          input.addEventListener("keydown", function (ke) {
            if (ke.key === "Enter") { input.blur(); }
            if (ke.key === "Escape") { input.value = snapshots[idx].name; input.blur(); }
          });
        });

        var delBtn = document.createElement("button");
        delBtn.className = "snapshot-del";
        delBtn.textContent = "\u00D7";
        delBtn.title = "Delete snapshot";
        delBtn.addEventListener("click", function (e) {
          e.stopPropagation();
          snapshots.splice(idx, 1);
          renderSnapshotList();
        });

        row.appendChild(nameSpan);
        row.appendChild(delBtn);
        snapshotListDiv.appendChild(row);
      })(i);
    }
  }

  snapshotBtn.addEventListener("click", function () {
    var name = "Snapshot " + (snapshots.length + 1);
    snapshots.push({ name: name, state: captureState() });
    renderSnapshotList();
  });

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
    var newHoveredFootElem = -1;
    var newHoveredFootCon = -1;

    // Find first visible intersect
    var hit = null;
    for (var hi = 0; hi < intersects.length; hi++) {
      var hObj = intersects[hi].object;
      if (!hObj.visible) continue;
      var hud = hObj.userData;
      if (hud && hud.type === "element" && !elementVisible[hud.index]) continue;
      if (hud && hud.type === "construct" && !constructVisible[hud.index]) continue;
      hit = hObj;
      break;
    }

    if (hit) {
      var ud = hit.userData;
      if (ud && ud.name) {
        var qualStr = ud.quality !== undefined
          ? " (quality: " + (ud.quality * 100).toFixed(1) + "%)" : "";
        tooltip.textContent = ud.name + qualStr;
        tooltip.style.display = "block";
        tooltip.style.left = (event.clientX + 12) + "px";
        tooltip.style.top = (event.clientY - 8) + "px";
        if (ud.type === "element") newHoveredElement = ud.index;
        if (ud.type === "construct") newHoveredConstruct = ud.index;
      } else if (ud && ud.type === "projectionFoot") {
        var eName = elements[ud.elementIndex].name;
        var cLeft = constructs[ud.constructIndex].left_pole;
        var cRight = constructs[ud.constructIndex].right_pole;
        tooltip.textContent = eName + " → " + cLeft + " – " + cRight;
        tooltip.style.display = "block";
        tooltip.style.left = (event.clientX + 12) + "px";
        tooltip.style.top = (event.clientY - 8) + "px";
        newHoveredFootElem = ud.elementIndex;
        newHoveredFootCon = ud.constructIndex;
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

    // Highlight grid cell when hovering projection foot dot
    if (newHoveredFootElem >= 0 && newHoveredFootCon >= 0) {
      highlightGridCell(newHoveredFootElem, newHoveredFootCon);
    } else {
      unhighlightGridCell();
    }

    // Highlight corresponding point in profile plot when hovering projection foot
    var footIsDisplayed = newHoveredFootElem >= 0 && newHoveredFootCon >= 0 &&
      (newHoveredFootElem === selectedElementIndex || benchmarkElements.indexOf(newHoveredFootElem) >= 0);
    var newFootCon = footIsDisplayed ? newHoveredFootCon : -1;
    var newFootElem = footIsDisplayed ? newHoveredFootElem : -1;
    if (newFootCon !== hoveredFootConstructIndex || newFootElem !== hoveredFootElementIndex) {
      hoveredFootConstructIndex = newFootCon;
      hoveredFootElementIndex = newFootElem;
      if (selectedElementIndex >= 0) drawProfilePlot(selectedElementIndex);
    }

    renderer.domElement.style.cursor = (newHoveredElement >= 0 || newHoveredConstruct >= 0 || newHoveredFootElem >= 0) ? "pointer" : "default";
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

  // Single click: select element (Cmd/Ctrl+click for multi-select)
  function onClick(event) {
    if (lassoJustFinished) { lassoJustFinished = false; return; }
    var rect = renderer.domElement.getBoundingClientRect();
    mouse.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
    mouse.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;

    raycaster.setFromCamera(mouse, camera);
    var intersects = raycaster.intersectObjects(hoverTargets);

    if (intersects.length > 0) {
      var ud = intersects[0].object.userData;
      if (ud.type === "element") {
        selectElement(ud.index, event.metaKey || event.ctrlKey);
        if (!(event.metaKey || event.ctrlKey)) { selectedConstructs = []; updateConstructSelection(); }
        updateProfilePlot(ud.index);
      } else if (ud.type === "construct") {
        selectConstruct(ud.index, event.metaKey || event.ctrlKey);
        if (!(event.metaKey || event.ctrlKey)) { selectedElements = []; updateElementGlows(); updateDynamicSortVisibility(); }
      }
    } else {
      // Click on background: clear selection
      if (!event.metaKey && !event.ctrlKey) {
        selectedElements = [];
        selectedConstructs = [];
        updateElementGlows();
        updateConstructSelection();
        updateDynamicSortVisibility();
      }
    }
  }

  // --- Rotate to construct ---
  function restoreOriginalCoords() {
    for (var i = 0; i < elements.length; i++) {
      elements[i].x = initialElements[i].x;
      elements[i].y = initialElements[i].y;
      elements[i].z = initialElements[i].z;
      elementObjects[i].sphere.position.set(elements[i].x, elements[i].y, elements[i].z);
      elementObjects[i].label.position.set(elements[i].x, elements[i].y + 0.05, elements[i].z);
      elementObjects[i].glow.position.set(elements[i].x, elements[i].y, elements[i].z);
    }
    for (var i = 0; i < constructs.length; i++) {
      constructs[i].x = initialConstructs[i].x;
      constructs[i].y = initialConstructs[i].y;
      constructs[i].z = initialConstructs[i].z;
      var r = constructs[i];
      var len = Math.sqrt(r.x * r.x + r.y * r.y + r.z * r.z);
      if (len === 0) len = 1;
      var sc = constructSphereCoords[i];
      sc.rx = r.x / len * sphereRadius; sc.ry = r.y / len * sphereRadius; sc.rz = r.z / len * sphereRadius;
      sc.lx = -sc.rx; sc.ly = -sc.ry; sc.lz = -sc.rz;

      var obj = constructObjects[i];
      obj.rightMarker.position.set(sc.rx, sc.ry, sc.rz);
      obj.rightLabel.position.set(sc.rx, sc.ry, sc.rz);
      obj.leftMarker.position.set(sc.lx, sc.ly, sc.lz);
      obj.leftLabel.position.set(sc.lx, sc.ly, sc.lz);

      var from = new THREE.Vector3(sc.lx, sc.ly, sc.lz);
      var to = new THREE.Vector3(sc.rx, sc.ry, sc.rz);
      var mid = new THREE.Vector3().addVectors(from, to).multiplyScalar(0.5);
      var lineDir = new THREE.Vector3().subVectors(to, from).normalize();
      var q = new THREE.Quaternion().setFromUnitVectors(new THREE.Vector3(0, 1, 0), lineDir);
      obj.line.position.copy(mid);
      obj.line.quaternion.copy(q);
      obj._origLinePos = mid.clone();
      obj._origLineQuat = q.clone();
    }
    if (calibration && initialCalibCoords) {
      for (var i = 0; i < calibration.construct_coords.length; i++) {
        calibration.construct_coords[i][0] = initialCalibCoords[i][0];
        calibration.construct_coords[i][1] = initialCalibCoords[i][1];
        calibration.construct_coords[i][2] = initialCalibCoords[i][2];
      }
    }
    buildCalibration();
    rebuildAllProjections();
  }

  // Camera-only animation: orbit the camera instead of rotating data.
  // Visually identical to rotating the globe by hand.
  var _camAnim = null; // { startTime, duration, posStart, posEnd, upStart, upEnd }

  function rotateToConstruct(conIdx) {
    var con = constructs[conIdx];
    var cDir = new THREE.Vector3(con.x, con.y, con.z).normalize();
    if (cDir.x < 0) cDir.negate(); // pick pole closer to +x for minimal rotation

    // Q maps construct direction → +x. Applying Q^-1 to the camera gives
    // the viewpoint from which the construct appears along the x-axis.
    var Q = new THREE.Quaternion().setFromUnitVectors(cDir, new THREE.Vector3(1, 0, 0));
    var Qinv = Q.clone().conjugate();

    _camAnim = {
      startTime: performance.now(),
      duration: 800,
      posStart: camera.position.clone(),
      posEnd: camera.position.clone().applyQuaternion(Qinv),
      upStart: camera.up.clone(),
      upEnd: camera.up.clone().applyQuaternion(Qinv)
    };
  }

  // Easing: smooth ease-in-out
  function easeInOutCubic(t) {
    return t < 0.5 ? 4 * t * t * t : 1 - Math.pow(-2 * t + 2, 3) / 2;
  }

  function tickCameraAnimation(now) {
    if (!_camAnim) return;
    var t = (now - _camAnim.startTime) / _camAnim.duration;
    if (t >= 1) t = 1;
    var ease = easeInOutCubic(t);

    // Slerp camera position on sphere (constant distance)
    var startDir = _camAnim.posStart.clone().normalize();
    var endDir = _camAnim.posEnd.clone().normalize();
    var posQuat = new THREE.Quaternion().setFromUnitVectors(startDir, endDir);
    var interpPosQuat = new THREE.Quaternion().slerp(posQuat, ease);
    camera.position.copy(_camAnim.posStart).applyQuaternion(interpPosQuat);

    // Slerp camera up vector
    var upQuat = new THREE.Quaternion().setFromUnitVectors(
      _camAnim.upStart.clone().normalize(),
      _camAnim.upEnd.clone().normalize()
    );
    var interpUpQuat = new THREE.Quaternion().slerp(upQuat, ease);
    camera.up.copy(_camAnim.upStart).applyQuaternion(interpUpQuat).normalize();

    controls.update();

    if (t >= 1) {
      _camAnim = null;
    }
  }

  // --- Context menu ---
  var contextMenu = document.getElementById("context-menu");
  var contextTargetElement = -1;

  function hideContextMenu() {
    contextMenu.style.display = "none";
    contextTargetElement = -1;
  }

  function addMenuItem(text, onClick, parent) {
    var container = parent || contextMenu;
    var item = document.createElement("div");
    item.className = "menu-item";
    item.textContent = text;
    item.addEventListener("click", function (e) {
      e.stopPropagation();
      onClick();
      hideContextMenu();
    });
    container.appendChild(item);
  }

  function addSubmenu(text) {
    var wrapper = document.createElement("div");
    wrapper.className = "menu-submenu";
    var trigger = document.createElement("div");
    trigger.className = "menu-item";
    trigger.textContent = text;
    wrapper.appendChild(trigger);
    var panel = document.createElement("div");
    panel.className = "submenu-panel";
    wrapper.appendChild(panel);
    contextMenu.appendChild(wrapper);
    return panel;
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

    // Determine target set: use selection if right-clicked element is part of it
    var targets = (selectedElements.length > 1 && selectedElements.indexOf(elemIdx) >= 0)
      ? selectedElements.slice()
      : [elemIdx];
    var isMulti = targets.length > 1;
    var suffix = isMulti ? " (" + targets.length + " elements)" : "";

    // Benchmark
    var allBenchmarked = targets.every(function (i) { return benchmarkElements.indexOf(i) >= 0; });
    addMenuItem((allBenchmarked ? "Remove benchmark" : "Add as benchmark") + suffix, function () {
      for (var t = 0; t < targets.length; t++) {
        var idx = targets[t];
        var bi = benchmarkElements.indexOf(idx);
        if (allBenchmarked) {
          if (bi >= 0) benchmarkElements.splice(bi, 1);
        } else {
          if (bi < 0) benchmarkElements.push(idx);
        }
      }
      if (selectedElementIndex >= 0) updateProfilePlot(selectedElementIndex);
    });

    // Projections
    var allProj = targets.every(function (i) { return elementProjections[i]; });
    addMenuItem((allProj ? "Hide projections" : "Show projections") + suffix, function () {
      for (var t = 0; t < targets.length; t++) {
        elementProjections[targets[t]] = !allProj;
        buildProjectionsForElement(targets[t]);
      }
    });

    var anyProjAll = elementProjections.some(function (v) { return v; });
    addMenuItem(anyProjAll ? "Hide all projections" : "Show all projections", function () {
      var newVal = !anyProjAll;
      for (var i = 0; i < elements.length; i++) {
        elementProjections[i] = newVal;
        buildProjectionsForElement(i);
      }
    });

    // Labels
    var allLabelsVis = targets.every(function (i) { return elementLabelVisible[i]; });
    addMenuItem((allLabelsVis ? "Hide labels" : "Show labels") + suffix, function () {
      for (var t = 0; t < targets.length; t++) {
        var idx = targets[t];
        elementLabelVisible[idx] = !allLabelsVis;
        elementObjects[idx].label.visible = elementLabelVisible[idx] && elementVisible[idx];
      }
    });

    // Hide
    addMenuItem("Hide" + suffix, function () {
      for (var t = 0; t < targets.length; t++) {
        elemCheckboxes[targets[t]].checked = false;
        elemCheckboxes[targets[t]].dispatchEvent(new Event("change"));
      }
    });

    // Keep (hide all others)
    addMenuItem("Keep only" + suffix, function () {
      for (var i = 0; i < elements.length; i++) {
        var keep = targets.indexOf(i) >= 0;
        if (elemCheckboxes[i].checked !== keep) {
          elemCheckboxes[i].checked = keep;
          elemCheckboxes[i].dispatchEvent(new Event("change"));
        }
      }
    });

    showContextMenuAt(x, y);
  }

  function showConstructContextMenu(x, y, conIdx) {
    contextMenu.innerHTML = "";

    // Determine target set: use selection if right-clicked construct is part of it
    var targets = (selectedConstructs.length > 1 && selectedConstructs.indexOf(conIdx) >= 0)
      ? selectedConstructs.slice()
      : [conIdx];
    var isMulti = targets.length > 1;
    var suffix = isMulti ? " (" + targets.length + " constructs)" : "";

    // Calibrated axis
    var allAxis = targets.every(function (i) { return constructLineVisible[i]; });
    addMenuItem((allAxis ? "Hide calibrated axis" : "Show calibrated axis") + suffix, function () {
      for (var t = 0; t < targets.length; t++) {
        constructLineVisible[targets[t]] = !allAxis;
      }
      buildCalibration();
      rebuildAllProjections();
    });

    // Labels
    var allLabelsVis = targets.every(function (i) { return constructLabelVisible[i]; });
    addMenuItem((allLabelsVis ? "Hide labels" : "Show labels") + suffix, function () {
      for (var t = 0; t < targets.length; t++) {
        constructLabelVisible[targets[t]] = !allLabelsVis;
      }
    });

    // Hide
    addMenuItem("Hide" + suffix, function () {
      for (var t = 0; t < targets.length; t++) {
        conCheckboxes[targets[t]].checked = false;
        conCheckboxes[targets[t]].dispatchEvent(new Event("change"));
      }
    });

    // Keep (hide all others)
    addMenuItem("Keep only" + suffix, function () {
      for (var i = 0; i < constructs.length; i++) {
        var keep = targets.indexOf(i) >= 0;
        if (conCheckboxes[i].checked !== keep) {
          conCheckboxes[i].checked = keep;
          conCheckboxes[i].dispatchEvent(new Event("change"));
        }
      }
    });

    // Rotate to align construct with PC1 (only for single construct)
    if (!isMulti) {
      addMenuItem("Rotate to x-axis", function () {
        rotateToConstruct(conIdx);
      });
    }

    showContextMenuAt(x, y);
  }

  function showBackgroundContextMenu(x, y) {
    contextMenu.innerHTML = "";

    var pcVisible = axesGroup.visible;
    addMenuItem(pcVisible ? "Hide PC axes" : "Show PC axes", function () {
      axesGroup.visible = !pcVisible;
      for (var a = 0; a < axisLabels.length; a++) {
        axisLabels[a].visible = !pcVisible;
      }
    });

    var anyProj = elementProjections.some(function (v) { return v; });
    addMenuItem(anyProj ? "Hide all projections" : "Show all projections", function () {
      var newVal = !anyProj;
      for (var i = 0; i < elements.length; i++) {
        elementProjections[i] = newVal;
        buildProjectionsForElement(i);
      }
    });

    // Calibrated axes submenu
    var axesSub = addSubmenu("Calibrated axes");
    addMenuItem("Show all", function () {
      for (var i = 0; i < constructs.length; i++) {
        if (!constructVisible[i]) {
          conCheckboxes[i].checked = true;
          conCheckboxes[i].dispatchEvent(new Event("change"));
        }
        constructLineVisible[i] = true;
      }
      buildCalibration();
      rebuildAllProjections();
    }, axesSub);
    addMenuItem("Hide all", function () {
      for (var i = 0; i < constructs.length; i++) {
        constructLineVisible[i] = false;
      }
      buildCalibration();
      rebuildAllProjections();
    }, axesSub);

    // Elements submenu
    var elemSub = addSubmenu("Elements");
    addMenuItem("Show all", function () {
      for (var i = 0; i < elements.length; i++) {
        if (!elemCheckboxes[i].checked) {
          elemCheckboxes[i].checked = true;
          elemCheckboxes[i].dispatchEvent(new Event("change"));
        }
      }
    }, elemSub);
    addMenuItem("Hide all", function () {
      for (var i = 0; i < elements.length; i++) {
        if (elemCheckboxes[i].checked) {
          elemCheckboxes[i].checked = false;
          elemCheckboxes[i].dispatchEvent(new Event("change"));
        }
      }
    }, elemSub);
    addMenuItem("Select all", function () {
      selectedElements = [];
      for (var i = 0; i < elements.length; i++) selectedElements.push(i);
      updateElementGlows();
      updateDynamicSortVisibility();
    }, elemSub);
    addMenuItem("Show all labels", function () {
      for (var i = 0; i < elements.length; i++) {
        elementLabelVisible[i] = true;
        elementObjects[i].label.visible = elementVisible[i];
      }
    }, elemSub);
    addMenuItem("Hide all labels", function () {
      for (var i = 0; i < elements.length; i++) {
        elementLabelVisible[i] = false;
        elementObjects[i].label.visible = false;
      }
    }, elemSub);

    // Constructs submenu
    var conSub = addSubmenu("Constructs");
    addMenuItem("Show all", function () {
      for (var i = 0; i < constructs.length; i++) {
        if (!conCheckboxes[i].checked) {
          conCheckboxes[i].checked = true;
          conCheckboxes[i].dispatchEvent(new Event("change"));
        }
      }
    }, conSub);
    addMenuItem("Hide all", function () {
      for (var i = 0; i < constructs.length; i++) {
        if (conCheckboxes[i].checked) {
          conCheckboxes[i].checked = false;
          conCheckboxes[i].dispatchEvent(new Event("change"));
        }
      }
    }, conSub);
    addMenuItem("Select all", function () {
      selectedConstructs = [];
      for (var i = 0; i < constructs.length; i++) selectedConstructs.push(i);
      updateConstructSelection();
    }, conSub);
    addMenuItem("Show all labels", function () {
      for (var i = 0; i < constructs.length; i++) {
        constructLabelVisible[i] = true;
      }
    }, conSub);
    addMenuItem("Hide all labels", function () {
      for (var i = 0; i < constructs.length; i++) {
        constructLabelVisible[i] = false;
      }
    }, conSub);
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
      selectedElements = [];
      selectedConstructs = [];
      selectedElementIndex = -1;
      restoreOriginalCoords();
      updateElementGlows();
      updateConstructSelection();
      updateDynamicSortVisibility();
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

  // --- Lasso (freeform) selection ---
  var lassoCanvas = document.createElement("canvas");
  lassoCanvas.style.cssText = "position:absolute;top:0;left:0;width:100%;height:100%;pointer-events:none;z-index:1000;display:none;";
  sceneContainer.appendChild(lassoCanvas);
  var lassoCtx = lassoCanvas.getContext("2d");

  var lassoActive = false;
  var lassoJustFinished = false;
  var lassoPoints = [];

  function getScreenPos(obj3d) {
    var v = new THREE.Vector3();
    v.setFromMatrixPosition(obj3d.matrixWorld);
    v.project(camera);
    var rect = renderer.domElement.getBoundingClientRect();
    return {
      x: (v.x * 0.5 + 0.5) * rect.width,
      y: (-v.y * 0.5 + 0.5) * rect.height
    };
  }

  function pointInPolygon(px, py, polygon) {
    var inside = false;
    for (var i = 0, j = polygon.length - 1; i < polygon.length; j = i++) {
      var xi = polygon[i].x, yi = polygon[i].y;
      var xj = polygon[j].x, yj = polygon[j].y;
      if (((yi > py) !== (yj > py)) && (px < (xj - xi) * (py - yi) / (yj - yi) + xi)) {
        inside = !inside;
      }
    }
    return inside;
  }

  function drawLasso() {
    var dpr = window.devicePixelRatio || 1;
    lassoCanvas.width = lassoCanvas.clientWidth * dpr;
    lassoCanvas.height = lassoCanvas.clientHeight * dpr;
    lassoCtx.setTransform(dpr, 0, 0, dpr, 0, 0);
    lassoCtx.clearRect(0, 0, lassoCanvas.clientWidth, lassoCanvas.clientHeight);
    if (lassoPoints.length < 2) return;
    lassoCtx.beginPath();
    lassoCtx.moveTo(lassoPoints[0].x, lassoPoints[0].y);
    for (var i = 1; i < lassoPoints.length; i++) {
      lassoCtx.lineTo(lassoPoints[i].x, lassoPoints[i].y);
    }
    lassoCtx.closePath();
    lassoCtx.fillStyle = "rgba(68,170,255,0.1)";
    lassoCtx.fill();
    lassoCtx.strokeStyle = "#44aaff";
    lassoCtx.lineWidth = 1.5;
    lassoCtx.setLineDash([5, 3]);
    lassoCtx.stroke();
  }

  renderer.domElement.addEventListener("mousedown", function (e) {
    if (e.shiftKey && e.button === 0) {
      lassoActive = true;
      controls.enabled = false;
      var rect = renderer.domElement.getBoundingClientRect();
      lassoPoints = [{ x: e.clientX - rect.left, y: e.clientY - rect.top }];
      lassoCanvas.style.display = "block";
      drawLasso();
      e.preventDefault();
    }
  }, false);

  renderer.domElement.addEventListener("mousemove", function (e) {
    if (!lassoActive) return;
    var rect = renderer.domElement.getBoundingClientRect();
    lassoPoints.push({ x: e.clientX - rect.left, y: e.clientY - rect.top });
    drawLasso();
  }, false);

  window.addEventListener("mouseup", function (e) {
    if (!lassoActive) return;
    lassoActive = false;
    controls.enabled = true;
    lassoCanvas.style.display = "none";

    // Need at least a small polygon
    if (lassoPoints.length < 5) return;

    lassoJustFinished = true;

    var newSelectedElements = [];
    var newSelectedConstructs = [];

    // Check elements
    for (var i = 0; i < elementObjects.length; i++) {
      if (!elementVisible[i]) continue;
      var sp = getScreenPos(elementObjects[i].sphere);
      if (pointInPolygon(sp.x, sp.y, lassoPoints)) {
        newSelectedElements.push(i);
      }
    }

    // Check constructs (either pole marker inside)
    for (var i = 0; i < constructObjects.length; i++) {
      if (!constructVisible[i]) continue;
      var rp = getScreenPos(constructObjects[i].rightMarker);
      var lp = getScreenPos(constructObjects[i].leftMarker);
      if (pointInPolygon(rp.x, rp.y, lassoPoints) || pointInPolygon(lp.x, lp.y, lassoPoints)) {
        newSelectedConstructs.push(i);
      }
    }

    // Apply selection: if we caught elements, select elements; if constructs, select constructs
    // If both, prefer whichever has more hits
    if (newSelectedElements.length > 0 || newSelectedConstructs.length > 0) {
      if (newSelectedElements.length >= newSelectedConstructs.length) {
        selectedElements = newSelectedElements;
        selectedConstructs = [];
        updateElementGlows();
        updateConstructSelection();
        updateDynamicSortVisibility();
      } else {
        selectedConstructs = newSelectedConstructs;
        selectedElements = [];
        updateConstructSelection();
        updateElementGlows();
        updateDynamicSortVisibility();
      }
    }
  }, false);

  // Disable orbit controls while Shift is held (must happen before OrbitControls captures mousedown)
  document.addEventListener("keydown", function (e) {
    if (e.key === "Shift") {
      controls.enabled = false;
      renderer.domElement.style.cursor = "crosshair";
    }
  });
  document.addEventListener("keyup", function (e) {
    if (e.key === "Shift" && !lassoActive) {
      controls.enabled = true;
      renderer.domElement.style.cursor = "default";
    }
  });

  renderer.domElement.addEventListener("click", onClick, false);
  renderer.domElement.addEventListener("mousemove", onMouseMove, false);
  renderer.domElement.addEventListener("dblclick", onDblClick, false);
  renderer.domElement.addEventListener("mouseleave", function () {
    tooltip.style.display = "none";
    unhighlightElement(hoveredElementIndex);
    unhighlightConstruct(hoveredConstructIndex);
    hoveredElementIndex = -1;
    hoveredConstructIndex = -1;
    hoveredFootConstructIndex = -1;
    hoveredFootElementIndex = -1;
    unhighlightGridCell();
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
    var asp = width / height;
    perspCamera.aspect = asp;
    perspCamera.updateProjectionMatrix();
    // Update ortho frustum on resize
    var halfH = orthoCamera.top;
    orthoCamera.left = -halfH * asp;
    orthoCamera.right = halfH * asp;
    orthoCamera.updateProjectionMatrix();
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
  var _prevCamPos = new THREE.Vector3();
  var _prevCamTarget = new THREE.Vector3();
  var _animId = null;
  var _calDirty = false;
  var _lastCalTime = 0;
  var _calThrottleMs = 80;

  function animate() {
    _animId = requestAnimationFrame(animate);
    controls.update();
    updateLabelVisibility();
    camera.getWorldDirection(wireUniforms.uCamDir.value);

    // Orient and scale silhouette ring to match sphere's visible edge
    if (silhouetteGroup.visible) {
      silhouetteGroup.quaternion.setFromRotationMatrix(camera.matrixWorld);
      if (isOrthographic) {
        silhouetteGroup.scale.setScalar(1);
      } else {
        var camDist = camera.position.length();
        var silR = camDist > 1 ? camDist / Math.sqrt(camDist * camDist - 1) : 1;
        silhouetteGroup.scale.setScalar(silR);
      }
    }

    // Smooth rotation animation
    if (_camAnim) {
      tickCameraAnimation(performance.now());
    }

    // Rebuild calibration on camera move (throttled, ticks face viewer)
    var camMoved = !camera.position.equals(_prevCamPos) ||
                   !controls.target.equals(_prevCamTarget);
    if (camMoved) {
      _prevCamPos.copy(camera.position);
      _prevCamTarget.copy(controls.target);
      _calDirty = true;
    }
    if (_calDirty) {
      var now = performance.now();
      if (now - _lastCalTime > _calThrottleMs) {
        _calDirty = false;
        _lastCalTime = now;
        buildCalibration();
        if (elevationFilterAngle < 90) {
          rebuildAllProjections();
        }
      }
    }

    renderer.render(scene, camera);
    labelRenderer.render(scene, camera);
    updateLabelAlignment();
    deconflictLabels();
  }

  // Pause rendering when tab is hidden to save CPU/GPU
  document.addEventListener("visibilitychange", function () {
    if (document.hidden) {
      if (_animId) {
        cancelAnimationFrame(_animId);
        _animId = null;
      }
    } else {
      if (!_animId) {
        _prevCamPos.set(NaN, NaN, NaN); // force recalc on resume
        animate();
      }
    }
  });

  animate();

})();
