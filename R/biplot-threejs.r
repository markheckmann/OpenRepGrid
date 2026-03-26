# ///////////////////////////////////////////////////////////////
#
#   THREE.JS BASED INTERACTIVE 3D BIPLOT
#
# ///////////////////////////////////////////////////////////////


#' Interactive 3D biplot using three.js
#'
#' Opens an interactive 3D biplot in the browser using three.js. The
#' visualization supports orbit/zoom/pan controls, toggling of elements,
#' constructs, axes, and projection lines, as well as hover tooltips showing
#' representation quality.
#'
#' @param x A `repgrid` object.
#' @param dim Dimensions (principal components) to display (default `1:3`).
#' @param center Centering of the grid data. `0` = no centering, `1` = row mean
#'   (default), `2` = column mean, `3` = double centering, `4` = midpoint
#'   centering.
#' @param normalize Normalization. `0` = none (default), `1` = normalize rows,
#'   `2` = normalize columns.
#' @param g Power of the singular values assigned to the construct coordinates
#'   (default `0`).
#' @param h Power of the singular values assigned to the element coordinates
#'   (default `1 - g`).
#' @param col.active Columns (elements) actively used in the SVD. By default
#'   all elements are active.
#' @param col.passive Supplementary columns projected into the biplot.
#' @param unity Logical. Scale coordinates so that the longest element and
#'   construct vectors have unit length (default `TRUE`).
#' @param scale.e Scaling factor for element coordinates (default `0.9`).
#' @param file Path for the output HTML file. If `NULL` (default), a temporary
#'   file is created.
#' @param launch Logical. Open the HTML file in the default browser (default
#'   `TRUE`).
#' @param ... Additional arguments passed to [calcBiplotCoords()].
#' @return The file path of the generated HTML file (invisibly).
#'
#' @seealso [biplot2d()], [biplot3d()], [biplotSlaterThreejs()], [biplotEsaThreejs()]
#'
#' @examples
#' \dontrun{
#' biplotThreejs(boeker)
#' biplotThreejs(boeker, dim = c(1, 2, 3), g = 1, h = 1)
#' }
#' @export
biplotThreejs <- function(x, dim = 1:3,
                          center = 1, normalize = 0,
                          g = 0, h = 1 - g,
                          col.active = NA, col.passive = NA,
                          unity = TRUE, scale.e = 0.9,
                          file = NULL, launch = TRUE, ...) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required for biplotThreejs(). Please install it.", call. = FALSE)
  }

  # compute biplot coordinates via SVD
  x <- calcBiplotCoords(x, g = g, h = h,
                         col.active = col.active,
                         col.passive = col.passive,
                         center = center, normalize = normalize, ...)
  E <- x@calcs$biplot$element.coords
  C <- x@calcs$biplot$construct.coords
  D <- x@calcs$biplot$D

  # scale to unity in 3D
  if (unity) {
    max.e <- max(apply(E[, dim]^2, 1, sum)^.5)
    max.c <- max(apply(C[, dim]^2, 1, sum)^.5)
    se <- 1 / max.e * scale.e
    sc <- 1 / max.c
  } else {
    se <- 1
    sc <- 1
  }
  Eu <- E * se
  Cu <- C * sc

  # variance explained
  var.explained <- D^2 / sum(D^2)

  # representation quality (cos^2)
  E_w <- sweep(E, 2, D^(1 - h), "*")
  C_w <- sweep(C, 2, D^(1 - g), "*")

  e_ssq_sel <- rowSums(E_w[, dim, drop = FALSE]^2)
  e_ssq_total <- rowSums(E_w^2)
  e_quality <- ifelse(e_ssq_total > 0, e_ssq_sel / e_ssq_total, 0)

  c_ssq_sel <- rowSums(C_w[, dim, drop = FALSE]^2)
  c_ssq_total <- rowSums(C_w^2)
  c_quality <- ifelse(c_ssq_total > 0, c_ssq_sel / c_ssq_total, 0)

  # build data for JSON
  elements_list <- data.frame(
    name = elements(x),
    x = Eu[, dim[1]],
    y = Eu[, dim[2]],
    z = Eu[, dim[3]],
    quality = round(e_quality, 4),
    stringsAsFactors = FALSE
  )
  constructs_list <- data.frame(
    right_pole = constructs(x)$rightpole,
    left_pole = constructs(x)$leftpole,
    x = Cu[, dim[1]],
    y = Cu[, dim[2]],
    z = Cu[, dim[3]],
    quality = round(c_quality, 4),
    preferred = preferredPoles(x),
    stringsAsFactors = FALSE
  )
  meta_list <- list(
    dim = dim,
    var_explained = round(var.explained, 6),
    var_explained_selected = round(var.explained[dim], 6),
    n_elements = ncol(x),
    n_constructs = nrow(x),
    scale_min = x@scale$min,
    scale_max = x@scale$max
  )

  # raw ratings matrix for grid table display (constructs x elements)
  ratings_mat <- unname(x@ratings[, , 1])
  ratings_list <- list(
    values = ratings_mat,
    element_names = elements(x),
    left_poles = constructs(x)$leftpole,
    right_poles = constructs(x)$rightpole
  )

  # calibration data: centering offsets and unscaled construct coords
  dat <- x@ratings[, , 1]
  nc <- nrow(dat)
  if (center == 0) {
    offsets <- rep(0, nc)
  } else if (center == 1) {
    offsets <- rowMeans(dat, na.rm = TRUE)
  } else if (center == 2) {
    offsets <- rep(0, nc)
  } else if (center == 3) {
    offsets <- rowMeans(dat, na.rm = TRUE)
  } else if (center == 4) {
    offsets <- rep(getScaleMidpoint(x), nc)
  }

  calibration_list <- list(
    offsets = round(offsets, 6),
    construct_coords = round(unname(C[, dim]), 6),
    se = round(se, 6)
  )

  json_str <- jsonlite::toJSON(
    list(elements = elements_list, constructs = constructs_list,
         meta = meta_list, ratings = ratings_list,
         calibration = calibration_list),
    auto_unbox = TRUE, digits = 6, dataframe = "rows"
  )

  # assemble HTML
  template_dir <- system.file("threejs", package = "OpenRepGrid")
  html_template <- paste(readLines(file.path(template_dir, "biplot3d-template.html"),
                                    warn = FALSE), collapse = "\n")
  threejs_lib <- paste(readLines(file.path(template_dir, "three.min.js"),
                                  warn = FALSE), collapse = "\n")
  orbit_lib <- paste(readLines(file.path(template_dir, "OrbitControls.js"),
                                warn = FALSE), collapse = "\n")
  css2d_lib <- paste(readLines(file.path(template_dir, "CSS2DRenderer.js"),
                                warn = FALSE), collapse = "\n")
  app_js <- paste(readLines(file.path(template_dir, "biplot3d.js"),
                             warn = FALSE), collapse = "\n")
  app_css <- paste(readLines(file.path(template_dir, "biplot3d.css"),
                              warn = FALSE), collapse = "\n")

  html <- html_template
  html <- sub("{{APP_CSS}}", app_css, html, fixed = TRUE)
  html <- sub("{{THREEJS_LIB}}", threejs_lib, html, fixed = TRUE)
  html <- sub("{{ORBIT_LIB}}", orbit_lib, html, fixed = TRUE)
  html <- sub("{{CSS2D_LIB}}", css2d_lib, html, fixed = TRUE)
  html <- sub("{{BIPLOT_DATA}}", json_str, html, fixed = TRUE)
  html <- sub("{{APP_JS}}", app_js, html, fixed = TRUE)

  # write and open
  if (is.null(file)) {
    file <- tempfile(fileext = ".html")
  }
  writeLines(html, file)

  if (launch) {
    browseURL(file)
  }
  invisible(file)
}


#' Interactive 3D Slater biplot using three.js
#'
#' Wrapper around [biplotThreejs()] with default settings for Slater's INGRID
#' biplot (`g = 1`, `h = 1`, `center = 1`).
#'
#' @inheritParams biplotThreejs
#' @return The file path of the generated HTML file (invisibly).
#'
#' @seealso [biplotThreejs()], [biplotSlater3d()]
#'
#' @examples
#' \dontrun{
#' biplotSlaterThreejs(boeker)
#' }
#' @export
biplotSlaterThreejs <- function(x, center = 1, g = 1, h = 1, ...) {
  biplotThreejs(x = x, center = center, g = g, h = h, ...)
}


#' Interactive 3D eigenstructure analysis biplot using three.js
#'
#' Wrapper around [biplotThreejs()] with default settings for eigenstructure
#' analysis biplot (`g = 1`, `h = 1`, `center = 4`).
#'
#' @inheritParams biplotThreejs
#' @return The file path of the generated HTML file (invisibly).
#'
#' @seealso [biplotThreejs()], [biplotEsa3d()]
#'
#' @examples
#' \dontrun{
#' biplotEsaThreejs(boeker)
#' }
#' @export
biplotEsaThreejs <- function(x, center = 4, g = 1, h = 1, ...) {
  biplotThreejs(x = x, center = center, g = g, h = h, ...)
}
