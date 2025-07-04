generateDefaultSettings <- function() {
  l <- list()
  type <- list()

  # print grid to console
  l$show.trim <- 30
  type$show.trim <- "numeric"
  l$show.cut <- 20
  type$show.cut <- "numeric"
  l$show.scale <- TRUE
  type$show.scale <- "logical"
  l$show.meta <- TRUE
  type$show.meta <- "logical"
  l$e.no <- TRUE
  type$e.no <- "logical"
  l$c.no <- TRUE
  type$c.no <- "logical"
  l$preferred <- TRUE
  type$preferred <- "logical"

  class(l) <- "openrepgridSettings"
  attr(l, "type") <- type
  l
}


typecheck <- function(x, type) {
  f <- switch(type,
    logical = is.logical,
    numeric = is.numeric
  )
  f(x)
}


# x  object of class openrepgridSettings
checkSettingsIntegrity <- function(x, do.print = TRUE) {
  if (!methods::is(x, "openrepgridSettings")) {
    stop(
      "settings integrity check cannot be performed",
      "as objects class is not 'openrepgridSettings'"
    )
  }
  types <- attr(x, "type")
  np <- !mapply(typecheck, x, types) # not passed
  if (any(np)) {
    for (par.name in names(x[np])) {
      if (do.print) {
        cat("Parameter '", par.name, "' must be ",
          types[[par.name]], "\n",
          sep = ""
        )
      }
    }
    stop("error in definition of parameters")
  } else {
    return(TRUE)
  }
}


setDefaultSettings <- function() {
  .OpenRepGridEnv$settings <- generateDefaultSettings()
}


#' global settings for OpenRepGrid
#'
#' @param ...       Use parameter value pairs (`par1=val1, par2=val2`) to
#'                  change a parameter. Use parameter names to request
#'                  parameter's value (`"par1", "par2"`).
#' @note   Currently the following parameters can be changed, ordered by topic.
#' The default value is shown in the brackets at the end of a line.
#'
#'  - `show.scale`: Show grid scale info? (`TRUE`)
#'  - `show.meta`: Show grid meta data? (`TRUE`)
#'  - `show.trim`: Number of chars to trim strings to (`30`)
#'  - `show.cut`: Maximum number of characters printed on the sides of a grid (`20`)
#'  - `e.no`: Print element ID number? (`TRUE`)
#'  - `c.no`: Print construct ID number? (`TRUE`)
#'  - `preferred`: Print preferred pole indicator? (`TRUE`)
#'
#' @export
#' @examples \dontrun{
#' # get current settings
#' settings()
#'
#' # get some parameters
#' settings("show.scale", "show.meta")
#'
#' # change parameters
#' bell2010
#'
#' settings(show.meta = F)
#' bell2010
#'
#' settings(show.scale = F, show.cut = 30)
#' bell2010
#' }
#'
settings <- function(...) {
  parnames <- names(generateDefaultSettings())
  cur.settings <- .OpenRepGridEnv$settings
  args <- list(...)
  if (length(args) == 0) { # get all arguments
    return(cur.settings)
  }
  # get args
  if (is.null(names(args)) &
    all(unlist(lapply(args, is.character)))) {
    pm <- pmatch(unlist(args), parnames)
    # if (length(pm) == 1L)
    #  return(cur.settings[pm][[1L]])
    return(cur.settings[na.omit(pm)])
  } else { # set arguments
    names(args) <- parnames[pmatch(names(args), parnames)] # partial matching of names
    new.settings <- modifyList(cur.settings, args) # modify settings list
    passed <- checkSettingsIntegrity(new.settings)
    if (passed) {
      .OpenRepGridEnv$settings <- new.settings # replace settings
      invisible(new.settings)
    }
  }
}


#' subset method for openrepgridSettings class
#' @export
#' @rdname openrepgridSettings
#' @method [ openrepgridSettings
#' @keywords internal
#'
`[.openrepgridSettings` <- function(x, i, ...) {
  types <- attr(x, "type")
  x <- unclass(x)
  x <- x[i]
  attr(x, "type") <- types
  class(x) <- "openrepgridSettings"
  x
}


#' Print method for openrepgridSettings class
#' @export
#' @rdname openrepgridSettings
#' @method print openrepgridSettings
#' @keywords internal
#'
print.openrepgridSettings <- function(x, ...) {
  cat("------------------------\n")
  cat("Settings for OpenRepGrid\n")
  cat("------------------------\n")

  cat("\nPrinting a grid to the console\n")
  if (!is.null(x$show.scale)) cat("\tshow.scale :", x$show.scale, "(show grid scale info?)\n")
  if (!is.null(x$show.meta)) cat("\tshow.meta  :", x$show.meta, "(show grid meta data?)\n")
  if (!is.null(x$show.trim)) cat("\tshow.trim  :", x$show.trim, "(number of chars to trim strings to)\n")
  if (!is.null(x$show.cut)) cat("\tshow.cut   :", x$show.cut, "(max no of chars on the sides of a grid)\n")
  if (!is.null(x$e.no)) cat("\te.no       :", x$e.no, "(print element id?)\n")
  if (!is.null(x$c.no)) cat("\tc.no       :", x$c.no, "(print construct id?)\n")
  if (!is.null(x$preferred)) cat("\tpreferred  :", x$c.no, "(print preferred pole indicator?)\n")
}


#' Save OpenRepGrid settings
#'
#' The current settings of OpenRepGrid can be saved into a file with
#' the extension `.orgset`.
#'
#' @param file    Path of the file to be saved to.
#' @export
settingsSave <- function(file) {
  # TODO: check for orgset extension?
  saveRDS(.OpenRepGridEnv$settings, file = file)
}


#' Load OpenRepGrid settings
#'
#' OpenRepGrid settings saved in an a settings file with
#' the extension `.orgset` can be loaded to restore the
#' settings.
#'
#' @param file    Path of the file to be loaded.
#' @export
settingsLoad <- function(file) {
  orgset <- readRDS(file)
  if (!methods::is(orgset, "openrepgridSettings")) {
    stop("file", file, "is no valid OpenRepGrid settings file")
  }
  .OpenRepGridEnv$settings <- orgset # save in environment in namespace
}
