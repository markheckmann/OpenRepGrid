# //////////////////////////////////////////////////////////////////////////////
#                                                                             #
#  			  			            GRID INDEX MEASURES     							            #
#                                                                             #
#       All function for index measures start with a lower case "i"           #
#       e.g. 'iBias' that stand for index followed by the an acronym          #
#       of what is calculated.                                                #
#                                                                             #
# //////////////////////////////////////////////////////////////////////////////

# ___________________ ----
# //////////////////////////////////////////////////////////////////////////////
#   						                   HELPERS  							                 ----
# //////////////////////////////////////////////////////////////////////////////


#' Print a square matrix in well readable format
#'
#' Helper function to produce output for square matrices, e.g. distance matrices etc. Adds a (trimmed) name and index
#' column. Displays upper triangle only by default. The function is used inside the print method of several classes.
#'
#' @param x A square `matrix`.
#' @param names names A vector of row names.
#' @param width Column width of output (numeric).
#' @param trim Trimmed length of names (numeric).
#' @param index Whether to add an index columns (default `TRUE`).
#' @param upper Whether to only show the upper triangle (default `TRUE`).
#'
#' @export
#' @keywords internal
print_square_matrix <- function(x, names = NA, trim = NA,
                                index = TRUE, width = NA, upper = TRUE) {
  if (!is.matrix(x)) {
    stop("'x' muste be a matrix", call. = FALSE)
  }
  if (dim(x)[1] != dim(x)[2]) {
    stop("'x' muste be a square matrix", call. = FALSE)
  }
  if (!is.null(names) && !is.na(names[1]) && length(names) != nrow(x)) {
    stop("length of 'names' must be equal to number of rows of 'x'", call. = FALSE)
  }
  if (!is.na(trim) && !trim >= 0) {
    stop("'trim' must be NA or >= 0", call. = FALSE)
  }

  # set width of columns
  if (is.na(width)) {
    width <- max(
      nchar(nrow(x)), # length of column index
      nchar(max(x, na.rm = T)) # biggest value
    )
  }

  # trim names
  if (!is.null(names) && !is.na(names[1]) &&
    !is.null(trim) && !is.na(trim[1])) {
    names <- substr(names, 1, trim)
  }

  # fill lower triangle with blanks of correct length
  if (upper) {
    x[lower.tri(x, diag = TRUE)] <- paste0(rep(" ", width), collapse = "")
  }

  # add index column for neater colnames
  if (index) {
    x <- addIndexColumnToMatrix(x)
  } else {
    colnames(x) <- seq_len(ncol(x))
  }

  # add names column in first position
  if (!is.na(names)[1]) {
    cnms <- c(" ", colnames(x))
    x <- cbind(names, x)
    colnames(x) <- cnms
  }

  x <- as.data.frame(x, stringsAsFactors = FALSE)
  rownames(x) <- NULL
  print(x)
}


#' Number of matches in ratings
#'
#' Count the number of matches, i.e. (near) identical ratings between two elements or constructs. Matches are used as
#' the basis for the calculation of grid indexes.
#'
#' @param x A `repgrid` object.
#' @param deviation Maximal difference between ratings to be considered a match (default `0` = only identical rating
#'   scores are a match). Especially useful for long rating scale (e.g. 0 to 100).
#' @param diag.na Whether to set the diagonal of the matrices to `NA` (default is `TRUE`).
#' @return A list of class `org.matches` with:
#'
#'  * `grid`: The grid used to calculate the matches.
#'  * `deviation` The deviation parameter.
#'  * `max_constructs` Maximum possible number of matches across constructs.
#'  * `max_elements` Maximum possible number of matches across elements.
#'  * `total_constructs` Total number of matches across constructs.
#'  * `total_elements` Total number of matches across elements.
#'  * `constructs`: Matrix with no. of matches for constructs.
#'  * `elements`: Matrix with no. of matches for elements.
#'
#' @keywords internal
#' @export
#' @example inst/examples/example-matches.R
#'
matches <- function(x, deviation = 0, diag.na = TRUE) {
  stop_if_not_is_repgrid(x)
  if (!is.numeric(deviation) || deviation < 0) {
    stop("'deviation' must be >= 0", call. = FALSE)
  }
  R <- ratings(x, names = FALSE)

  # constructs
  M_c <- apply(t(R), 2, function(x) {
    colSums(abs(x - t(R)) <= deviation) # matches per column
  })
  # elements
  M_e <- apply(R, 2, function(x) {
    colSums(abs(x - R) <= deviation) # matches per column
  })

  # set diagonal NA
  if (diag.na) {
    diag(M_c) <- NA
    diag(M_e) <- NA
  }

  # total number of C/E matches
  total_constructs <- sum(M_c[upper.tri(M_c)])
  total_elements <- sum(M_e[upper.tri(M_e)])

  # maximal number of possible matches
  nc <- nrow(x)
  ne <- ncol(x)
  max_constructs <- unname(ne * nc * (nc - 1) / 2)
  max_elements <- unname(nc * ne * (ne - 1) / 2)

  l <- list(
    grid = x,
    deviation = deviation,
    total_constructs = total_constructs,
    total_elements = total_elements,
    max_constructs = max_constructs,
    max_elements = max_elements,
    constructs = unname(M_c),
    elements = unname(M_e)
  )
  class(l) <- c("org.matches", class(l))
  l
}


#' Print method for class org.matches.
#'
#' @param x Object of class `org.matches`.
#' @param output    String with each letter indicating which parts of the output to print (default is `"ICE"`, order
#'   does not matter): `I` = Information, `C` = Constructs' matches, `E` = Elements' matches.
#' @param names names A vector of row names.
#' @param width Column width of output (numeric).
#' @param trim Trimmed length of names (default = `50`).
#' @param index Whether to add an index columns (default `TRUE`).
#' @param upper Whether to only show the upper triangle (default `TRUE`).
#' @export
#' @keywords internal
print.org.matches <- function(x, output = "ICE", index = TRUE,
                              names = TRUE, trim = 50, upper = TRUE, width = NA, ...) {
  l <- x # renamed from 'l' to 'x' to match arg in print generic
  output <- toupper(output)
  g <- l$grid
  if (names) {
    cnames <- constructs(g, collapse = TRUE)
    enames <- elements(g)
  } else {
    cnames <- NA
    enames <- NA
  }

  # matrices with no of matches
  M_c <- l$constructs
  M_e <- l$elements

  cat("\n##############")
  cat("\nRATING MATCHES")
  cat("\n##############\n")

  ## I = Info
  if (str_detect(output, "I")) {
    cat("\nMaximal rating difference to count as match: ", l$deviation)
    cat("\n")
    cat("\nTotal no. of matches between constructs: ", l$total_constructs)
    cat("\nMaximum possible no. of matches between constructs: ", l$max_constructs)
    cat("\n")
    cat("\nTotal no. of matches between elements: ", l$total_elements)
    cat("\nMaximum possible no. of matches between elements: ", l$max_elements)
    cat("\n")
  }
  ## E = Elements
  if (str_detect(output, "E")) {
    cat(bold("\nELEMENTS\n\n"))
    print_square_matrix(M_e,
      names = enames, index = index,
      upper = upper, trim = trim, width = width
    )
  }
  ## C = Constructs
  if (str_detect(output, "C")) {
    cat(bold("\nCONSTRUCTS\n\n"))
    print_square_matrix(M_c,
      names = cnames, index = index,
      upper = upper, trim = trim, width = width
    )
  }
}


# ___________________ ----
# //////////////////////////////////////////////////////////////////////////////
#   						            SLATER MEASURES  							                 ----
# //////////////////////////////////////////////////////////////////////////////


#' Calculate 'bias' of grid as defined by Slater (1977).
#'
#' "Bias records a tendency for responses to accumulate at one end of the grading scale" (Slater, 1977, p.88).
#'
#' @param  x `repgrid` object.
#' @param min,max Minimum and maximum grid scale values. Nor needed if they are set for the grid.
#' @param digits Numeric. Number of digits to round to (default is `2`).
#' @return Numeric.
#' @references  Slater, P. (1977). *The measurement of intrapersonal space by Grid technique*. London: Wiley.
#' @note STATUS: Working and checked against example in Slater, 1977, p. 87.
#' @export
#' @seealso [indexVariability()]
#' @examples
#' indexBias(boeker)
#'
indexBias <- function(x, min = NULL, max = NULL, digits = 2) {
  dat <- getRatingLayer(x)
  sc <- getScale(x)
  if (is.null(min)) {
    min <- sc[1]
  }
  if (is.null(max)) {
    max <- sc[2]
  }
  p <- min + (max - min) / 2 # scale midpoint
  q <- max - p # distance to scale limits
  n <- nrow(dat) # number of rows (constructs)
  row.means <- apply(dat, 1, mean, na.rm = TRUE) # means of construct rows
  bias <- (sum((row.means - p)^2) / n)^.5 / q # calculation of bias
  round(bias, digits)
}


#' Calculate 'variability' of a grid as defined by Slater (1977).
#'
#' Variability records a tendency for the responses to gravitate towards both end of the gradings scale. (Slater, 1977,
#' p.88).
#'
#' @param  x `repgrid` object.
#' @param min,max Minimum and maximum grid scale values. Nor needed if they are set for the grid.
#' @param digits Numeric. Number of digits to round to (default is `2`).
#' @return  Numeric.
#' @references Slater, P. (1977). *The measurement of intrapersonal space by Grid technique*. London: Wiley.
#' @note STATUS: working and checked against example in Slater, 1977 , p.88.
#' @export
#' @seealso [indexBias()]
#' @examples
#' indexVariability(boeker)
#'
indexVariability <- function(x, min = NULL, max = NULL, digits = 2) {
  dat <- getRatingLayer(x)
  sc <- getScale(x)
  if (is.null(min)) {
    min <- sc[1]
  }
  if (is.null(max)) {
    max <- sc[2]
  }

  D <- as.matrix(center(x)) # row centered grid matrix
  W <- D %*% t(D) # co-variation Matrix W
  V <- diag(W) # extract trace (construct variations)
  V.tot <- sum(V, na.rm = TRUE) # total variation

  p <- min + (max - min) / 2 # scale midpoint
  q <- max - p # distance to scale limits
  n <- nrow(D) # number of rows (constructs)
  m <- ncol(D) # number of columns (elements)
  res <- (V.tot / (n * (m - 1)))^.5 / q # calculate variability
  round(res, digits)
}


# . ----
# ___________________ ----
# //////////////////////////////////////////////////////////////////////////////
#   						                    MISC             							         ----
# //////////////////////////////////////////////////////////////////////////////


#' Percentage of Variance Accounted for by the First Factor (PVAFF)
#'
#' The PVAFF is used as a measure of cognitive complexity. It was introduced in an unpublished PhD thesis by Jones
#' (1954, cit. Bonarius, 1965). To calculate it, the 'first factor' two different methods may be used. One applies
#' principal component analysis (PCA) to the construct centered raw data (default), the second applies SVD to the
#' construct correlation matrix. The PVAFF reflects the amount of variation that is accounted for by a single linear
#' component. If a single latent component is able to explain the variation in the grid, the cognitive complexity is
#' said to be low. In this case the construct system is regarded as 'simple' (Bell, 2003).
#'
#' @param x `repgrid` object.
#' @param method Method to compute PVAFF: `1` = PCA is applied to raw data with centered constructs (default), `2` =
#'   SVD of construct correlation matrix.
#' @references   Bell, R. C. (2003). An evaluation of indices used to represent construct structure. In G. Chiari & M.
#'   L. Nuzzo (Eds.), *Psychological Constructivism and the Social World* (pp. 297-305). Milan: FrancoAngeli.
#'
#'   Bonarius, J. C. J. (1965). Research in the personal construct theory of George A. Kelly: role construct repertory
#'   test and basic theory. In B. A. Maher (Ed.),
#'                  *Progress in experimental personality research*
#'   (Vol. 2). New York: Academic Press.
#'
#'   James, R. E. (1954). *Identification in terms of personal constructs* (Unpublished doctoral thesis). Ohio State
#'   University, Columbus, OH.
#'
#' @export
#' @examples
#'
#' indexPvaff(bell2010)
#'
indexPvaff <- function(x, method = 1) {
  message(
    "Note: As of v0.1.14 PVAFF is derived using PCA of the construct centered ratings by default.",
    "Before that the construct correlation matrix was used (see method=2).\n\n"
  )
  if (!inherits(x, "repgrid")) {
    stop("Object must be of class 'repgrid'")
  }

  if (method == 1) {
    r <- ratings(x)
    p <- stats::prcomp(t(r), center = TRUE, scale. = FALSE)
    pvaff <- (p$sdev^2 / sum(p$sdev^2))[1]
  } else if (method == 2) {
    cr <- constructCor(x)
    sv <- svd(cr)$d
    pvaff <- sv[1]^2 / sum(sv^2)
  } else {
    stop("'method' must be 1 or 2.", call. = FALSE)
  }

  return(pvaff)
}


# Print method for class indexPvaff.
#
# @param x         Object of class indexPvaff.
# @param digits    Numeric. Number of digits to round to (default is
#                  `2`).
# @param ...       Not evaluated.
# @export
# @method          print indexPvaff
# @keywords        internal
#
# print.indexPvaff <- function(x, digits=2, ...)
# {
#   cat("\n########################################################")
#   cat("\nPercentage of Variance Accounted for by the First Factor")
#   cat("\n########################################################")
#   cat("\n\nPVAFF: ", round(x*100, digits), "%")
# }



#' Bieri's index of cognitive complexity
#'
#' The index builds on the number of rating matches between pairs of constructs. It is the relation between the total
#' number of matches and the possible number of matches.
#'
#' **CAVEAT**: The Bieri index will change when constructs are reversed.
#'
#' @param x A `repgrid` object.
#' @param deviation Maximal difference between ratings to be considered a match (default `0` = identical scores for a
#'   match).
#' @return List of class `indexBieri`:
#'
#'  * `grid`: The grid used to calculate the index
#'  * `deviation` The deviation parameter.
#'  * `matches_max` Maximum possible number of matches across constructs.
#'  * `matches` Total number of matches across constructs.
#'  * `constructs`: Matrix with no. of matches for constructs.
#'  * `bieri`: Bieri index (= matches / matches_max)
#'
#' @example inst/examples/example-indexBieri.R
#' @export
#'
#'
indexBieri <- function(x, deviation = 0) {
  stop_if_not_is_repgrid(x)

  m <- matches(x, deviation = deviation)
  n_matches <- m$total_constructs
  n_matches_max <- m$max_constructs

  l <- list(
    grid = x,
    deviation = deviation,
    constructs = m$constructs,
    matches = n_matches,
    matches_max = n_matches_max,
    bieri = n_matches / n_matches_max
  )
  class(l) <- c("indexBieri", class(l))
  l
}


#' Print method for class indexBieri
#'
#' @param x Object of class `indexBieri`.
#' @param output  String with each letter indicating which parts of the output to print (default is `"IC"`, order does
#'   not matter): `I` = Information, `C` = Matrix of matches.
#' @param digits Number of digits to display.
#' @export
#' @keywords internal
#'
print.indexBieri <- function(x, output = "I", digits = 3, ...) {
  cat("\n######################")
  cat("\nBIERI COMPLEXITY INDEX")
  cat("\n######################\n")

  M_c <- x$constructs
  cnames <- constructs(x$grid, collapse = T)
  index <- TRUE
  upper <- TRUE
  width <- NA
  trim <- 50

  ## I = Information
  if (str_detect(output, "I")) {
    cat("\nBieri:", round(x$bieri, digits))
    cat("\n")
    cat("\nMaximal rating difference to count as match: ", x$deviation)
    cat("\nTotal no. of matches between constructs: ", x$matches)
    cat("\nMaximum possible no. of matches between constructs: ", x$matches_max)
    cat("\n")
  }
  ## C = Constructs
  if (str_detect(output, "C")) {
    cat(bold("\nMATCHES BETWEEN CONSTRUCTS\n\n"))
    print_square_matrix(M_c,
      names = cnames, index = index,
      upper = upper, trim = trim, width = width
    )
  }
}


#' Dilemmatic constructs
#'
#' A Dilemmatic Construct (DC) is one where the ideal element is rated on the scale midpoint. This means, the person
#' cannot decide which of the poles is preferable. Such constructs are called "dilemmatic". For example, on a rating
#' scale from 1 to 7, a rating of 4 on the ideal element means that the construct is dilemmatic. By definition, DCs can
#' only emerge in scales with an uneven number of rating options, i.e. 5-point scale, 7-point scale etc. However, the
#' function makes it possible to allow for a deviation from the midpoint, to still count as dilemmatic. This is useful
#' if the grid uses a large rating scale, e.g. from 0 to 100 or a visual analog scale, as some grid administration
#' programs do. In this case you may want to set ratings, for example, between 45 and 55 as close enough to the
#' midpoint to indicate that both poles are equally desirable.
#'
#' @param x A `repgrid` object.
#' @param ideal Index of ideal element.
#' @param deviation The maximal deviation from the scale midpoint for an ideal rating to be considered dilemmatic
#'   (default = `0`). For scales larger than a 17-point rating scale a warning is raised, if deviation is `0` (see
#'   details).
#' @param warn Show warnings?
#' @return List of class `indexDilemmatic`:
#'
#'  * `ideal`: Name of the ideal element.
#'  * `n_constructs` Number of grid's constructs.
#'  * `scale`: Minimum and maximum of grid rating scale.
#'  * `midpoint`: Midpoint of rating scale.
#'  * `lower,upper`: Lower and upper value to for a rating to be considered in the midpoint range.
#'  * `midpoint_range`: Midpoint range as interval.
#'  * `n_dilemmatic`: Number of dilemmatic constructs.
#'  * `perc_dilemmatic`: Percentage of constructs which are dilemmatic.
#'  * `i_dilemmatic`: Index of dilemmatic constructs.
#'  * `dilemmatic_constructs`: Labels of dilemmatic constructs.
#'  * `summary`: Summary dataframe.
#'
#' @example inst/examples/example-indexDilemmatic.R
#' @export
#'
indexDilemmatic <- function(x, ideal, deviation = 0, warn = TRUE) {
  if (!is.repgrid(x)) {
    stop("'x' must be 'repgrid' object", call. = FALSE)
  }
  ne <- ncol(x)
  if (ideal < 1 || ideal > ncol(x)) {
    stop("'ideal' must be in the range from 1 to ", ne, call. = FALSE)
  }

  # warn if uneven number of rating options
  n_options <- diff(getScale(x)) + 1
  if (n_options %% 2 == 0 && warn) {
    warning("The rating scale has an even number of options (", n_options, "). ",
      "Dilemmatic constructs usually require an uneven rating scale length (see details)",
      call. = FALSE
    )
  }
  # warn if uneven number of rating options
  if (n_options >= 16 && deviation == 0 && warn) {
    warning("The rating scale is quite long (", n_options, "). ",
      "You may want to consider allowing deviations from the midpoint (see details)",
      call. = FALSE
    )
  }

  cnames <- constructs(x, collapse = TRUE)
  R <- ratings(x)
  r_ideal <- R[, ideal]
  m <- midpoint(x)
  sc <- getScale(x)
  lower <- m - deviation
  upper <- m + deviation
  i_low <- r_ideal >= lower
  i_high <- r_ideal <= upper
  i_dilemmatic <- i_low & i_high
  n_dilemmatic <- sum(i_dilemmatic)
  midpoint_range <- paste0("[", upper, ", ", lower, "]")

  df_dilemmatic <- data.frame(
    Construct = constructs(x, collapse = TRUE),
    Ideal = unname(r_ideal),
    MidpointRange = midpoint_range,
    Dilemmatic = i_dilemmatic
  )
  rownames(df_dilemmatic) <- NULL

  l <- list(
    ideal = elements(x)[ideal],
    n_constructs = nrow(x),
    scale = sc,
    midpoint = m,
    deviation = deviation,
    lower = lower,
    upper = upper,
    midpoint_range = midpoint_range,
    n_dilemmatic = n_dilemmatic,
    perc_dilemmatic = n_dilemmatic / nrow(x),
    i_dilemmatic = which(i_dilemmatic),
    dilemmatic_constructs = cnames[i_dilemmatic],
    summary = df_dilemmatic
  )
  class(l) <- c("indexDilemmatic", class(l))
  l
}


#' Print method for class indexDilemmatic
#'
#' @param x  Object of class `indexDilemmatic`
#' @param output String with each letter indicating which parts of the output to print (default is `"SD"`, order
#'   does not matter): `S` = Summary, `D` = Details (dilemmatic constructs).
#' @export
#' @method print indexDilemmatic
#' @keywords internal
print.indexDilemmatic <- function(x, output = "SD", ...) {
  output <- toupper(output)

  cat("\n#####################")
  cat("\nDilemmatic Constructs")
  cat("\n#####################\n")

  ## I = Info
  if (str_detect(output, "S")) {
    cat(bold("\nSUMMARY\n"))
    cat("\nGrid rating scale:", x$scale["min"], "(left pole) to", x$scale["max"], "(right pole)")
    cat("\nScale midpoint:", x$midpoint)
    cat("\nIdeal element:", x$ideal)
    cat("\nDilemmatic: Constructs with ideal ratings in the interval", x$midpoint_range)
    cat("\n")
    cat("\nNo. of dilemmatic constructs:", x$n_dilemmatic)
    cat("\nPercent dilemmatic constructs: ", scales::percent(x$perc_dilemmatic, .1),
      " (", x$n_dilemmatic, "/", x$n_constructs, ")",
      sep = ""
    )
  }

  ## C = Constructs
  if (str_detect(output, "D")) {
    cat("\n")
    cat(bold("\nDETAILS\n\n"))
    if (x$n_dilemmatic > 0) {
      x$summary %>%
        dplyr::filter(Dilemmatic) %>%
        print()
    } else {
      cat("  No dilemmatic constructs found.\n")
    }
  }
}


#' Calculate intensity index.
#'
#' The Intensity index has been suggested by Bannister (1960) as a measure of the amount of construct linkage.
#' Bannister suggested that the score reflects the degree of organization of the construct system under investigation
#' (Bannister & Mair, 1968). The index resulted from his and his colleagues work on construction systems of patient
#' suffering schizophrenic thought disorder. The concept of intensity has a theoretical connection to the notion of
#' "tight" and "loose" construing as proposed by Kelly (1991). While tight constructs lead to unvarying prediction,
#' loose constructs allow for varying predictions. Bannister hypothesized that schizophrenic thought disorder is liked
#' to a process of extremely loose construing leading to a loss of predictive power of the subject's construct system.
#' The Intensity score as a structural measure is thought to reflect this type of system disintegration (Bannister,
#' 1960).
#'
#' Implementation as in the Gridcor program and explained on the correspoding help pages: "\ldots the sum of the
#' squared values of the correlations of each construct with the rest of the constructs, averaged by the total number
#' of constructs minus one. This process is repeated with each element, and the overall Intensity is calculated by
#' averaging the intensity scores of constructs and elements." (Gridcor manual). Currently
#' the total is calculated as the unweighted average of all single scores (for elements and construct).
#'
#' @title Intensity index
#' @section Development: TODO: Results have not been tested against other programs' results.
#'
#' @param x A `repgrid` object.
#' @param rc Whether to use Cohen's rc for the calculation of inter-element correlations. See [elementCor()] for
#'   further explanations of this measure.
#' @param trim The number of characters a construct is trimmed to (default is `30`). If `NA` no trimming occurs.
#'   Trimming simply saves space when displaying correlation of constructs or elements with long names.
#' @return An object of class `indexIntensity` containing a list with the following elements: \cr
#'
#'   `c.int`: Intensity scores by construct. `e.int`: Intensity scores by element. `c.int.mean`: Average intensity
#'   score for constructs. `e.int.mean`: Average intensity score for elements. `total.int`: Total intensity score.
#'
#' @export
#' @references Bannister, D. (1960). Conceptual structure in thought-disordered schizophrenics. *The Journal of mental
#' science*, 106, 1230-49.
#' @examples
#'
#' indexIntensity(bell2010)
#' indexIntensity(bell2010, trim = NA)
#'
#' # using Cohen's rc for element correlations
#' indexIntensity(bell2010, rc = TRUE)
#'
#' # save output
#' x <- indexIntensity(bell2010)
#' x
#'
#' # printing options
#' print(x, digits = 4)
#'
#' # accessing the objects' content
#' x$c.int
#' x$e.int
#' x$c.int.mean
#' x$e.int.mean
#' x$total.int
#'
indexIntensity <- function(x, rc = FALSE, trim = 30) {
  if (!is.repgrid(x)) {
    stop("'x' must be 'repgrid' object", call. = FALSE)
  }

  cr <- constructCor(x, trim = trim)
  nc <- getNoOfConstructs(x)
  diag(cr) <- 0 # out zeros in diagonal (won't have an effect)
  c.int <- apply(cr^2, 2, function(x) sum(x) / (nc - 1)) # sum of squared correlations / nc -1

  er <- elementCor(x, rc = rc, trim = trim)
  ne <- getNoOfElements(x)
  diag(er) <- 0 # out zeros in diagonal (won't have an effect)
  e.int <- apply(er^2, 2, function(x) sum(x) / (ne - 1)) # sum of squared correlations / (ne - 1)

  c.int.mean <- mean(c.int, na.rm = TRUE) # mean of construct intensity scores
  e.int.mean <- mean(e.int, na.rm = TRUE) # mean of element intensity scores

  total.int <- mean(c(c.int, e.int, na.rm = TRUE))

  res <- list(
    c.int = c.int,
    e.int = e.int,
    c.int.mean = c.int.mean,
    e.int.mean = e.int.mean,
    total.int = total.int
  )
  class(res) <- "indexIntensity"
  res
}


#' Print method for class indexIntensity.
#'
#' @param x Object of class indexIntensity.
#' @param digits Numeric. Number of digits to round to (default is `2`).
#' @param output String with each letter indicating which parts of the output to print (default is `"TCE"`, order
#'   does not matter): `T` = Total Intensity, `C` = Constructs' intensities, `E` = Elements' intensities.
#' @export
#' @method print indexIntensity
#' @keywords internal
#'
print.indexIntensity <- function(x, digits = 2, output = "TCE", ...) {
  output <- toupper(output)

  cat("\n################")
  cat("\nIntensity index")
  cat("\n################")

  ## T = Total
  if (str_detect(output, "T")) {
    cat("\n\nTotal intensity:", round(x$total.int, digits), "\n")
  }

  ## C = Constructs
  if (str_detect(output, "C")) {
    cat("\n\nAverage intensity of constructs:", round(x$c.int.mean, digits), "\n")
    cat("\nItensity by construct:\n")
    df.c.int <- data.frame(intensity = x$c.int)
    rownames(df.c.int) <- paste(seq_along(x$c.int), names(x$c.int))
    print(round(df.c.int, digits))
  }

  ## E = Elements
  if (str_detect(output, "E")) {
    cat("\n\nAverage intensity of elements:", round(x$e.int.mean, digits), "\n")
    cat("\nItensity by element:\n")
    df.e.int <- data.frame(intensity = x$e.int)
    rownames(df.e.int) <- paste(seq_along(x$e.int), names(x$e.int))
    print(round(df.e.int, digits))
  }
}


#' Polarization (percentage of extreme ratings)
#'
#' Polarization is the percentage of extreme ratings, e.g. the values 1 and 7 for a grid with a 7-point ratings scale.
#'
#' @param x A `repgrid` object.
#' @param deviation The maximal deviation from the end of the rating scale for values to be considered an 'extreme'
#'   rating. By default only values that lie directly on ends of the ratings scales are considered 'extreme' (default =
#'   `0`).
#' @return List of class `indexPolarization`:
#'
#'  * `scale`: Minimum and maximum of grid rating scale.
#'  * `lower,upper` Lower and upper value to decide which ratings are considered extreme.
#'  * `polarization_total`: Grid's overall polarization.
#'  * `polarization_constructs`: Polarization per construct.
#'  * `polarization_elements`: Polarization per element.
#'
#' @example inst/examples/example-indexPolarization.R
#' @export
#'
indexPolarization <- function(x, deviation = 0) {
  if (!is.repgrid(x)) {
    stop("'x' must be 'repgrid' object", call. = FALSE)
  }

  R <- ratings(x)
  sc <- getScale(x)
  lower <- sc["min"] + deviation
  upper <- sc["max"] - deviation
  i_low <- R <= lower
  i_high <- R >= upper
  ii <- i_low | i_high
  K <- R
  K[, ] <- ii # indicator matrix 0/1
  R[!ii] <- NA

  l <- list(
    scale = sc,
    lower = lower,
    upper = upper,
    polarization_total = data.frame(
      Ratings = prod(dim(x)),
      Extreme = sum(K),
      Polarization = mean(K)
    ),
    polarization_constructs = data.frame(
      Construct = constructs(x, collapse = TRUE),
      Ratings = unname(ncol(x)),
      Extremes = unname(rowSums(K)),
      Polarization = unname(rowMeans(K))
    ),
    polarization_elements = data.frame(
      Element = elements(x),
      Ratings = unname(nrow(x)),
      Extremes = unname(colSums(K)),
      Polarization = unname(colMeans(K))
    )
  )
  class(l) <- c("indexPolarization", class(l))
  l
}


#' Print method for class indexPolarization.
#'
#' @param x Object of class indexPolarization.
#' @param output String with each letter indicating which parts of the output to print (default is `"ITCE"`, order
#'   does not matter): `I` = Information, `T` = Total Intensity, `C` = Constructs' intensities, `E` = Elements'
#'   intensities.
#' @export
#' @method print indexPolarization
#' @keywords internal
#'
print.indexPolarization <- function(x, output = "ITCE", ...) {
  output <- toupper(output)

  cat("\n##################")
  cat("\nPolarization index")
  cat("\n##################\n")

  ## I = Info
  if (str_detect(output, "I")) {
    cat(
      "\nThe grid is rated on a scale from",
      x$scale["min"], "(left pole) to", x$scale["max"], "(right pole)"
    )
    cat("\nExtreme ratings are ratings <=", x$lower, "or >=", x$upper)
  }

  ## T = Total
  if (str_detect(output, "T")) {
    cat("\n\n")
    cat(bold("\nPOLARIZATION OVERALL\n\n"))
    x$polarization_total %>%
      dplyr::mutate(
        Polarization = scales::percent(Polarization, .1)
      ) %>%
      print()
  }

  ## C = Constructs
  if (str_detect(output, "C")) {
    cat("\n")
    cat(bold("\nPOLARIZATION BY CONSTRUCT\n\n"))
    x$polarization_constructs %>%
      dplyr::mutate(
        Polarization = scales::percent(Polarization, .1)
      ) %>%
      print()
  }

  ## E = Elements
  if (str_detect(output, "E")) {
    cat("\n")
    cat(bold("\nPOLARIZATION BY ELEMENT\n\n"))
    x$polarization_elements %>%
      dplyr::mutate(
        Polarization = scales::percent(Polarization, .1)
      ) %>%
      print()
  }
}


#' Self construction profile
#'
#' TBD
#'
#' @param x A `repgrid` object.
#' @param self Numeric. Index of self element.
#' @param ideal Numeric. Index of ideal element.
#' @param others Numeric. Index(es) of self related "other" elements (e.g. father, friend).
#' @param method The distance or correlation measure:
#'   * Distances:  `euclidean`, `manhattan`, `maximum`,  `canberra`, `binary`, `minkowski`
#'   * Correlations: `pearson`, `kendall`, `spearman`
#' @param p The power of the Minkowski distance, in case `minkowski` is used as argument for `method`, otherwise it is
#'   ignored.
#' @param round   Round average rating scores for 'others' to closest integer?
#' @param normalize Normalize values?
#' @return  List object of class `indexSelfConstruction`, containing the results from the calculations:
#'
#'  * `grid`: Reduced grid with self, ideal and others
#'  * `method_type`: method type (correlation or distance)
#'  * `method`: correlation or distance method used
#'  * `self_element`: name of the self element
#'  * `ideal_element`: name of the ideal element
#'  * `other_elements`: name(s) of other elements
#'  * `self_ideal`: measure between self and ideal
#'  * `self_others`: measure between self and others
#'  * `ideal_others`: measure betwen ideal and others
#'
#' @references TBD
#'
#' @export
#' @example inst/examples/example-indexSelfConstruction.R
#'
indexSelfConstruction <- function(x, self, ideal, others = c(-self, -ideal),
                                  method = "euclidean", p = 2, normalize = TRUE,
                                  round = FALSE) {
  # sanity/arg checks
  if (!is.repgrid(x)) {
    stop("'x' must be a repgrid object", call. = FALSE)
  }
  nc <- ncol(x)
  if (!is.numeric(self) || !length(self) == 1 || !(self >= 1 && self <= nc)) {
    stop("'self' must be ONE numeric value in the range from 1 to ", nc, call. = FALSE)
  }
  if (!is.numeric(ideal) || !length(ideal) == 1 || !(ideal >= 1 && ideal <= nc)) {
    stop("'ideal' must be ONE numeric value in the range from 1 to ", nc, call. = FALSE)
  }
  if (!is.numeric(others) || !length(others) >= 1) {
    stop("'others' must be a numeric vector with at least one entry", call. = FALSE)
  }
  if (!all(abs(others) >= 1) && all(abs(others) <= nc)) {
    stop("indexes indicating 'others' must range between 1 and ", nc, call. = FALSE)
  }
  if (any(others < 0) && any(others > 0)) {
    stop("It is not allowed to mix positive and negative indexes", call. = FALSE)
  }
  if (sum(duplicated(abs(others))) > 0) {
    stop("duplicated indexes ore not allowed in 'others'", call. = FALSE)
  }

  # treat negative indexes
  if (all(others < 0)) {
    others <- setdiff(1L:nc, abs(others))
  }

  # warnings for potentially wring input
  if (self %in% others) {
    warning("'self' is also contained in 'others'", call. = FALSE)
  }
  if (ideal %in% others) {
    warning("'ideal' is also contained in 'others'", call. = FALSE)
  }

  # build a new 'others' element as average rating of all others elements
  digits <- ifelse(round, 0, Inf)
  x <- addAvgElement(x, name = "others", i = others, digits = digits)
  i_others <- ncol(x)

  # Select method and get measures
  distances <- c("euclidean", "manhattan", "maximum", "canberra", "binary", "minkowski")
  correlations <- c("pearson", "kendall", "spearman")
  choices <- c(distances, correlations)
  method <- match.arg(method, choices)
  method_type <- NA
  if (method %in% distances) {
    method_type <- "distance"
    S <- distance(x, along = 2, dmethod = method, p = p, normalize = normalize)
  }
  if (method %in% c("pearson", "kendall", "spearman")) {
    method_type <- "correlation"
    S <- elementCor(x, rc = TRUE, method = method)
  }

  # extract relevant measures
  s_self_ideal <- S[self, ideal]
  s_self_others <- S[self, i_others]
  s_ideal_others <- S[ideal, i_others]

  enames <- elements(x)

  # return indexSelfConstruction object
  l <- list(
    grid = x[, c(self, ideal, i_others)],
    method_type = method_type,
    method = method,
    normalize = normalize,
    round = round,
    self_element = enames[self],
    ideal_element = enames[ideal],
    other_elements = enames[others],
    self_ideal = s_self_ideal,
    self_others = s_self_others,
    ideal_others = s_ideal_others
  )
  class(l) <- c("indexSelfConstruction", class(l))
  l
}


#' Print method for indexSelfConstruction
#' @export
#' @keywords internal
#'
print.indexSelfConstruction <- function(x, digits = 2, ...) {
  w <- options()$width
  l <- x
  cat("=================")
  cat("\nSELF CONSTRUCTION\n")
  cat("=================\n")
  cat("\n  Mean ratings for 'others' rounded to closest integer: ", l$round)
  cat("\n\nMEASURE\n")
  normalized <- ifelse(l$normalize, "normalized", "")
  cat("\n ", normalized, l$method, l$method_type)
  if (l$method_type == "correlation") {
    cat(crayon::blue(
      strwrap("Note: All correlations use Cohen's rc version which is invariant to construct reflections",
        indent = 2, prefix = "\n", exdent = 8
      )
    ))
  }
  cat("\n")
  cat(
    "\nCOMPARISONS\n",
    "\n  * Self - Ideal: ", round(l$self_ideal, digits),
    "\n  * Self - Others: ", round(l$self_others, digits),
    "\n  * Ideal - Others: ", round(l$ideal_others, digits)
  )
  cat("\n")
  cat("\nELEMENTS\n")
  cat("\n  * self:", l$self_element)
  cat("\n  * ideal:", l$ideal_element)
  cat("\n  * others:", strwrap(paste(l$other_elements, collapse = ", "), width = w - 12, exdent = 12, prefix = "\n", initial = ""))
  cat("\n")
}


# # Alternative using cli package. I do not like the look
# print.indexSelfConstruction <- function(x, digits = 2, ...)
# {
#   w <- options()$width
#   l <- x
#   cli_h1("COGNITIVE PROFILE")
#
#   cli_h3("MEASURE")
#   cat_line()
#   cat_line("  ", l$method, " ", l$method_type)
#   if (l$method_type == "correlation") {
#     cat(crayon::blue(
#       strwrap("Note: All correlations use Cohen's rc version which is invariant to construct reflections",
#               indent = 2, prefix = "\n", exdent = 8)))
#   }
#   cli_h3("COMPARISONS")
#   cat_line()
#   comp <-
#     c(paste("Self - Ideal: ", round(l$self_ideal, digits)),
#       paste("Self - Others: ", round(l$self_others, digits)),
#       paste("Ideal - Others: ", round(l$ideal_others, digits))
#     )
#   cat_bullet(comp)
#   cli_h3("ELEMENTS")
#   cat_line()
#   elems <-
#     c(paste("self: ", l$self_element),
#       paste("ideal: ", l$ideal_element),
#       paste("others:", strwrap(paste(l$other_elements, collapse = ", "), width = w - 12, exdent = 12, prefix = "\n", initial = ""), collapse = " ")
#     )
#
#   cat_bullet(elems)
# }


# . ----------
# ___________________ ----
# //////////////////////////////////////////////////////////////////////////////
#      					            CONFLICT MEASURES       							         ----
# //////////////////////////////////////////////////////////////////////////////


# __ Misc ----------------------------------

#' Print function for class indexConflict1
#'
#' @param x         Object of class indexConflict1.
#' @param digits    Numeric. Number of digits to round to (default is `1`).
#' @param ...       Not evaluated.
#' @export
#' @method          print indexConflict1
#' @keywords        internal
#'
print.indexConflict1 <- function(x, digits = 1, ...) {
  cat("\n################################")
  cat("\nConflicts based on correlations")
  cat("\n################################")
  cat("\n\nAs devised by Slade & Sheehan (1979)")

  cat("\n\nTotal number of triads:", x$total)
  cat("\nNumber of imbalanced triads:", x$imbalanced)

  cat(
    "\n\nProportion of balanced triads:",
    round(x$prop.balanced * 100, digits = digits), "%"
  )
  cat(
    "\nProportion of imbalanced triads:",
    round(x$prop.imbalanced * 100, digits = digits), "%"
  )
}


#' Conflict measure as proposed by Slade and Sheehan (1979)
#'
#' The first approach to mathematically derive a conflict measure based on grid data was presented by Slade and Sheehan
#' (1979). Their operationalization is based on an approach by Lauterbach (1975) who applied the *balance theory*
#' (Heider, 1958) for a quantitative assessment of psychological conflict. It is based on a count of balanced and
#' imbalanced triads of construct correlations. A triad is imbalanced if one or all three of the correlations are
#' negative, i. e. leading to contrary implications. This approach was shown by Winter (1982) to be flawed. An improved
#' version was proposed by Bassler et al. (1992) and has been implemented in the function `indexConflict2`.
#'
#' The table below shows when a triad made up of the constructs A, B, and C is balanced and imbalanced:
#'
#' | cor(A,B) | cor(A,C) | cor(B,C) | Triad characteristic |
#' |----------|----------|----------|----------------------|
#' | +        | +        | +        | balanced             |
#' | +        | +        | -        | imbalanced           |
#' | +        | -        | +        | imbalanced           |
#' | +        | -        | -        | balanced             |
#' | -        | +        | +        | imbalanced           |
#' | -        | +        | -        | balanced             |
#' | -        | -        | +        | balanced             |
#' | -        | -        | -        | imbalanced           |
#'
#' @title Conflict measure for grids (Slade & Sheehan, 1979) based on correlations.
#' @param x       `repgrid` object.
#' @return  A list with the following elements:
#'
#' - `total`: Total number of triads
#' - `imbalanced`: Number of imbalanced triads
#' - `prop.balanced`: Proportion of balanced triads
#' - `prop.imbalanced`: Proportion of imbalanced triads
#'
#' @references Bassler, M., Krauthauser, H., & Hoffmann, S. O. (1992). A new approach to the identification of
#' cognitive conflicts in the repertory grid: An illustrative case study.
#' *Journal of Constructivist Psychology, 5*(1), 95-111.
#'
#' Heider, F. (1958). *The Psychology of Interpersonal Relation*. John Wiley & Sons.
#'
#' Lauterbach, W. (1975). Assessing psychological conflict.
#' *The British Journal of Social and Clinical Psychology, 14*(1), 43-47.
#'
#' Slade, P. D., & Sheehan, M. J. (1979). The measurement of 'conflict' in repertory grids. *British Journal of
#' Psychology, 70*(4), 519-524.
#'
#' Winter, D. A. (1982). Construct relationships, psychological disorder and therapeutic change. *The British Journal
#' of Medical Psychology, 55* (Pt 3), 257-269.
#'
#' @export
#' @seealso [indexConflict2()] for an improved version of this measure; see [indexConflict3()] for a measure based on distances.
#' @examples
#'
#' indexConflict1(feixas2004)
#' indexConflict1(boeker)
#'
indexConflict1 <- function(x) {
  if (!inherits(x, "repgrid")) {
    stop("Object must be of class 'repgrid'")
  }

  r <- constructCor(x) # construct correlation matrix
  z <- fisherz(r)
  nc <- getNoOfConstructs(x) # number of constructs
  comb <- t(combn(nc, 3)) # all possible correlation triads
  balanced <- rep(NA, nrow(comb)) # set up result vector

  for (i in 1:nrow(comb)) {
    z.triad <- z[t(combn(comb[i, ], 2))] # correlations of triad
    z.prod <- prod(z.triad)
    if (sign(z.prod) > 0) { # triad is imbalanced if product of correlations is negative
      balanced[i] <- TRUE
    } else {
      balanced[i] <- FALSE
    }
  }
  prop.balanced <- sum(balanced) / length(balanced) # proportion of
  prop.imbalanced <- 1 - prop.balanced # proportion of

  res <- list(
    total = length(balanced),
    imbalanced = sum(!balanced),
    prop.balanced = prop.balanced,
    prop.imbalanced = prop.imbalanced
  )
  class(res) <- "indexConflict1"
  res
}


#' Conflict measure for grids (Bassler et al., 1992) based on correlations.
#'
#' The function calculates the conflict measure as devised by Bassler et al. (1992). It is an improved version of the
#' ideas by Slade and Sheehan (1979) that have been implemented in the function [indexConflict1()]. The new approach
#' also takes into account the magnitude of the correlations in a trait to assess whether it is balanced or imbalanced.
#' As a result, small correlations that are psychologically meaningless are considered accordingly. Also, correlations
#' with a  small magnitude, i. e. near zero, which may  be positive or negative due to chance alone will no longer
#' distort the measure (Bassler et al., 1992).
#'
#' Description of the balance / imbalance assessment:
#'
#' 1. Order correlations of the triad by absolute magnitude, so that \eqn{ r_{max} > r_{mdn} > r_{min}}, \eqn{r_{max} > r_{mdn} > r_{min}}.
#' 2. Apply Fisher's Z-transformation and division by 3 to yield values between 1 and -1  (\eqn{ Z_{max} > Z_{mdn} > Z_{min}, Z_{max} > Z_{mdn} > Z_{min}}).
#' 3. Check whether the triad is balanced by assessing if the following relation holds:
#'
#'    - If \eqn{Z_{max} Z_{mdn} > 0, Z_{max} x Z_{mdn} > 0},
#'      the triad is balanced if \eqn{Z_{max} Z_{mdn} - Z_{min} <= crit},
#'      \eqn{Z_{max} x Z_{mdn} - Z_{min} <= crit}.
#'    - If \eqn{Z_{max} Z_{mdn} < 0,  Z_{max} x Z_{mdn} < 0},
#'      the triad is balanced if \eqn{Z_{min} - Z_{max} Z_{mdn} <= crit},
#'      \eqn{Z_{min} - Z_{max} x Z_{mdn} <= crit}.
#'
#' @section Personal remarks (MH): I am a bit suspicious about step 2 from above. To devide by 3 appears pretty arbitrary.
#'        The r for a z-values of 3 is 0.9950548 and not 1.
#'        The r for 4 is 0.9993293. Hence, why not a value of 4, 5, or 6?
#'        Denoting the value to devide by with `a`, the relation for the
#'        first case translates into \eqn{a  Z_{max}  Z_{mdn} <= \frac{crit}{a} + Z_{min}},
#'        \eqn{a x Z_{max} x Z_{mdn} =< crit/a + Z_{min}}. This shows that a bigger value of `a`
#'        will make it more improbable that the relation will hold.
#'
#' @param x A `repgrid` object.
#' @param crit Sensitivity criterion with which triads are marked as unbalanced. A bigger values will lead to less
#'   imbalanced triads. The default is `0.03`. The value should be adjusted with regard to the researchers interest.
#'
#' @references Bassler, M., Krauthauser, H., & Hoffmann, S. O. (1992). A new approach to the identification of
#' cognitive conflicts in the repertory grid: An illustrative case study.
#'  *Journal of Constructivist Psychology, 5*(1), 95-111.
#'
#' Slade, P. D., & Sheehan, M. J. (1979). The measurement of 'conflict' in repertory grids. *British Journal of
#' Psychology, 70*(4), 519-524.
#'
#' @seealso  See [indexConflict1()] for the older version of this measure; see [indexConflict3()] for a measure based
#'   on distances instead of correlations.
#' @examples
#'
#' indexConflict2(bell2010)
#'
#' x <- indexConflict2(bell2010)
#' print(x)
#'
#' # show conflictive triads
#' print(x, output = 2)
#'
#' # accessing the calculations for further use
#' x$total
#' x$imbalanced
#' x$prop.balanced
#' x$prop.imbalanced
#' x$triads.imbalanced
#'
#' @export
indexConflict2 <- function(x, crit = .03) {
  if (!inherits(x, "repgrid")) {
    stop("Object must be of class 'repgrid'")
  }

  r <- constructCor(x) # construct correlation matrix
  z <- fisherz(r)
  nc <- getNoOfConstructs(x) # number of constructs
  comb <- t(combn(nc, 3)) # all possible correlation triads
  balanced <- rep(NA, nrow(comb)) # set up result vector

  for (i in 1:nrow(comb)) {
    z.triad <- z[t(combn(comb[i, ], 2))] # z-values of triad
    ind <- order(abs(z.triad), decreasing = TRUE) # order for absolute magnitude
    z.triad <- z.triad[ind] # reorder z values by magnitude
    z.12 <- prod(z.triad[1:2]) # product of two biggest z values
    z.3 <- z.triad[3] # minimal absolute z value
    # select case for inequality relation assessment
    if (sign(z.12) > 0) {
      balanced[i] <- z.12 - z.3 <= crit
    } else {
      balanced[i] <- z.3 - z.12 <= crit
    }
  }
  prop.balanced <- sum(balanced) / length(balanced) # proportion of
  prop.imbalanced <- 1 - prop.balanced # proportion of

  res <- list(
    total = length(balanced),
    imbalanced = sum(!balanced),
    prop.balanced = prop.balanced,
    prop.imbalanced = prop.imbalanced,
    triads.imbalanced = comb[!balanced, ]
  )
  class(res) <- "indexConflict2"
  res
}


indexConflict2Out1 <- function(x, digits = 1) {
  cat("\n###############################")
  cat("\nConflicts based on correlations")
  cat("\n###############################")
  cat("\n\nAs devised by Bassler et al. (1992)")

  cat("\n\nTotal number of triads:", x$total)
  cat("\nNumber of imbalanced triads:", x$imbalanced)
  cat(
    "\n\nProportion of balanced triads:",
    round(x$prop.balanced * 100, digits = digits), "%"
  )
  cat(
    "\nProportion of imbalanced triads:",
    round(x$prop.imbalanced * 100, digits = digits), "%\n"
  )
}


indexConflict2Out2 <- function(x) {
  cat("\nConstructs that form imbalanced triads:\n")
  df <- as.data.frame(x$triads.imbalanced)
  colnames(df) <- c(" ", "  ", "   ")
  print(df)
}


#' Print method for class indexConflict2
#'
#' @param x A `repgrid` object.
#' @param digits  Numeric. Number of digits to round to (default is `1`).
#' @param output  Numeric. The output printed to the console. `output=1` (default) will print information about the
#'   conflicts to the console. `output = 2` will additionally print the conflictive triads.
#' @param ... Not evaluated.
#' @export
#' @method print indexConflict2
#' @keywords internal
#'
print.indexConflict2 <- function(x, digits = 1, output = 1, ...) {
  indexConflict2Out1(x, digits = digits)
  if (output == 2) {
    indexConflict2Out2(x)
  }
}


#' Conflict or inconsistency measure for grids (Bell, 2004) based on distances.
#'
#' Measure of conflict or inconsistency as proposed by Bell (2004). The
#' identification of conflict is based on distances rather than correlations as
#' in other measures of conflict [indexConflict1()] and
#' [indexConflict2()]. It assesses if the distances between all
#' components of a triad, made up of one element and two constructs, satisfies
#' the "triangle inequality" (cf. Bell, 2004). If not, a triad is regarded as
#' conflictive. An advantage of the measure is that it can be interpreted not
#' only as a global measure for a grid but also on an element, construct, and
#' element by construct level making it valuable for detailed feedback. Also,
#' differences in conflict can be submitted to statistical testing procedures.
#'
#' Status:  working; output for euclidean and manhattan distance
#'          checked against Gridstat output. \cr
#' TODO:    standardization and z-test for discrepancies;
#'          Index of Conflict Variation.
#'
#' @param x             `repgrid` object.
#' @param p             The power of the Minkowski distance. `p=2` (default) will result
#'                      in euclidean distances, `p=1` in city block
#'                      distances.
#' @param e.out         Numeric. A vector giving the indexes of the elements
#'                      for which detailed stats (number of conflicts per element,
#'                      discrepancies for triangles etc.) are prompted
#'                      (default `NA`, i.e. no detailed stats for any element).
#' @param e.threshold   Numeric. Detailed stats are prompted for those elements with a an
#'                      attributable percentage to the overall conflicts
#'                      higher than the supplied threshold
#'                      (default `NA`).
#' @param c.out         Numeric. A vector giving the indexes of the constructs
#'                      for which detailed stats (discrepancies for triangles etc.)
#'                      are prompted (default `NA`, i. e. no detailed stats).
#' @param c.threshold   Numeric. Detailed stats are prompted for those constructs with a an
#'                      attributable percentage to the overall conflicts
#'                      higher than the supplied threshold
#'                      (default `NA`).
#' @param trim          The number of characters a construct (element) is trimmed to (default is
#'                      `10`). If `NA` no trimming is done. Trimming
#'                      simply saves space when displaying the output.
#'
#' @return  A list (invisibly) containing:
#'
#' - `potential`: number of potential conflicts
#' - `actual`: count of actual conflicts
#' - `overall`: percentage of conflictive relations
#' - `e.count`: number of involvements of each element in conflictive relations
#' - `e.perc`: percentage of involvement of each element in total of conflictive relations
#' - `c.count`: number of involvements of each construct in conflictive relation
#' - `c.perc`: percentage of involvement of each construct in total of conflictive relations
#' - `e.stats`: detailed statistics for prompted elements
#' - `c.stats`: detailed statistics for prompted constructs
#' - `e.threshold`: threshold percentage. Used by print method
#' - `c.threshold`: threshold percentage. Used by print method
#' - `enames`: trimmed element names. Used by print method
#' - `cnames`: trimmed construct names. Used by print method
#'
#' @references    Bell, R. C. (2004). A new approach to measuring inconsistency
#'                or conflict in grids. *Personal Construct Theory & Practice*, (1), 53-59.
#'
#' @section output: For further control over the output see [print.indexConflict3()].
#' @export
#' @seealso    See [indexConflict1()] and [indexConflict2()] for conflict measures based on triads of correlations.
#' @examples
#' # calculate conflicts
#' indexConflict3(bell2010)
#'
#' # show additional stats for elements 1 to 3
#' indexConflict3(bell2010, e.out = 1:3)
#'
#' # show additional stats for constructs 1 and 5
#' indexConflict3(bell2010, c.out = c(1, 5))
#'
#' # finetune output
#' ## change number of digits
#' x <- indexConflict3(bell2010)
#' print(x, digits = 4)
#'
#' ## omit discrepancy matrices for constructs
#' x <- indexConflict3(bell2010, c.out = 5:6)
#' print(x, discrepancies = FALSE)
#'
indexConflict3 <- function(x, p = 2,
                           e.out = NA,
                           e.threshold = NA,
                           c.out = NA,
                           c.threshold = NA,
                           trim = 20) {
  # To assess the triangle inequality we need:
  #
  # - d.ij   'distance'  between element i and constuct j
  # - d.ik   'distance'  between element i and constuct k
  # - d.jk   distance between the constructs j and k
  #
  # Let the distance between element i and a construct j (i.e. d.ij)
  # be the rating of element i on construct j.
  # The distance between the constructs it the distance (euclidean or city block)
  # between them without taking into account the element under consideration.

  s <- getRatingLayer(x) # grid scores matrix
  ne <- getNoOfElements(x)
  nc <- getNoOfConstructs(x)
  enames <- getElementNames2(x, index = T, trim = trim, pre = "", post = " ")
  cnames <- getConstructNames2(x, index = T, trim = trim, mode = 1, pre = "", post = " ")

  # set up result vectors
  # confict.disc      discrepancy for each triangle (indexed e, c1, c2)
  # confict.e         number of conflicts for each element
  # conflict.c        number of conflicts for each construct
  # conflict.total    overall value of conflictive triangles
  conflict.disc <- array(NA, dim = c(nc, nc, ne))
  conflict.e <- rep(0, ne)
  conflict.c <- rep(0, nc)
  conflict.total <- 0
  conflicts.potential <- ne * nc * (nc - 1) / 2
  # e is i, c1 is j and c2 is k in Bell's Fortran code

  for (e in seq_len(ne)) {
    # average distance between constructs c1 and c2 not taking into account
    # the element under consideration. Generalization for any minkwoski metric
    dc <- dist(s[, -e], method = "minkowski", p = p) / (ne - 1)^(1 / p) # Bell averages the unsquared distances (euclidean),
    dc <- as.matrix(dc) # convert dist object to matrix             # i.e. divide euclidean dist by root of n or p in the general case

    for (c1 in seq_len(nc)) {
      for (c2 in seq_len(nc)) {
        if (c1 < c2) {
          d.jk <- dc[c1, c2]
          d.ij <- s[c1, e]
          d.ik <- s[c2, e]

          # assess if triangle inequality fails., i.e. if one distance is bigger
          # than the sum of the other two distances. The magnitude it is bigger
          # is recorded in disc (discrepancy)
          if (d.ij > (d.ik + d.jk)) {
            disc <- d.ij - (d.ik + d.jk)
          } else if (d.ik > (d.ij + d.jk)) {
            disc <- d.ik - (d.ij + d.jk)
          } else if (d.jk > (d.ij + d.ik)) {
            disc <- d.jk - (d.ij + d.ik)
          } else {
            disc <- NA
          }

          # store size of discrepancy in confict.disc and record discrepancy
          # by element (confict.e) construct (confict.c) and overall (confict.total)
          if (!is.na(disc)) {
            conflict.disc[c1, c2, e] <- disc
            conflict.disc[c2, c1, e] <- disc
            conflict.e[e] <- conflict.e[e] + 1
            conflict.c[c1] <- conflict.c[c1] + 1
            conflict.c[c2] <- conflict.c[c2] + 1
            conflict.total <- conflict.total + 1
          }
        }
      }
    }
  }

  # add e and c names to results
  dimnames(conflict.disc)[[3]] <- enames
  conflict.e.df <- data.frame(percentage = conflict.e)
  rownames(conflict.e.df) <- enames
  conflict.c.df <- data.frame(percentage = conflict.c)
  rownames(conflict.c.df) <- cnames


  ### Detailed stats for elements ###

  conflictAttributedByConstructForElement <- function(e) {
    e.disc.0 <- e.disc.na <- conflict.disc[, , e] # version with NAs and zeros for no discrepancies
    e.disc.0[is.na(e.disc.0)] <- 0 # replace NAs by zeros

    e.disc.no <- apply(!is.na(e.disc.na), 2, sum) # number of conflicts per construct
    e.disc.perc <- e.disc.no / sum(e.disc.no) * 100 # no conf. per as percentage
    e.disc.perc.df <- data.frame(percentage = e.disc.perc) # convert to dataframe
    rownames(e.disc.perc.df) <- cnames # add rownames

    n.conflict.pairs <- sum(e.disc.no) / 2 # number of conflicting construct pairs all elements
    disc.avg <- mean(e.disc.0) # average level of discrepancy
    disc.sd <- sd(as.vector(e.disc.na), na.rm = TRUE) # sd of discrepancies

    disc.stand <- (e.disc.na - disc.avg) / disc.sd # standardized discrepancy

    list(
      e = e,
      disc = e.disc.na,
      pairs = n.conflict.pairs,
      constructs = e.disc.perc.df,
      avg = disc.avg,
      sd = disc.sd
    ) # ,
    # disc.stand=round(disc.stand, digits))
  }


  ### Detailed stats for constructs ###

  conflictAttributedByElementForConstruct <- function(c1) {
    c1.disc.0 <- c1.disc.na <- conflict.disc[c1, , ] # version with NAs and zeros for no discrepancies
    rownames(c1.disc.na) <- paste("c", seq_len(nrow(c1.disc.na)))
    colnames(c1.disc.na) <- paste("e", seq_len(ncol(c1.disc.na)))

    c1.disc.0[is.na(c1.disc.0)] <- 0 # replace NAs by zeros

    disc.avg <- mean(c1.disc.0) # average level of discrepancy
    disc.sd <- sd(as.vector(c1.disc.na), na.rm = TRUE) # sd of discrepancies
    list(
      c1 = c1,
      disc = c1.disc.na,
      avg = disc.avg,
      sd = disc.sd
    ) # ,
    # disc.stand=round(disc.stand, digits))
  }

  # Select which detailed stats for elements. Either all bigger than
  # a threshold or the ones selected manually.
  if (!is.na(e.out[1])) {
    e.select <- e.out
  } else if (!is.na(e.threshold[1])) {
    e.select <- which(conflict.e / conflict.total * 100 > e.threshold)
  } else {
    e.select <- NA
  }

  e.stats <- list() # list with detailed results
  if (!is.na(e.select[1])) {
    for (e in seq_along(e.select)) {
      e.stats[[e]] <- conflictAttributedByConstructForElement(e.select[e])
    }
    names(e.stats) <- enames[e.select]
  }

  # Select which detailed stats for constructs. Either all bigger than
  # a threshold or the ones selected manually.
  if (!is.na(c.out[1])) {
    c.select <- c.out
  } else if (!is.na(c.threshold[1])) {
    c.select <- which(.5 * conflict.c / conflict.total * 100 > c.threshold)
  } else {
    c.select <- NA
  }

  c.stats <- list() # list with detailed results
  if (!is.na(c.select[1])) {
    for (c in seq_along(c.select)) {
      c.stats[[c]] <- conflictAttributedByElementForConstruct(c.select[c])
    }
    names(c.stats) <- cnames[c.select]
  }

  res <- list(
    potential = conflicts.potential,
    actual = conflict.total,
    overall = conflict.total / conflicts.potential * 100,
    e.count = conflict.e,
    e.perc = conflict.e.df / conflict.total * 100,
    c.count = conflict.c,
    c.perc = .5 * conflict.c.df / conflict.total * 100,
    e.stats = e.stats,
    c.stats = c.stats,
    e.threshold = e.threshold, # threshold for elements
    c.threshold = c.threshold,
    enames = enames, # element names
    cnames = cnames
  )
  class(res) <- "indexConflict3"
  res
}


### Output to console ###
indexConflict3Out1 <- function(x, digits = 1) {
  cat("\n##########################################################")
  cat("\nCONFLICT OR INCONSISTENCIES BASED ON TRIANGLE INEQUALITIES")
  cat("\n##########################################################\n")
  cat("\nPotential conflicts in grid: ", x$potential)
  cat("\nActual conflicts in grid: ", x$actual)
  cat(
    "\nOverall percentage of conflict in grid: ",
    round(x$actual / x$potential * 100, digits), "%\n"
  )

  cat("\nELEMENTS")
  cat("\n########\n")
  cat("\nPercent of conflict attributable to element:\n\n")
  print(round(x$e.perc * 100, digits))
  cat("\nChi-square test of equal count of conflicts for elements.\n")
  print(chisq.test(x$e.count))

  cat("\nCONSTRUCTS")
  cat("\n##########\n")
  cat("\nPercent of conflict attributable to construct:\n\n")
  print(round(x$c.perc, digits))
  cat("\nChi-square test of equal count of conflicts for constructs.\n")
  print(chisq.test(x$c.count))
  # print(sd(conflict.c.perc))
  # print(var(conflict.c.perc))
}


indexConflict3Out2 <- function(x, digits = 1, discrepancies = TRUE) {
  e.stats <- x$e.stats
  e.threshold <- x$e.threshold
  enames <- x$enames

  if (length(e.stats) == 0) { # stop function in case
    return(NULL)
  }

  cat("\n\nCONFLICTS BY ELEMENT")
  cat("\n####################\n")
  if (!is.na(e.threshold)) {
    cat("(Details for elements with conflict >", e.threshold, "%)\n")
  }

  for (e in seq_along(e.stats)) {
    m <- e.stats[[e]]
    if (!is.null(m)) {
      cat("\n\n### Element: ", enames[m$e], "\n")
      cat("\nNumber of conflicting construct pairs: ", m$pairs, "\n")
      if (discrepancies) {
        cat("\nConstruct conflict discrepancies:\n\n")
        disc <- round(m$disc, digits)
        print(as.data.frame(formatMatrix(disc,
          rnames = "",
          mode = 2, diag = FALSE
        ), stringsAsFactors = FALSE))
      }
      cat("\nPercent of conflict attributable to each construct:\n\n")
      print(round(m$constructs, digits))
      cat("\nAv. level of discrepancy:   ", round(m$avg, digits), "\n")
      cat("\nStd. dev. of discrepancies: ", round(m$sd, digits + 1), "\n")
    }
  }
}


indexConflict3Out3 <- function(x, digits = 1, discrepancies = TRUE) {
  c.threshold <- x$c.threshold
  c.stats <- x$c.stats
  cnames <- x$cnames

  if (length(c.stats) == 0) { # stop function in case
    return(NULL)
  }

  cat("\n\nCONFLICTS BY CONSTRUCT")
  cat("\n######################\n")
  if (!is.na(c.threshold)) {
    cat("(Details for constructs with conflict >", c.threshold, "%)\n")
  }

  for (c in seq_along(c.stats)) {
    x <- c.stats[[c]]
    if (!is.null(x)) {
      cat("\n\n### Construct: ", cnames[x$c1], "\n")
      if (discrepancies) {
        cat("\nElement-construct conflict discrepancies:\n\n")
        disc <- round(x$disc, digits)
        print(as.data.frame(formatMatrix(disc,
          rnames = paste("c", seq_len(nrow(x$disc)), sep = ""),
          cnames = paste("e", seq_len(ncol(x$disc)), sep = ""),
          pre.index = c(FALSE, FALSE),
          mode = 2, diag = FALSE
        ), stringsAsFactors = FALSE))
      }
      cat("\nAv. level of discrepancy:   ", round(x$avg, digits), "\n")
      cat("\nStd. dev. of discrepancies: ", round(x$sd, digits + 1), "\n")
    }
  }
}


#' print method for class indexConflict3
#'
#' @param x Output from function indexConflict3
#' @param output Type of output. `output=1` will print all results to the console, `output=2` will only print
#'   the detailed statistics for elements and constructs.
#' @param digits Numeric. Number of digits to round to (default is `2`).
#' @param discrepancies Logical. Whether to show matrices of discrepancies in detailed element and construct stats
#'   (default `TRUE`).
#' @param ... Not evaluated.
#' @export
#' @method print indexConflict3
#' @keywords internal
#'
print.indexConflict3 <- function(x, digits = 2, output = 1, discrepancies = TRUE, ...) {
  if (output == 1) {
    indexConflict3Out1(x, digits = digits)
  }
  indexConflict3Out2(x, digits = digits, discrepancies = discrepancies)
  indexConflict3Out3(x, digits = digits, discrepancies = discrepancies)
}


# . ----
# __ Implicative Dilemma ----------------------------------

# plots distribution of construct correlations
#
indexDilemmaShowCorrelationDistribution <- function(x, e1, e2) {
  rc.including <- constructCor(x)
  rc.excluding <- constructCor(x[, -c(e1, e2)])
  rc.inc.vals <- abs(rc.including[lower.tri(rc.including)])
  rc.exc.vals <- abs(rc.excluding[lower.tri(rc.excluding)])

  histDensity <- function(vals, probs = c(.2, .4, .6, .8, .9), ...) {
    h <- hist(vals,
      breaks = seq(0, 1.01, len = 21), freq = FALSE,
      xlim = c(0, 1), border = "white", col = grey(.8), ...
    )
    d <- density(vals)
    lines(d$x, d$y)
    q <- quantile(vals, probs = probs)
    abline(v = q, col = "red")
    text(q, 0, paste(round(probs * 100, 0), "%"), cex = .8, pos = 2, col = "red")
  }

  layout(matrix(c(1, 2), ncol = 1))
  par(mar = c(3, 4.2, 2.4, 2))
  histDensity(rc.inc.vals,
    cex.main = .8, cex.axis = .8, cex.lab = .8,
    main = "Distribution of absolute construct-correlations \n(including 'self' and 'ideal self')"
  )
  histDensity(rc.exc.vals,
    cex.main = .8, cex.axis = .8, cex.lab = .8,
    main = "Distribution of absolute construct-correlations \n(excluding 'self' and 'ideal self')"
  )
}


# internal workhorse for indexDilemma
#
# @param x               `repgrid` object.
# @param self            Numeric. Index of self element.
# @param ideal           Numeric. Index of ideal self element.
# @param diff.mode       Numeric. Method adopted to classify construct pairs into congruent
#                        and discrepant. With `diff.mode=1`, the minimal and maximal
#                        score difference criterion is applied. With `diff.mode=0` the Mid-point
#                        rating criterion is applied. Default is `diff.mode=1`.

# @param diff.congruent  Is used if `diff.mode=1`. Maximal difference between
#                        element ratings to define construct as congruent (default
#                        `diff.congruent=1`). Note that the value
#                        needs to be adjusted by the user according to the rating scale
#                        used.
# @param diff.discrepant Is used if `diff.mode=1`. Minimal difference between
#                        element ratings to define construct as discrepant (default
#                        `diff.discrepant=4`). Note that the value
#                        needs to be adjusted by the user according to the rating scale
#                        used.
# @param diff.poles      Not yet implemented.
# @param r.min           Minimal correlation to determine implications between
#                        constructs ([0, 1]).
# @param exclude         Whether to exclude the elements self and ideal self
#                        during the calculation of the inter-construct correlations.
#                        (default is `FALSE`).
# @param index           Whether to print index numbers in front of each construct
#                        (default is `TRUE`).
# @param trim            The number of characters a construct (element) is trimmed to (default is
#                        `20`). If `NA` no trimming is done. Trimming
#                        simply saves space when displaying the output.
# @param digits          Numeric. Number of digits to round to (default is
#                        `2`).
# @export
# @keywords internal
# @return     A list with four elements containing different steps of the
#                        calculation.
#
#
indexDilemmaInternal <- function(x, self, ideal,
                                 diff.mode = 1, diff.congruent = 1,
                                 diff.discrepant = 4, diff.poles = 1,
                                 r.min, exclude = FALSE, digits = 2,
                                 index = T, trim = FALSE) {
  nc <- nrow(x)
  ne <- ncol(x)
  enames <- elements(x)
  e_ii <- seq_len(ne) # possible element indexes

  if (!self %in% e_ii) {
    stop("'self' element index must be within interval [", 1, ",", ne, "]", call. = FALSE)
  }
  if (!ideal %in% e_ii) {
    stop("'ideal' element index must be within interval [", 1, ",", ne, "]", call. = FALSE)
  }
  if (diff.congruent < 0) {
    stop("'diff.congruent' must be non-negative", call. = FALSE)
  }
  if (diff.discrepant < 0) {
    stop("'diff.discrepant' must be non-negative", call. = FALSE)
  }
  if (diff.congruent >= diff.discrepant) {
    stop("'diff.congruent' must be smaller than 'diff.discrepant'", call. = FALSE)
  }
  if (r.min < 0 | r.min > 1) {
    stop("'r.min' must lie in interval [0, 1]", call. = FALSE)
  }

  # r.min <- abs(r.min)  # direction does not matter the way we process
  s <- ratings(x) # grid scores matrix
  # create a vector of inverted scores for the 'self' element:
  # invscr = 8 - scr
  # Example: 2 -> 8 - 2 -> 6
  #          5 -> 8 - 5 -> 3
  s_inverted <- ratings(swapPoles(x)) # grid with inverted scores
  cnames <- getConstructNames2(x, index = index, trim = trim, mode = 1, pre = "", post = " ")
  sc <- getScale(x)
  midpoint <- getScaleMidpoint(x) # NEW (DIEGO) get scale midpoint this is importat in
  # when Alejandro's code check whether self/ideal
  # is == to the midpoint or not (see below "Get Dilemmas" section)

  # FLAG ALL CONSTRUCTS AS DISCREPANT, CONGRUENT OR NEITHER

  diff.between <- abs(s[, self] - s[, ideal]) # self - ideal difference
  is.congruent <- logical()
  type.c <- character()

  # CORRECTION (ALEJANDRO):
  # a construct can't be congruent if it's 'self' score is 4 (AKA self-disorientation).
  # Neither can be congruent if IDEAL is 4 (i.e. midpoint).
  # CORRECTION (Diego): I have just updated this avoid hardcoding the midpoint!!
  if (diff.mode == 1) {
    for (i in 1L:nc) {
      if (s[, self][i] != midpoint) {
        if (s[, ideal][i] != midpoint) {
          is.congruent[i] <- diff.between[i] <= diff.congruent
        } else {
          is.congruent[i] <- FALSE
        }
      } else {
        is.congruent[i] <- FALSE
      }
    }
    is.discrepant <- diff.between >= diff.discrepant
    is.neither <- !is.congruent & !is.discrepant

    type.c[is.congruent] <- "congruent"
    type.c[is.discrepant] <- "discrepant"
    type.c[is.neither] <- "neither"
  }

  # # difference from poles NOT YET IMPLEMENTED
  # sc <- getScale(x)
  # diff.pole1 <- abs(s[, c(e.self, e.ideal)] - sc[1])
  # diff.pole2 <- abs(s[, c(e.self, e.ideal)] - sc[2])
  # #are both elements within the allowed distance from the poles and at the same pole (congruent)
  # is.congruent.p <- diff.pole1[,1] <= diff.poles & diff.pole1[,2] <= diff.poles |
  #                   diff.pole2[,1] <= diff.poles & diff.pole2[,2] <= diff.poles
  # is.discrepant.p <- diff.pole1[,1] <= diff.poles & diff.pole2[,2] <= diff.poles |
  #                     diff.pole1[,1] <= diff.poles & diff.pole2[,2] <= diff.poles
  #
  # is.neither.p <- !is.congruent.p & !is.discrepant.p
  # type.c.poles[is.congruent.p] <- "congruent"
  # type.c.poles[is.discrepant.p] <- "discrepant"
  # type.c.poles[is.neither.p] <- "neither"
  #
  #
  # //////////////////////////////////////////////////////////////////////////////
  ## MIDPOINT-BASED CRITERION TO IDENTIFY CONGRUENT AND DISCREPANT constructs
  # //////////////////////////////////////////////////////////////////////////////
  #### added by DIEGO
  #   I have tried to implement here the other popular method for the identification of
  #   Congruent and Discrepant constructs. This proposed below is that applied by IDIOGRID
  #   software (V.2.3)
  #   IDIOGRID uses "the scale midpoint as the 'dividing line' for discrepancies; for example,
  #   if the actual self (the Subject Element) is rated above the scale midpoint and the ideal
  #   self (the Target Element) is rated below the midpoint, then a discrepancy exists (and
  #   vice versa). If the two selves are rated on the same side of the scale or if either
  #   the actual self or the ideal self are rated at the midpoint of the scale, then a discre-
  #   pancy does not exist." (from IDIOGRID manual)

  else if (diff.mode == 0) {
    is.congruent <- (s[, self] < midpoint & s[, ideal] < midpoint) |
      (s[, self] > midpoint & s[, ideal] > midpoint)
    is.discrepant <- (s[, self] < midpoint & s[, ideal] > midpoint) |
      (s[, self] > midpoint & s[, ideal] < midpoint)
    is.neither <- !is.congruent & !is.discrepant
    type.c[is.congruent] <- "congruent"
    type.c[is.discrepant] <- "discrepant"
    type.c[is.neither] <- "neither"
  } else {
    stop("Differentiation method (diff.mode) must be 0 or 1", call. = FALSE)
  }

  #--------------- END OF MIDPOINT-BASED CRITERION -----------------------------#

  # //////////////////////////////////////////////////////////////////////////////
  # DIEGO: This that I have commented-out is now redundant as the variables are not duplicates
  # anymore and are calculated only in their conditional loop. This is more efficient
  # //////////////////////////////////////////////////////////////////////////////
  #  if (diff.mode == 1){
  #  is.congruent <- is.congruent.e
  #  is.discrepant <- is.discrepant.e
  #  type.construct <- type.c.elem
  #  } else if (diff.mode == 0){ ##### ADDED CHOICE "0" for MIDPOINT RATING CRITERION
  #  is.congruent <- is.congruent.p
  #  is.discrepant <- is.discrepant.p
  #  type.construct <- type.c.poles
  #  }
  # we just need the next line to reconnect with the original indexdilemma routine
  # //////////////////////////////////////////////////////////////////////////////

  type.construct <- type.c
  # GET CORRELATIONS

  # inter-construct correlations including and excluding
  # the elements self and ideal self
  rc.include <- constructCor(x) # TODO digits=digits
  rc.exclude <- constructCor(x[, -c(self, ideal)]) # digits=digits

  # correlations to use for evaluation
  if (exclude) {
    rc.use <- rc.exclude
  } else {
    rc.use <- rc.include
  }

  # type.c.poles <- type.c.elem <- rep(NA, nrow(s)) # set up results vectors
  type.c <- rep(NA, nrow(s))
  # GET DILEMMAS

  # which pairs of absolute construct correlations are bigger than r.min?
  comb <- t(combn(nc, 2)) # all possible correlation pairs (don't repeat)
  n_construct_pairs <- nrow(comb) # = factorial(n) / (2*factorial(n - 2))
  needs.to.invert <- logical()

  # set up result vectors
  check <- bigger.rmin <- r.include <- r.exclude <- type.c1 <- type.c2 <- rep(NA, nrow(comb))

  # check every pair of constructs for characteristics
  for (i in 1L:nrow(comb)) {
    c1 <- comb[i, 1]
    c2 <- comb[i, 2]
    r.include[i] <- rc.include[c1, c2]
    r.exclude[i] <- rc.exclude[c1, c2]
    type.c1[i] <- type.construct[c1]
    type.c2[i] <- type.construct[c2]

    # CORRECTION:
    # To create a dilemma, the 'self' scores of both constructs must be
    # on the same pole. We have to check for that.

    # REMOVED HARDCODED MIDPOINT
    # DIEGO: 4 is the midpoint and it was "hardcoded". This is not good if we have a scoring range
    # that is not 1-7 because in that case the midpoint will NOT be 4!
    #
    # DIEGO: another bug-fix is that in the section where the scripts "reorient" the constructs:
    # the code to re-orient the constructs is not controlling for self or ideal self to be scored
    # as the midpoint. This causes the script break. I have added a condition for those combinations
    # equivalent to self-score != midpoint

    if (s[c1, self] != midpoint & s[c2, self] != midpoint) {
      if (s[c1, self] > midpoint & s[c2, self] > midpoint) {
        if (rc.use[c1, c2] >= r.min) { # CORRECTION: don't use ABS values,
          # we invert scores to check constructs
          # to find correlations the other way
          bigger.rmin[i] <- TRUE
        } else {
          bigger.rmin[i] <- FALSE
        }
        check[i] <- (is.congruent[c1] & is.discrepant[c2]) |
          (is.discrepant[c1] & is.congruent[c2])
        needs.to.invert[c1] <- TRUE
        needs.to.invert[c2] <- TRUE
      } else if (s[c1, self] < midpoint & s[c2, self] < midpoint) {
        if (rc.use[c1, c2] >= r.min) {
          bigger.rmin[i] <- TRUE
        } else {
          bigger.rmin[i] <- FALSE
        }
        check[i] <- (is.congruent[c1] & is.discrepant[c2]) |
          (is.discrepant[c1] & is.congruent[c2])
        needs.to.invert[c1] <- FALSE
        needs.to.invert[c2] <- FALSE
      }

      # NEW:
      # Now check for inverted scores.
      # You only need to invert one construct at a time

      if (s_inverted[c1, self] > midpoint & s[c2, self] > midpoint) {
        r.include[i] <- cor(s_inverted[c1, ], s[c2, ])
        r.exclude[i] <- "*Not implemented"
        if (r.include[i] >= r.min) {
          bigger.rmin[i] <- TRUE
        } else {
          bigger.rmin[i] <- FALSE
        }
        check[i] <- (is.congruent[c1] & is.discrepant[c2]) |
          (is.discrepant[c1] & is.congruent[c2])
        needs.to.invert[c2] <- TRUE
      } else if (s_inverted[c1, self] < midpoint & s[c2, self] < midpoint) {
        r.include[i] <- cor(s_inverted[c1, ], s[c2, ])
        r.exclude[i] <- "*Not implemented"
        if (r.include[i] >= r.min) {
          bigger.rmin[i] <- TRUE
        } else {
          bigger.rmin[i] <- FALSE
        }
        check[i] <- (is.congruent[c1] & is.discrepant[c2]) |
          (is.discrepant[c1] & is.congruent[c2])
        needs.to.invert[c1] <- TRUE
      }

      if (s[c1, self] > midpoint & s_inverted[c2, self] > midpoint) {
        r.include[i] <- cor(s[c1, ], s_inverted[c2, ])
        r.exclude[i] <- "*Not implemented"
        if (r.include[i] >= r.min) {
          bigger.rmin[i] <- TRUE
        } else {
          bigger.rmin[i] <- FALSE
        }
        check[i] <- (is.congruent[c1] & is.discrepant[c2]) |
          (is.discrepant[c1] & is.congruent[c2])
        needs.to.invert[c1] <- TRUE
      } else if (s[c1, self] < midpoint & s_inverted[c2, self] < midpoint) {
        r.include[i] <- cor(s[c1, ], s_inverted[c2, ])
        r.exclude[i] <- "*Not implemented"
        if (r.include[i] >= r.min) {
          bigger.rmin[i] <- TRUE
        } else {
          bigger.rmin[i] <- FALSE
        }
        check[i] <- (is.congruent[c1] & is.discrepant[c2]) |
          (is.discrepant[c1] & is.congruent[c2])
        needs.to.invert[c2] <- TRUE
      }
    } else { # DIEGO: closing of the if() where I put the condition for self to be != to the midpoint score
      needs.to.invert[c1] <- FALSE
      needs.to.invert[c2] <- FALSE
    }
    # print(paste(needs.to.invert,s[c1,self],s[c2,self])) # Diego debug printout of variables
  }
  # New: invert construct label poles if needed
  needs.to.invert[is.na(needs.to.invert)] <- FALSE
  leftpole <- constructs(x)$leftpole
  rightpole <- constructs(x)$rightpole

  for (i in 1L:nc) {
    if (needs.to.invert[i]) {
      s[i, self] <- s_inverted[i, self]
      s[i, ideal] <- s_inverted[i, ideal]
      cnames[i] <- paste(rightpole[i], leftpole[i], sep = " - ")
    } else {
      cnames[i] <- paste(leftpole[i], rightpole[i], sep = " - ")
    }
  }

  # GET RESULTS

  ## 1: this data frame contains information related to 'self' and 'ideal' elements
  construct_classification <- data.frame(
    construct = cnames, a.priori = type.construct, self = s[, self], ideal = s[, ideal],
    stringsAsFactors = FALSE
  )
  colnames(construct_classification) <- c("Construct", "Classification", "Self", "Ideal")
  rownames(construct_classification) <- NULL
  construct_classification <- construct_classification %>%
    dplyr::mutate(
      Difference = abs(Self - Ideal)
    ) %>%
    select(Construct, Self, Ideal, Difference, Classification)

  ## 2: This dataframe stores the information for all posible construct combinations
  all_pairs <- data.frame(
    c1 = comb[, 1], c2 = comb[, 2], r.inc = r.include,
    r.exc = r.exclude, bigger.rmin, type.c1, type.c2, check,
    name.c1 = cnames[comb[, 1]], name.c2 = cnames[comb[, 2]],
    stringsAsFactors = FALSE
  )

  ## 3: This dataframe contains information for all the dilemmas
  dilemmas_info <- subset(all_pairs, check == TRUE & bigger.rmin == TRUE)
  no_ids <- nrow(dilemmas_info) # Number of implicative dilemmas
  cnstr.labels <- character()
  cnstr.labels.left <- cnstr.labels.right <- cnstr.labels
  cnstr.id.left <- cnstr.id.right <- numeric()

  # Put all discrepant constructs to the right
  if (no_ids != 0) {
    for (v in seq_len(no_ids)) {
      if (dilemmas_info$type.c1[v] == "discrepant") {
        cnstr.labels.left[v] <- dilemmas_info[v, "name.c2"]
        cnstr.labels.right[v] <- dilemmas_info[v, "name.c1"]
        cnstr.id.left[v] <- dilemmas_info[v, "c2"]
        cnstr.id.right[v] <- dilemmas_info[v, "c1"]
      } else {
        cnstr.labels.left[v] <- dilemmas_info[v, "name.c1"]
        cnstr.labels.right[v] <- dilemmas_info[v, "name.c2"]
        cnstr.id.left[v] <- dilemmas_info[v, "c1"]
        cnstr.id.right[v] <- dilemmas_info[v, "c2"]
      }
    }
  }


  ## 4: reordered dilemma output
  cnstr.labels.left <- paste0(cnstr.id.left, ". ", cnstr.labels.left)
  cnstr.labels.right <- paste0(cnstr.id.right, ". ", cnstr.labels.right)
  if (no_ids == 0) {
    cnstr.id.left <- numeric()
    cnstr.id.right <- numeric()
    cnstr.labels.left <- character()
    cnstr.labels.right <- character()
  }
  dilemmas_df <- data.frame(cnstr.id.left, cnstr.labels.left,
    cnstr.id.right, cnstr.labels.right,
    Rtot = dilemmas_info[, 3], RexSI = dilemmas_info[, 4],
    stringsAsFactors = FALSE
  )
  # colnames(dilemmas_df) = c('Self - Not self', 'Rtot', 'Self - Ideal', 'RexSI')
  colnames(dilemmas_df) <- c("id_c", "Congruent", "id_d", "Discrepant", "R", "RexSI")

  ## 5: measures
  d <- no_ids
  r <- dilemmas_df$R # correlations between ID pairs

  # PID
  # percentage of IDs over total number of possible constructs pairs
  pid <- d / nrow(comb)

  # IID
  # Intensity of the implicative dilemma (IID), quotient of the number of
  # constructs by the probability of finding implicative dilemmas given the matrix size.
  # iid = sqrt(sum(r^2)) / d * 100    # "correct" version from paper
  iid <- sum(r) / d * 100 # version as in Gridcor (not as in paper)

  # PICID
  # proportion of the intensity of constructs of implicative dilemmas
  # picid = sqrt(sum(r^2)) / n_construct_pairs * 100   # "correct" version from paper
  picid <- sum(r) / n_construct_pairs * 100 # version in Gridcor (not as in paper)

  # gather measures
  measures <- list(
    iid = iid,
    pid = pid,
    picid = picid
  )

  # REALIGEND GRID
  ii_swap <- which(needs.to.invert)
  x_aligned <- swapPoles(x, pos = ii_swap)

  # CONSTRUCTS INVOLVED in IDS
  i_involved <- union(dilemmas_info$c1, dilemmas_info$c2)

  # indexDilemma object
  l <- list(
    no_ids = no_ids,
    n_construct_pairs = n_construct_pairs, # = factorial(n) / (2*factorial(n - 2))
    self = self,
    reversed = which(needs.to.invert),
    constructs_involved = i_involved,
    ideal = ideal,
    elements = enames,
    diff.discrepant = diff.discrepant,
    diff.congruent = diff.congruent,
    exclude = exclude,
    r.min = r.min,
    diff.mode = diff.mode,
    midpoint = midpoint,
    measures = measures,
    # dataframes
    construct_classification = construct_classification, # discrepant / congruent
    dilemmas_info = dilemmas_info,
    dilemmas_df = dilemmas_df, # table with dilemmas and correlations
    grid_aligned = x_aligned
  )
  class(l) <- c("indexDilemma", class(l))
  l
}


#' Print method for class indexDilemma
#'
#' @param x         Object of class indexDilemma
#' @param digits    Numeric. Number of digits to round to (default is `2`).
#' @param output    String with each letter indicating which parts of the output to print
#'                  (default is `"OCD"`, order does not matter):
#'                  `S` = Summary (Number of IDs, PID, etc.),
#'                  `P` = Analysis parameters,
#'                  `C` = Construct classification table,
#'                  `D` = Implicative dilemmas table.
#' @param ...       Not evaluated.
#' @method          print indexDilemma
#' @keywords        internal
#' @export
print.indexDilemma <- function(x, digits = 2, output = "SPCD", ...) {
  output <- toupper(output)
  enames <- x$elements
  self <- x$self
  ideal <- x$ideal
  diff.mode <- x$diff.mode
  diff.discrepant <- x$diff.discrepant
  diff.congruent <- x$diff.congruent
  r.min <- x$r.min
  no_ids <- x$no_ids
  n_construct_pairs <- x$n_construct_pairs
  exclude <- x$exclude
  midpoint <- x$midpoint
  dilemmas_df <- x$dilemmas_df

  # measures
  pid <- x$measures$pid
  iid <- x$measures$iid
  picid <- x$measures$picid

  cat("\n####################\n")
  cat("Implicative Dilemmas")
  cat("\n####################\n")

  ## Summary and Measures
  if (str_detect(output, "S")) {
    cat("\n-------------------------------------------------------------------------------")
    cat(bold("\n\nSUMMARY:\n"))
    cat("\nNo. of Implicative Dilemmas (IDs):", no_ids)
    cat("\nNo. of possible construct pairs:", n_construct_pairs)
    pid_perc <- scales::percent(pid, .1)
    pid_no <- paste0("(", no_ids, "/", n_construct_pairs, ")")
    cat("\nPercentage of IDs (PID):", pid_perc, pid_no)
    cat("\nIntensity of IDs (IID):", round(iid, 1))
    cat("\nProportion of the intensity of constructs of IDs (PICID):", round(picid, 1))
  }

  ## Parameters
  if (str_detect(output, "P")) {
    cat("\n\n-------------------------------------------------------------------------------")
    cat(bold("\n\nPARAMETERS:\n"))
    cat("\nSelf: Element No.", paste0(self, " = ", enames[self]))
    cat("\nIdeal: Element No.", paste0(ideal, " = ", enames[ideal]))
    cat("\n\nCorrelation Criterion: >=", r.min)
    if (exclude) {
      cat("\nNote: Correlation calculated excluding elements Self & Ideal")
    } else {
      cat("\nNote: Correlation calculated including elements Self & Ideal\n")
    }
    cat("\nCriteria (for construct classification):")
    # differentiation mode 0 for midpoint-based criterion (Grices - Idiogrid) OR
    # differentiation mode 1 for Feixas "correlation cut-off" criterion
    if (diff.mode == 1) {
      cat("\nDiscrepant if Self-Ideal difference: >=", diff.discrepant)
      cat("\nCongruent if Self-Ideal difference: <=", diff.congruent)
    } else if (diff.mode == 0) {
      cat("\nUsing Midpoint rating criterion")
    }
  }
  # Extreme Criteria:
  # Discrepant Difference: Self-Ideal greater than or equal to, Max Other-Self difference
  # Congruent Difference: Self-Ideal less than or equal to, Min Other-Self difference

  ## Classification of constructs:
  if (str_detect(output, "C")) {
    cat("\n\n-------------------------------------------------------------------------------")
    cat(bold("\n\nCLASSIFICATION OF CONSTRUCTS:\n"))
    cat(blue("\n   Note: Constructs aligned so 'Self' corresponds to left pole\n\n"))

    # cat(paste0("\n   Note: 'Self' corresponds to left pole ",
    #            "unless score equals the midpoint (", midpoint, " = undecided)\n\n"))
    print(x$construct_classification)
  }

  ## Implicative Dilemmas:
  if (str_detect(output, "D")) {
    cat("\n-------------------------------------------------------------------------------")
    cat(bold("\n\nIMPLICATIVE DILEMMAS:\n"))
    cat(blue("\n   Note: Congruent constructs on the left - Discrepant constructs on the right"))
    cat("\n\n")

    if (nrow(dilemmas_df) > 0) {
      dilemmas_df <- dilemmas_df %>% select(-id_c, -id_d)
      dilemmas_df$R <- round(dilemmas_df$R, digits)
      ii <- str_detect(dilemmas_df$RexSI, "\\.")
      dilemmas_df$RexSI[ii] <- as.character(round(as.numeric(dilemmas_df$RexSI[ii]), digits))
      print(dilemmas_df)
      cat("\n\tR = Correlation including Self & Ideal")
      cat("\n\tRexSI = Correlation excluding Self & Ideal")
      cor.used <- ifelse(exclude, "RexSI", "R")
      cat("\n\t", cor.used, " was used as criterion", sep = "")
    } else {
      cat("No implicative dilemmas detected")
    }
  }
}



#' Implicative Dilemmas
#'
#' Implicative dilemmas are closely related to the notion of conflict. An implicative dilemma arises when a desired
#' change on one construct is associated with an undesired implication on another construct. E. g. a timid subject may
#' want to become more socially skilled but associates being socially skilled with different negative characteristics
#' (selfish, insensitive etc.). Hence, he may anticipate that becoming less timid will also make him more selfish (cf.
#' Winter, 1982). As a consequence, the subject will resist to the change if the negative presumed implications will
#' threaten the patients identity and the predictive power of his construct system. From this stance the resistance to
#' change is a logical consequence coherent with the subjects construct system (Feixas, Saul, & Sanchez, 2000). The
#' investigation of the role of cognitive dilemma in different disorders in the context of PCP is a current field of
#' research (e.g. Feixas & Saul, 2004, Dorough et al. 2007).
#'
#' The detection of implicative dilemmas happens in two steps. First the constructs are classified as being 'congruent'
#' or 'discrepant'. Secondly, the correlation between a congruent and discrepant construct pair is assessed if it is
#' big enough to indicate an implication.
#'
#' **Classifying the construct**
#'
#' To detect implicit dilemmas the construct pairs are first identified as 'congruent' or 'discrepant'. The assessment
#' is based on the rating differences between the elements 'self' and 'ideal self'. A construct is 'congruent' if the
#' construction of the 'self' and the preferred state (i.e. ideal self) are the same or similar. A construct is
#' discrepant if the construction of the 'self' and the 'ideal' is dissimilar.
#'
#' There are two popular accepted methods to identify congruent and discrepant constructs:
#'
#' 1. "Scale Midpoint criterion" (cf. Grice 2008)
#' 2.  "Minimal and maximal score difference" (cf. Feixas & Saul, 2004)
#'
#' *"Scale Midpoint criterion" (cf. Grice 2008)*
#'
#' As reported in the Idiogrid (v. 2.4) manual: "... The Scale Midpoint uses the scales as the 'dividing line' for
#' discrepancies; for example, if the actual element is rated above the midpoint, then the discrepancy exists (and vice
#' versa). If the two selves are the same as the actual side of the scale, then a discrepancy does not exist". As an
#' example:
#'
#' Assuming a scoring range of 1-7, the midpoint score will be 4, we then look at self and ideal-self scoring on any
#' given construct and we proceed as follow:
#'
#' - If the scoring of Self AND Ideal Self are both < 4: construct is "Congruent"
#' - If the scoring of Self AND Ideal Self are both > 4: construct is "Congruent"
#' - If the scoring of Self is < 4 AND Ideal Self is > 4 (OR vice versa): construct is "discrepant"
#' - If scoring Self OR Ideal Self = 4 then the construct is NOT Discrepant and it is "Undifferentiated"
#'
#' *Minimal and maximal score difference criterion (cf. Feixas & Saul, 2004)*
#'
#' This other method is more conservative and it is designed to minimize Type I errors by a) setting a default minimum
#' correlation between constructs of `r=.34`; b) discarding cases where the ideal Self and self are neither congruent
#' or discrepant; c) discarding cases where ideal self is "not oriented", i.e. scored at the midpoint.
#'
#' E.g. suppose the element 'self' is rated 2 and 'ideal self' 5 on a scale from 1 to 6. The ratings differences are
#' 5-2 = 3. If this difference is smaller than e.g. 1 the construct is 'congruent', if it is bigger than 3 it is
#' 'discrepant'.
#'
#' The values used to classify the constructs 'congruent' or 'discrepant' can be determined in several ways (cf. Bell,
#' 2009):
#'
#' 1. They are set 'a priori'.
#' 2. They are implicitly derived by taking into account the rating
#'    differences to the other constructs. (Not yet implemented)
#'
#' The value mode is determined via the argument `diff.mode`.
#'
#' If no 'a priori' criteria to determine whether the construct is congruent or discrepant is supplied as an argument,
#' the values are chosen according to the range of the rating scale used. For the following scales the defaults are
#' chosen as:
#'
#'  | Scale                  | 'A priori' criteria   |
#'  |------------------------|-----------------------|
#'  | 1 2                    | --> con: <=0    disc: >=1 |
#'  | 1 2 3                  | --> con: <=0    disc: >=2 |
#'  | 1 2 3 4                | --> con: <=0    disc: >=2 |
#'  | 1 2 3 4 5              | --> con: <=1    disc: >=3 |
#'  | 1 2 3 4 5 6            | --> con: <=1    disc: >=3 |
#'  | 1 2 3 4 5 6 7          | --> con: <=1    disc: >=4 |
#'  | 1 2 3 4 5 6 7 8        | --> con: <=1    disc: >=5 |
#'  | 1 2 3 4 5 6 7 8 9      | --> con: <=2    disc: >=5 |
#'  | 1 2 3 4 5 6 7 8 9 10   | --> con: <=2    disc: >=6 |
#'
#' **Defining the correlations**
#'
#' As the implications between constructs cannot be derived from a rating grid directly, the correlation between two
#' constructs is used as an indicator for implication. A large correlation means that one construct pole implies the
#' other. A small correlation indicates a lack of implication. The minimum criterion for a correlation to indicate
#' implication is set to .35 (cf. Feixas & Saul, 2004). The user may also choose another value. To get a an impression
#' of the distribution of correlations in the grid, a visualization can be prompted via the argument `show`. When
#' calculating the correlation used to assess if an implication is given or not, the elements under consideration (i.
#' e. self and ideal self) can be included (default) or excluded. The options will cause different correlations (see
#' argument `exclude`).
#'
#' **Example of an implicative dilemma**
#'
#' A depressive person considers herself as 'timid' and wished to change to the opposite pole she defines as
#' 'extraverted'. This construct is called discrepant as the construction of the 'self' and the desired state (e.g.
#' described by the 'ideal self') on this construct differ. The person also considers herself as 'sensitive' (preferred
#' pole) for which the opposite pole is 'selfish'. This construct is congruent, as the person construes herself as she
#' would like to be. If the person now changed on the discrepant construct from the undesired to the desired pole, i.e.
#' from timid to extraverted, the question can be asked what consequences such a change has. If the person construes
#' being timid and being sensitive as related and that someone who is extraverted will not be timid, a change on the
#' first construct will imply a change on the congruent construct as well. Hence, the positive shift from timid to
#' extraverted is presumed to have a undesired effect in moving from sensitive towards selfish. This relation is called
#' an implicative dilemma. As the implications of change on a construct cannot be derived from a rating grid directly,
#' the correlation between two constructs is used as an indicator of implication.
#'
#' @param x A `repgrid` object.
#' @param self Numeric. Index of self element.
#' @param ideal Numeric. Index of ideal self element.
#' @param diff.mode Numeric. Method adopted to classify construct pairs into congruent and discrepant. With
#'   `diff.mode=1`, the minimal and maximal score difference criterion is applied. With `diff.mode=0` the Mid-point
#'   rating criterion is applied. Default is `diff.mode=1`.
#' @param diff.congruent Is used if `diff.mode=1`. Maximal difference between element ratings to define construct as
#'   congruent (default `diff.congruent=1`). Note that the value needs to be adjusted by the user according to the
#'   rating scale used.
#' @param diff.discrepant Is used if `diff.mode=1`. Minimal difference between element ratings to define construct as
#'   discrepant (default `diff.discrepant=3`). Note that the value needs to be adjusted by the user according to the
#'   rating scale used.
#' @param diff.poles Not yet implemented.
#' @param r.min Minimal correlation to determine implications between constructs.
#' @param exclude Whether to exclude the elements self and ideal self during the calculation of the
#'   inter-construct correlations. (default is `FALSE`).
#' @param show Whether to additionally plot the distribution of correlations to help the user assess what
#'   level is adequate for `r.min`.
#' @param index Whether to print index numbers in front of each construct (default is `TRUE`).
#' @param trim The number of characters a construct (element) is trimmed to (default is `20`). If `NA` no
#'   trimming is done. Trimming simply saves space when displaying the output.
#' @param digits Numeric. Number of digits to round to (default is `2`).
#' @param output The type of output to return.
#'
#' @author Mark Heckmann, Alejandro García, Diego Vitali
#' @return  List object of class `indexDilemma`, containing the result from the calculations.
#' @references
#'   Bell, R. C. (2009). *Gridstat version 5 - A Program for Analyzing the Data of A Repertory Grid*
#'   (manual). University of Melbourne, Australia: Department of Psychology.
#'
#'   Dorough, S., Grice, J. W., & Parker, J. (2007). Implicative dilemmas and psychological well-being. *Personal
#'   Construct Theory & Practice*, (4), 83-101.
#'
#'   Feixas, G., & Saul, L. A. (2004). The Multi-Center Dilemma Project: an investigation on the role of cognitive
#'   conflicts in health. *The Spanish Journal of Psychology, 7*(1), 69-78.
#'
#'   Feixas, G., Saul, L. A., & Sanchez, V. (2000). Detection and analysis of implicative dilemmas: implications for
#'   the therapeutic process. In J. W. Scheer (Ed.), *The Person in Society: Challenges to a Constructivist Theory*.
#'   Giessen: Psychosozial-Verlag.
#'
#'   Winter, D. A. (1982). Construct relationships, psychological disorder and therapeutic change. *British Journal of
#'   Medical Psychology, 55* (Pt 3), 257-269.
#'
#'   Grice, J. W. (2008). Idiogrid: Idiographic Analysis with Repertory Grids (Version 2.4). Oklahoma: Oklahoma State
#'   University.
#' @seealso [print.indexDilemma()], [plot.indexDilemma()]
#' @export
#' @example inst/examples/example-implicative-dilemmas.R
#'
indexDilemma <- function(x, self = 1, ideal = ncol(x),
                         diff.mode = 1, diff.congruent = NA,
                         diff.discrepant = NA, diff.poles = 1,
                         r.min = .35, exclude = FALSE, digits = 2, show = FALSE,
                         output = 1, index = TRUE, trim = 20) {
  # automatic selection of a priori criteria
  sc <- getScale(x)
  if (is.na(diff.congruent)) {
    diff.congruent <- floor(diff(sc) * .25)
  }
  if (is.na(diff.discrepant)) {
    diff.discrepant <- ceiling(diff(sc) * .6)
  }

  # detect dilemmas
  res <- indexDilemmaInternal(x,
    self = self, ideal = ideal,
    diff.mode = diff.mode, diff.congruent = diff.congruent,
    diff.discrepant = diff.discrepant, diff.poles = diff.poles,
    r.min = r.min, exclude = exclude, digits = digits,
    index = index, trim = trim
  )
  if (show) {
    indexDilemmaShowCorrelationDistribution(x, self, ideal)
  }

  res
}



#' Plot method for indexDilemma (network graph)
#'
#' Produces a network graph using of the detected implicative dilemmas using the
#'  `igraph` package.
#'
#' @param x Object returned by `indexDilemma`.
#' @param layout Name of layout. One of `rows`, `circle`, `star`, or `nicely` or a
#'   `igraph` layout function.
#' @param both.poles Show both construct poles? (default `TRUE`). If `FALSE`
#' only the poles corresponding to the implied undesired changes are shown.
#' @param digits Number of digits for correlations.
#' @param node.size Size of nodes (default `50`).
#' @param node.text.cex Text size of construct labels.
#' @param node.label.color Color of construct labels.
#' @param node.color.discrepant,node.color.congruent Color of discrepant and congruent constructs nodes.
#' @param edge.label.color,edge.label.cex Color and size of correlation labels.
#' @param edge.color,edge.arrow.size Color and Size of arrow.
#' @param edge.lty Linetype of arrow.
#' @keywords internal
#' @export
#'
plot.indexDilemma <- function(
    x,
    layout = "rows",
    both.poles = TRUE,
    node.size = 50,
    node.text.cex = 1,
    node.label.color = "black",
    node.color.discrepant = "darkolivegreen3",
    node.color.congruent = "lightcoral",
    edge.label.color = grey(.4),
    edge.label.cex = 1,
    edge.digits = 2,
    edge.arrow.size = .5,
    edge.color = grey(.6),
    edge.lty = 2,
    ...) {
  id <- x # renamed from 'id' to 'x' to match arg in print generic

  # response in case no dilemmas were found
  if (id$no_ids == 0) {
    plot.new()
    text(.5, .5, "No implicative dilemmas detected")
    return(invisible(NULL))
  }

  # rename args
  vertex.size <- node.size
  vertex.label.cex <- node.text.cex

  # get relevant data from indexDilemma object
  x <- id$grid
  r.min <- id$r.min
  dilemmas_df <- id$dilemmas_df
  x <- id$grid_aligned
  i_involved <- id$constructs_involved # constructs involved in IDs
  R <- constructCor(x, trim = NA)
  if (both.poles) {
    vertex_labels <- rownames(R)
  } else {
    vertex_labels <- constructs(x)$rightpole
  }
  vertex_labels <- vertex_labels[i_involved] %>% str_wrap(width = 15)

  # Create directed indicator matrix. Only one direction, i.e. from
  # discrepant to congruent construct = negative implication
  # direction in matrix from row (discrepant) to column (congruent)
  K <- R
  K[, ] <- 0
  for (i in 1L:nrow(dilemmas_df)) {
    K[dilemmas_df$id_d[i], dilemmas_df$id_c[i]] <- 1 # row -> column
  }
  edge_labels <- R[K == 1] %>% round(edge.digits) # round correlations

  # remove non-ID constructs
  W_red <- K[i_involved, i_involved]
  g <- igraph::graph_from_adjacency_matrix(W_red,
    diag = FALSE,
    mode = "directed", weighted = TRUE
  )
  vertex_colors <- dplyr::recode(id$construct_classification$Classification,
    "congruent" = node.color.congruent,
    "discrepant" = node.color.discrepant,
    "neither" = "grey"
  )
  vertex_colors <- vertex_colors[i_involved]
  igraph::V(g)$color <- vertex_colors

  # type vector for bipartite layout (boolean)
  vertex_bipart_type <- dplyr::recode(id$construct_classification$Classification,
    "congruent" = T,
    "discrepant" = F,
    "neither" = NA
  )
  vertex_bipart_type <- vertex_bipart_type[i_involved]
  igraph::V(g)$type <- vertex_bipart_type

  # simplified selection among sensible igraph layouts
  if (is.function(layout)) {
    layout <- layout
  } else if (layout == "star") {
    layout <- igraph::layout_as_star
  } else if (layout == "circle") {
    layout <- igraph::layout_in_circle
  } else if (layout == "rows") {
    layout <- igraph::layout_as_bipartite
  } else {
    layout <- igraph::layout_nicely
  }

  old_par <- par(oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
  on.exit(old_par)
  igraph::plot.igraph(g,
    frame = FALSE, ylim = c(-1.3, 1),
    layout = layout, rescale = TRUE,
    edge.curved = FALSE,
    edge.arrow.size = edge.arrow.size,
    edge.label.cex = edge.label.cex,
    edge.lty = edge.lty,
    edge.width = 1.5,
    edge.label = edge_labels,
    edge.color = edge.color,
    edge.label.color = edge.label.color,
    vertex.size = vertex.size,
    vertex.size2 = vertex.size,
    vertex.label = vertex_labels,
    vertex.label.color = node.label.color,
    vertex.label.cex = vertex.label.cex,
    vertex.label.family = "sans",
    vertex.color = vertex_colors,
    vertex.frame.color = grey(.5)
  )
  legend(
    x = "bottom",
    legend = c("a desired change on 'discrepant' construct", "implies an undesired change on 'congruent' construct"),
    bty = "n", cex = 1, inset = c(0, 0),
    xjust = .5, box.col = FALSE, horiz = FALSE, yjust = 1,
    fill = c(node.color.discrepant, node.color.congruent)
  )
}


# dilemmaViz <- function(x)
# {
# self <- id$self
# ideal <- id$ideal
# i <- 1
# id$dilemmas_info
#
# r <- ratings(x)
# r_self <- r[i, self]
# r_ideal <- r[i, ideal]
#
# r = .39                      1   2   3   4   5   6   7
# congruent:  jayn jnay inay  |SI-----------------------| jayn jnay inay
# discrepant:  sxknsx kmsx    |S----------I-------------| iisxsxsxsxsx
#
# library(crayon)
#
#
# }




# //////////////////////////////////////////////////////////////////////////////

# Pemutation test to test if grid is random.
# "The null hypothesis [is] that a particular grid
# is indis- tinguishable from an array of random numbers"
# (Slater, 1976, p. 129).
#
# randomTest <- function(x){
#   x
# }
# permutationTest
# Hartmann 1992:
# To illustrate: If a person decided to produce a nonsense grid,
# the most appropriate way to achieve this goal would be to rate
# (rank) the elements randomly. The variation of the elements on
# the con- structs would lack any psychological sense. Every
# statistical analysis should then lead to noninterpretable results.
