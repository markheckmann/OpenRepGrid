# =============================================================================
# E-GTSP Standalone Solver
# =============================================================================
#
# Self-contained E-GTSP (Equality Generalized Traveling Salesman Problem)
# solver. Copy this single file into any R project to use the E-GTSP solver
# without installing the egtsp package.
#
# The E-GTSP: given n nodes partitioned into m clusters, find the shortest
# round trip that visits exactly one node from each cluster.
#
# Two solver backends are provided:
#   - TSP backend (heuristic): transforms the E-GTSP into a standard ATSP
#     using the Noon-Bean transformation, then solves with a TSP heuristic.
#     Fast but no optimality guarantee.
#   - MILP backend (exact): formulates the E-GTSP directly as a mixed-integer
#     linear program. Guarantees optimality but slower for large instances.
#
# Required packages (install as needed):
#   - TSP                                        (for heuristic backend)
#   - ompr, ompr.roi, ROI, ROI.plugin.glpk       (for exact MILP backend)
#
# Usage:
#   source("egtsp_standalone.R")
#   x   <- new_egtsp(cost_matrix, cluster_vector)
#   sol <- solve_egtsp(x, backend = "TSP")   # heuristic
#   sol <- solve_egtsp(x, backend = "MILP")  # exact
#   sol$gtour       # selected nodes (one per cluster)
#   sol$objective   # tour cost
# =============================================================================


# -- Helpers ------------------------------------------------------------------

# Check that a required package is installed; stop with a helpful message if not
.egtsp_check_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package '", pkg, "' is required for this backend. ",
         "Install it with: install.packages('", pkg, "')", call. = FALSE)
  }
}


# -- Constructor & validation -------------------------------------------------

#' Create a new E-GTSP problem instance
#'
#' @param cost Square numeric cost matrix. Entry cost `[i,j]` is the travel cost
#'   from node i to node j.
#' @param cluster Integer vector assigning each node to a cluster. Must have the
#'   same length as nrow(cost). E.g. c(1,1,2,2,3) means nodes 1-2 are in
#'   cluster 1, nodes 3-4 in cluster 2, node 5 in cluster 3.
#' @param labels Optional character vector of node labels.
#' @return An S3 object of class \code{"egtsp"} with fields:
#'   cost, cluster, labels, n (number of nodes), m (number of clusters).
new_egtsp <- function(cost, cluster, labels = NULL) {
  cost <- as.matrix(cost)
  cluster <- as.integer(cluster)
  n <- nrow(cost)

  # Use row names from cost matrix as labels if available, otherwise 1..n
  if (is.null(labels)) {
    labels <- if (!is.null(rownames(cost))) {
      rownames(cost)
    } else {
      as.character(seq_len(n))
    }
  }

  structure(
    list(cost = cost, cluster = cluster, labels = labels,
         n = n, m = length(unique(cluster))),
    class = "egtsp"
  )
}

#' Validate an E-GTSP instance
#'
#' Checks that the cost matrix and cluster assignments are consistent and
#' well-formed. Called automatically by solve_egtsp(), but can also be used
#' explicitly.
#'
#' @param x An \code{egtsp} object.
#' @return The validated object (invisibly). Throws an error if invalid.
validate_egtsp <- function(x) {
  if (!inherits(x, "egtsp")) {
    stop("'x' must be an 'egtsp' object.", call. = FALSE)
  }
  cost <- x$cost
  cluster <- x$cluster
  n <- x$n

  # Cost must be numeric (not character, logical, etc.)
  if (!is.numeric(cost)) {
    stop("'cost' must be a numeric matrix.", call. = FALSE)
  }

  # Cost must be square: n x n, one row/col per node
  if (nrow(cost) != ncol(cost)) {
    stop("'cost' must be a square matrix (", nrow(cost), " x ", ncol(cost), ").",
         call. = FALSE)
  }

  # Every node needs a cluster assignment
  if (length(cluster) != n) {
    stop("Length of 'cluster' (", length(cluster),
         ") must equal the number of nodes (", n, ").", call. = FALSE)
  }

  # No missing cluster assignments allowed
  if (anyNA(cluster)) {
    stop("'cluster' must not contain NA values.", call. = FALSE)
  }

  # Need at least 2 clusters for the problem to be meaningful
  if (x$m < 2) {
    stop("At least two clusters are required (found ", x$m, ").", call. = FALSE)
  }

  # Warn (but don't error) about Inf/-Inf/NaN in off-diagonal entries
  if (any(!is.finite(cost[row(cost) != col(cost)]))) {
    warning("'cost' matrix contains non-finite off-diagonal values.", call. = FALSE)
  }
  invisible(x)
}


# -- Noon-Bean transformation -------------------------------------------------
#
# The Noon-Bean transformation converts an E-GTSP into a standard Asymmetric
# Traveling Salesman Problem (ATSP). The key idea:
#
# 1. Within each cluster, create a "successor cycle" that links the nodes in a
#    ring (e.g. for cluster {1,2,3}: 1->2->3->1).
#
# 2. Assign a large negative cost (-M) to each successor arc. This ensures the
#    optimal ATSP tour always traverses the full cycle within each cluster.
#
# 3. Set non-successor within-cluster arcs to +Inf (forbidden).
#
# 4. For between-cluster arcs: the cost from node i to node j (different
#    clusters) in the ATSP equals the original cost from successor(i) to j.
#    This "shifts" the cost so that when the tour exits a cluster, it
#    effectively picks the entry node as the representative.
#
# 5. After solving the ATSP, we extract the E-GTSP tour by keeping only the
#    first node encountered from each cluster (the entry point).
#
# The penalty M must be large enough that skipping any successor arc would
# always be suboptimal.

#' Transform E-GTSP to ATSP (Noon-Bean transformation)
#'
#' @param x An \code{egtsp} object.
#' @param M Penalty constant. If NULL (default), auto-computed as
#'   max(|cost|) * n + 1, which is large enough to guarantee correctness.
#' @param cycle Successor cycle method. Currently only \code{"default"}
#'   (natural index order within each cluster).
#' @return An \code{egtsp_atsp} object with fields:
#'   atsp_cost (transformed n x n matrix), successor (integer vector mapping
#'   each node to its successor in the cycle), M (penalty used),
#'   original (reference to the input egtsp object),
#'   cluster_nodes (list mapping each cluster to its node indices).
as_atsp <- function(x, M = NULL, cycle = "default") {
  x <- validate_egtsp(x)
  cost <- x$cost
  cluster <- x$cluster
  n <- x$n

  # Build the successor mapping: for each cluster, chain nodes into a cycle.
  # E.g. cluster with nodes {2, 5, 8} gets cycle 2->5->8->2,
  # so successor[2]=5, successor[5]=8, successor[8]=2.
  # Singleton clusters point to themselves.
  successor <- integer(n)
  cluster_nodes <- split(seq_len(n), cluster)

  for (cl in names(cluster_nodes)) {
    nodes <- cluster_nodes[[cl]]
    k <- length(nodes)
    if (k == 1L) {
      # Singleton cluster: node is its own successor
      successor[nodes] <- nodes
    } else {
      # Cycle: 1st->2nd->...->last->1st
      successor[nodes] <- nodes[c(2:k, 1L)]
    }
  }

  # Compute penalty M if not provided.
  # M must be large enough that the optimal ATSP tour will always use all
  # successor arcs (each worth -M), making it never beneficial to skip one.
  if (is.null(M)) {
    finite_costs <- cost[is.finite(cost)]
    if (length(finite_costs) == 0L) {
      stop("Cannot compute M: no finite values in cost matrix.", call. = FALSE)
    }
    M <- max(abs(finite_costs)) * n + 1
  }

  # Build the transformed ATSP cost matrix.
  # Start with all entries = Inf (forbidden by default).
  atsp_cost <- matrix(Inf, nrow = n, ncol = n)
  for (i in seq_len(n)) {
    si <- successor[i]
    for (j in seq_len(n)) {
      if (cluster[i] == cluster[j]) {
        # Within-cluster: only the successor arc is allowed (cost = -M).
        # All other within-cluster arcs stay Inf (forbidden).
        if (j == si) atsp_cost[i, j] <- -M
      } else {
        # Between-cluster: use original cost from successor(i) to j.
        # This "shift" is what makes the transformation work: the ATSP tour
        # enters a cluster at some node, traverses the successor cycle, and
        # when it exits, the between-cluster cost reflects the entry node.
        atsp_cost[i, j] <- cost[si, j]
      }
    }
  }

  structure(
    list(atsp_cost = atsp_cost, successor = successor, M = M,
         original = x, cluster_nodes = cluster_nodes),
    class = "egtsp_atsp"
  )
}


# -- Tour extraction & objective ----------------------------------------------

#' Extract E-GTSP tour from raw ATSP tour
#'
#' After solving the transformed ATSP, the solution visits all n nodes. This
#' function recovers the E-GTSP tour by walking the ATSP tour and keeping only
#' the first node encountered from each cluster. That first node is the
#' "representative" chosen for that cluster.
#'
#' @param raw_tour Integer vector of node indices (the full ATSP tour).
#' @param atsp An \code{egtsp_atsp} object (from as_atsp()).
#' @return Integer vector of representative nodes (one per cluster, in visit
#'   order).
extract_gtour <- function(raw_tour, atsp) {
  raw <- as.integer(raw_tour)
  cluster <- atsp$original$cluster
  m <- atsp$original$m

  # Track which clusters we've already seen
  seen_clusters <- logical(max(cluster))
  gtour <- integer(m)
  idx <- 0L

  # Walk the ATSP tour node by node
  for (node in raw) {
    cl <- cluster[node]
    if (!seen_clusters[cl]) {
      # First time seeing this cluster: this node is its representative
      seen_clusters[cl] <- TRUE
      idx <- idx + 1L
      gtour[idx] <- node
    }
    # Subsequent nodes from the same cluster are skipped (they're part of
    # the successor cycle, not additional visits)
  }

  if (idx != m) {
    warning("Expected ", m, " clusters in g-tour but found ", idx, ".", call. = FALSE)
  }
  gtour[seq_len(idx)]
}

#' Compute E-GTSP tour cost on original cost matrix
#'
#' Evaluates the total travel cost of a g-tour. The tour is treated as a cycle:
#' the cost from the last node back to the first node is included.
#'
#' Important: this always uses the ORIGINAL cost matrix, not the transformed
#' ATSP costs. The transformed costs include the -M penalties and shifted
#' between-cluster costs, which are not meaningful as a real objective.
#'
#' @param gtour Integer vector of representative nodes (one per cluster).
#' @param x An \code{egtsp} object with the original cost matrix.
#' @return Total tour cost (numeric scalar).
gtour_cost <- function(gtour, x) {
  cost <- x$cost
  gtour <- as.integer(gtour)
  m <- length(gtour)

  # Sum the arc costs along the tour cycle: node[1]->node[2]->...->node[m]->node[1]
  total <- 0
  for (k in seq_len(m)) {
    from <- gtour[k]
    to <- gtour[if (k < m) k + 1L else 1L]  # wrap around to first node
    total <- total + cost[from, to]
  }
  total
}


# -- Solver dispatcher --------------------------------------------------------

#' Solve an E-GTSP instance
#'
#' Main entry point. Validates the problem and delegates to the chosen backend.
#'
#' @param x An \code{egtsp} object (from new_egtsp()).
#' @param backend \code{"TSP"} (heuristic, default) or \code{"MILP"} (exact).
#' @param ... Passed to the backend function. For TSP: method, control.
#' @return An \code{egtsp_solution} object with fields:
#'   backend, raw_tour, gtour, objective, status, diagnostics.
solve_egtsp <- function(x, backend = c("TSP", "MILP"), ...) {
  backend <- match.arg(backend)
  x <- validate_egtsp(x)
  switch(backend,
    TSP  = .solve_egtsp_tsp(x, ...),
    MILP = .solve_egtsp_milp(x, ...)
  )
}


# -- TSP backend (heuristic) --------------------------------------------------
#
# Workflow:
#   1. Transform E-GTSP -> ATSP via Noon-Bean (as_atsp)
#   2. Create a TSP::ATSP object from the transformed cost matrix
#   3. Solve with TSP::solve_TSP() using a heuristic (default: repetitive_nn)
#   4. Extract E-GTSP tour from the raw ATSP tour (extract_gtour)
#   5. Compute objective on the ORIGINAL cost matrix (gtour_cost)

.solve_egtsp_tsp <- function(x, method = "repetitive_nn", control = list(), ...) {
  .egtsp_check_pkg("TSP")

  # Step 1: Transform to ATSP
  atsp <- as_atsp(x, ...)

  # Step 2-3: Create TSP object and solve with chosen heuristic
  tsp_obj <- TSP::ATSP(atsp$atsp_cost)
  raw_tour <- TSP::solve_TSP(tsp_obj, method = method, control = control)
  raw_tour_int <- as.integer(raw_tour)

  # Step 4: Extract one representative node per cluster from the full ATSP tour
  gtour <- extract_gtour(raw_tour_int, atsp)

  # Step 5: Compute cost on the original (not transformed!) cost matrix
  objective <- gtour_cost(gtour, x)

  structure(
    list(backend = "TSP", raw_tour = raw_tour_int, gtour = gtour,
         objective = objective, status = "success",
         diagnostics = list(method = method,
                            atsp_tour_length = TSP::tour_length(raw_tour),
                            n_atsp = nrow(atsp$atsp_cost), M = atsp$M)),
    class = "egtsp_solution"
  )
}


# -- MILP backend (exact) -----------------------------------------------------
#
# Unlike the TSP backend, this formulates the ORIGINAL E-GTSP directly as a
# Mixed Integer Linear Program (no Noon-Bean transformation needed).
#
# Decision variables:
#   y[i]   = 1 if node i is selected as its cluster's representative (binary)
#   a[i,j] = 1 if the tour uses arc from node i to node j (binary)
#   u[i]   = position of node i in the tour (continuous, for subtour elimination)
#
# Objective:
#   Minimize sum of cost[i,j] * a[i,j] over all arcs
#
# Constraints:
#   1. Exactly one node selected per cluster (sum of y[i] = 1 for each cluster)
#   2. Out-degree: each node's outgoing arcs to other clusters equal y[i]
#      (selected nodes have exactly one outgoing arc, unselected have zero)
#   3. In-degree: same logic for incoming arcs
#   4. No arcs within the same cluster (only between-cluster arcs allowed)
#   5. No self-loops
#   6. MTZ subtour elimination: u[i] - u[j] + m*a[i,j] <= m-1 ensures the
#      tour forms a single cycle, not multiple disconnected subtours.
#      Nodes in cluster 1 are fixed at position u=1 as the reference.

.solve_egtsp_milp <- function(x, ...) {
  .egtsp_check_pkg("ompr")
  .egtsp_check_pkg("ompr.roi")
  .egtsp_check_pkg("ROI")
  .egtsp_check_pkg("ROI.plugin.glpk")

  cmat <- x$cost
  cluster <- x$cluster
  n <- x$n
  m <- x$m
  clusters <- sort(unique(cluster))

  # Remap cluster IDs to consecutive 1..m for the MTZ constraints
  cluster_id <- match(cluster, clusters)
  cluster_nodes <- split(seq_len(n), cluster_id)

  # Initialize empty MIP model
  model <- ompr::MIPModel()

  # --- Decision variables ---
  # y[i]: binary, 1 if node i is the selected representative of its cluster
  model <- ompr::add_variable(model, y[i], i = 1:n, type = "binary")
  # a[i,j]: binary, 1 if the tour traverses arc i -> j
  model <- ompr::add_variable(model, a[i, j], i = 1:n, j = 1:n, type = "binary")
  # u[i]: continuous in [1, m], MTZ position variable for subtour elimination
  model <- ompr::add_variable(model, u[i], i = 1:n, type = "continuous", lb = 1, ub = m)

  # --- Objective: minimize total arc cost ---
  model <- ompr::set_objective(model,
    ompr::sum_expr(cmat[i, j] * a[i, j], i = 1:n, j = 1:n), "min")

  # --- Constraint 1: exactly one node selected per cluster ---
  for (cl in seq_len(m)) {
    nodes_cl <- cluster_nodes[[cl]]
    model <- ompr::add_constraint(model, ompr::sum_expr(y[i], i = nodes_cl) == 1)
  }

  # --- Constraint 2: out-degree equals selection status ---
  # If y[ii]=1 (selected), exactly one outgoing arc to another cluster.
  # If y[ii]=0 (not selected), no outgoing arcs.
  for (ii in seq_len(n)) {
    other <- which(cluster_id != cluster_id[ii])
    if (length(other) > 0) {
      model <- ompr::add_constraint(model, ompr::sum_expr(a[ii, j], j = other) == y[ii])
    }
  }

  # --- Constraint 3: in-degree equals selection status ---
  # Same logic as out-degree but for incoming arcs.
  for (jj in seq_len(n)) {
    other <- which(cluster_id != cluster_id[jj])
    if (length(other) > 0) {
      model <- ompr::add_constraint(model, ompr::sum_expr(a[i, jj], i = other) == y[jj])
    }
  }

  # --- Constraint 4: forbid arcs within the same cluster ---
  # The tour only travels between clusters, not within them.
  for (cl in seq_len(m)) {
    nodes_cl <- cluster_nodes[[cl]]
    if (length(nodes_cl) > 1) {
      for (ii in nodes_cl) {
        for (jj in nodes_cl) {
          if (ii != jj) model <- ompr::add_constraint(model, a[ii, jj] == 0)
        }
      }
    }
  }

  # --- Constraint 5: no self-loops ---
  for (ii in seq_len(n)) {
    model <- ompr::add_constraint(model, a[ii, ii] == 0)
  }

  # --- Constraint 6: MTZ subtour elimination ---
  # Fix position u=1 for all nodes in cluster 1 (the "depot" cluster).
  # For arcs between other clusters: u[i] - u[j] + m*a[i,j] <= m-1
  # This forces a single connected tour by requiring positions to increase
  # along the path (except when wrapping back to cluster 1).
  nodes_c1 <- cluster_nodes[[1]]
  for (ii in nodes_c1) {
    model <- ompr::add_constraint(model, u[ii] == 1)
  }

  for (ii in seq_len(n)) {
    for (jj in seq_len(n)) {
      if (ii != jj && cluster_id[ii] != cluster_id[jj] && cluster_id[jj] != 1) {
        model <- ompr::add_constraint(model, u[ii] - u[jj] + m * a[ii, jj] <= m - 1)
      }
    }
  }

  # --- Solve the model ---
  result <- ompr::solve_model(model, ompr.roi::with_ROI(solver = "glpk"))
  solver_status <- ompr::solver_status(result)

  if (!solver_status %in% c("optimal", "success")) {
    warning("MILP solver status: ", solver_status, call. = FALSE)
  }

  # --- Extract solution ---
  # Get which nodes were selected (y[i] > 0.5 due to binary -> use threshold)
  y_val <- ompr::get_solution(result, y[i])
  a_val <- ompr::get_solution(result, a[i, j])
  selected <- y_val$i[y_val$value > 0.5]
  arcs <- a_val[a_val$value > 0.5, , drop = FALSE]

  # Reconstruct the tour order from the selected arcs
  gtour <- .egtsp_build_tour(selected, arcs, cluster_id, m)

  # Compute objective on the original cost matrix
  objective <- gtour_cost(gtour, x)

  structure(
    list(backend = "MILP", raw_tour = gtour, gtour = gtour,
         objective = objective, status = solver_status,
         diagnostics = list(solver = "GLPK", selected_nodes = selected,
                            n_arcs = nrow(arcs))),
    class = "egtsp_solution"
  )
}

#' Reconstruct tour sequence from MILP arc solution
#'
#' Given the selected arcs (`a[i,j] = 1`), build the ordered tour by following
#' the arc chain starting from a node in cluster 1.
#'
#' @param selected Integer vector of selected node indices.
#' @param arcs Data frame with columns i, j (the active arcs).
#' @param cluster_id Integer vector mapping nodes to cluster IDs 1..m.
#' @param m Number of clusters.
#' @return Integer vector of m nodes in tour order.
.egtsp_build_tour <- function(selected, arcs, cluster_id, m) {
  # Build adjacency: for each node, which node comes next in the tour
  next_node <- stats::setNames(arcs$j, arcs$i)

  # Start from the selected node in cluster 1 (the reference cluster)
  start <- selected[cluster_id[selected] == 1]
  start <- if (length(start) == 0L) selected[1] else start[1]

  # Follow the arc chain for m steps to build the full tour
  tour <- integer(m)
  tour[1] <- start
  current <- start
  for (k in 2:m) {
    current <- next_node[[as.character(current)]]
    tour[k] <- current
  }
  tour
}


# -- Print / summary methods --------------------------------------------------

#' @exportS3Method print egtsp
#' @keywords internal
print.egtsp <- function(x, ...) {
  cat("E-GTSP instance\n")
  cat("  Nodes:   ", x$n, "\n")
  cat("  Clusters:", x$m, "\n")
  cat("  Cluster sizes:", paste(table(x$cluster), collapse = ", "), "\n")
  invisible(x)
}

#' @exportS3Method print egtsp_solution
#' @keywords internal
print.egtsp_solution <- function(x, ...) {
  cat("E-GTSP solution\n")
  cat("  Backend:  ", x$backend, "\n")
  cat("  Status:   ", x$status, "\n")
  cat("  Objective:", x$objective, "\n")
  cat("  Tour:     ", paste(x$gtour, collapse = " -> "), "\n")
  invisible(x)
}

#' @exportS3Method summary egtsp_solution
#' @keywords internal
summary.egtsp_solution <- function(object, ...) {
  cat("E-GTSP Solution Summary\n")
  cat(strrep("-", 40), "\n")
  cat("Backend:    ", object$backend, "\n")
  cat("Status:     ", object$status, "\n")
  cat("Objective:  ", object$objective, "\n")
  cat("Tour length:", length(object$gtour), "clusters\n")
  cat("Tour:       ", paste(object$gtour, collapse = " -> "), "\n")
  if (length(object$diagnostics) > 0) {
    cat("\nDiagnostics:\n")
    for (nm in names(object$diagnostics)) {
      val <- object$diagnostics[[nm]]
      cat("  ", nm, ": ", paste(val, collapse = ", "), "\n", sep = "")
    }
  }
  invisible(object)
}
