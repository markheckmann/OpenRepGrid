# ============================================================================ #
#                          Tests for FOCUS algorithm                           #
# ============================================================================ #


# --- Helper: small test grid ------------------------------------------------

make_test_grid <- function() {
  # 3 constructs, 4 elements, scale 1-5
  # Construct 3 is a reversed version of construct 1
  makeRepgrid(list(
    name = paste0("E", 1:4),
    l.name = c("A", "C", "E"),
    r.name = c("B", "D", "F"),
    scores = c(
      1, 2, 4, 5,
      3, 3, 4, 2,
      5, 4, 2, 1
    ),
    min = 1, max = 5
  ))
}


# --- Construct matching scores -----------------------------------------------

test_that(".focus_construct_matching_scores works for hand-calculated example", {
  # 3 constructs, 4 elements, scale 1-5
  R <- matrix(c(
    1, 2, 4, 5,
    3, 3, 4, 2,
    5, 4, 2, 1
  ), nrow = 3, byrow = TRUE)

  res <- OpenRepGrid:::.focus_construct_matching_scores(R, 1, 5)

  # Diagonal should be 100
  expect_equal(unname(diag(res$scores)), rep(100, 3))

  # Construct 1 vs 2 (original):
  # d_orig = |1-3| + |2-3| + |4-4| + |5-2| = 2+1+0+3 = 6
  # ms_orig = (-200*6) / (4*4) + 100 = -1200/16 + 100 = -75 + 100 = 25
  # d_rev (reversed c2: 5+1-3=3, 5+1-3=3, 5+1-4=2, 5+1-2=4):
  # d_rev = |1-3| + |2-3| + |4-2| + |5-4| = 2+1+2+1 = 6
  # ms_rev = (-200*6)/(4*4) + 100 = 25
  # Same score, so no reversal needed (ms_rev not strictly greater)
  expect_equal(res$scores[1, 2], 25)
  expect_false(res$needs_reversal[1, 2])

  # Construct 1 vs 3 (original):
  # d_orig = |1-5| + |2-4| + |4-2| + |5-1| = 4+2+2+4 = 12
  # ms_orig = (-200*12) / (4*4) + 100 = -2400/16 + 100 = -150+100 = -50
  # d_rev (reversed c3: 5+1-5=1, 5+1-4=2, 5+1-2=4, 5+1-1=5):
  # d_rev = |1-1| + |2-2| + |4-4| + |5-5| = 0
  # ms_rev = (-200*0)/(4*4) + 100 = 100
  # ms_rev > ms_orig, so reversal flagged
  expect_equal(res$scores[1, 3], 100)
  expect_true(res$needs_reversal[1, 3])

  # Matrix should be symmetric
  expect_equal(res$scores[1, 2], res$scores[2, 1])
  expect_equal(res$scores[1, 3], res$scores[3, 1])
})


test_that("construct matching scores are in [-100, 100]", {
  x <- bell2010
  R <- getRatingLayer(x, names = FALSE)
  sc <- getScale(x)
  res <- OpenRepGrid:::.focus_construct_matching_scores(R, sc["min"], sc["max"])

  expect_true(all(res$scores >= -100))
  expect_true(all(res$scores <= 100))
})


# --- Element matching scores -------------------------------------------------

test_that(".focus_element_matching_scores works", {
  R <- matrix(c(
    1, 2, 4, 5,
    3, 3, 4, 2
  ), nrow = 2, byrow = TRUE)

  res <- OpenRepGrid:::.focus_element_matching_scores(R, 1, 5)

  # Diagonal should be 100
  expect_equal(unname(diag(res)), rep(100, 4))

  # Element 1 vs 2:
  # d = |1-2| + |3-3| = 1+0 = 1
  # ms = (-100*1) / (4*2) + 100 = -100/8 + 100 = -12.5 + 100 = 87.5
  expect_equal(res[1, 2], 87.5)

  # Element scores in [0, 100]
  expect_true(all(res >= 0))
  expect_true(all(res <= 100))

  # Symmetric
  expect_equal(res[1, 2], res[2, 1])
})


test_that("element matching scores are in [0, 100]", {
  x <- bell2010
  R <- getRatingLayer(x, names = FALSE)
  sc <- getScale(x)
  res <- OpenRepGrid:::.focus_element_matching_scores(R, sc["min"], sc["max"])

  expect_true(all(res >= 0))
  expect_true(all(res <= 100))
})


# --- Chain cluster -----------------------------------------------------------

test_that(".focus_chain_cluster returns correct structure", {
  # Simple 3x3 score matrix
  scores <- matrix(c(
    100, 80, 20,
     80, 100, 60,
     20,  60, 100
  ), nrow = 3, byrow = TRUE)

  res <- OpenRepGrid:::.focus_chain_cluster(scores)

  expect_type(res$chain_order, "integer")
  expect_length(res$chain_order, 3)
  # All items should appear exactly once
  expect_setequal(res$chain_order, 1:3)
  # Merges should have expected columns
  expect_true(all(c("step", "item1", "item2", "score") %in% names(res$merges)))
})


test_that(".focus_chain_cluster chains highest-scoring pair first", {
  scores <- matrix(c(
    100, 90, 10,
     90, 100, 10,
     10,  10, 100
  ), nrow = 3, byrow = TRUE)

  res <- OpenRepGrid:::.focus_chain_cluster(scores)

  # Items 1 and 2 have highest score (90), so should be adjacent in chain
  pos1 <- which(res$chain_order == 1)
  pos2 <- which(res$chain_order == 2)
  expect_equal(unname(abs(pos1 - pos2)), 1)
})


test_that(".focus_chain_cluster handles n=1", {
  scores <- matrix(100, 1, 1)
  res <- OpenRepGrid:::.focus_chain_cluster(scores)
  expect_equal(res$chain_order, 1L)
  expect_equal(nrow(res$merges), 0)
})


test_that(".focus_chain_cluster handles n=2", {
  scores <- matrix(c(100, 75, 75, 100), 2, 2)
  res <- OpenRepGrid:::.focus_chain_cluster(scores)
  expect_setequal(res$chain_order, 1:2)
  expect_equal(nrow(res$merges), 1)
  expect_equal(res$merges$score[1], 75)
})


# --- Main focus() function ---------------------------------------------------

test_that("focus() with grid_only=TRUE returns repgrid", {
  res <- focus(bell2010)

  expect_true(is.repgrid(res))
  expect_equal(getNoOfConstructs(res), getNoOfConstructs(bell2010))
  expect_equal(getNoOfElements(res), getNoOfElements(bell2010))
})


test_that("focus() with grid_only=FALSE returns focus object", {
  res <- focus(bell2010, grid_only = FALSE)

  expect_s3_class(res, "focus")
  expect_true(is.repgrid(res$grid))
  expect_equal(getNoOfConstructs(res$grid), getNoOfConstructs(bell2010))
  expect_equal(getNoOfElements(res$grid), getNoOfElements(bell2010))
})


test_that("focus() runs on boeker without error", {
  res <- focus(boeker, grid_only = FALSE)

  expect_s3_class(res, "focus")
  expect_true(is.repgrid(res$grid))
  expect_equal(getNoOfConstructs(res$grid), getNoOfConstructs(boeker))
  expect_equal(getNoOfElements(res$grid), getNoOfElements(boeker))
})


test_that("focus() detects reversed constructs", {
  x <- make_test_grid()
  res <- focus(x, grid_only = FALSE)

  # Construct 3 is a reversed version of construct 1 (perfect match = 100)
  expect_s3_class(res, "focus")
  cm <- res$construct_matching
  expect_equal(cm[1, 3], 100) # Perfect match after considering reversal
})


test_that("focus() errors on NA ratings", {
  # Use bell2010 and introduce NA via direct slot manipulation
  x <- bell2010
  x@ratings[1, 1, 1] <- NA
  expect_error(focus(x), "no NA")
})


test_that("focus() errors on missing scale", {
  x <- bell2010
  x@scale$min <- NA
  expect_error(focus(x), "scale")
})


test_that("focus() matching score ranges are correct", {
  res <- focus(bell2010, grid_only = FALSE)

  # Construct scores in [-100, 100]
  expect_true(all(res$construct_matching >= -100))
  expect_true(all(res$construct_matching <= 100))

  # Element scores in [0, 100]
  expect_true(all(res$element_matching >= 0))
  expect_true(all(res$element_matching <= 100))
})


test_that("focus() chain orders contain all indices", {
  res <- focus(bell2010, grid_only = FALSE)

  nc <- getNoOfConstructs(bell2010)
  ne <- getNoOfElements(bell2010)

  expect_setequal(res$construct_chain_order, seq_len(nc))
  expect_setequal(res$element_chain_order, seq_len(ne))
})


test_that("focus() handles 2x2 grid", {
  x <- makeRepgrid(list(
    name = paste0("E", 1:2),
    l.name = c("A", "B"),
    r.name = c("C", "D"),
    scores = c(1, 3, 4, 2),
    min = 1, max = 5
  ))
  res <- focus(x, grid_only = FALSE)
  expect_s3_class(res, "focus")
  expect_equal(getNoOfConstructs(res$grid), 2)
  expect_equal(getNoOfElements(res$grid), 2)
})


test_that("focus() handles identical constructs", {
  x <- makeRepgrid(list(
    name = paste0("E", 1:3),
    l.name = c("A", "A"),
    r.name = c("B", "B"),
    scores = c(1, 2, 3, 1, 2, 3),
    min = 1, max = 5
  ))
  res <- focus(x, grid_only = FALSE)
  expect_s3_class(res, "focus")
  # Identical constructs should have matching score of 100
  expect_equal(res$construct_matching[1, 2], 100)
})


# --- Validation against Jankowicz & Thomas (1982) worked example -----------

test_that("focus() reproduces Jankowicz & Thomas (1982) element results", {
  # Table I raw grid: 5 constructs, 5 elements (JH, RD, PP, EA, GB), scale 1-5
  # Grid values verified against paper's Table II element difference sums
  # and Table VIII construct difference sums
  x <- makeRepgrid(list(
    name = c("JH", "RD", "PP", "EA", "GB"),
    l.name = c("Effective admin", "Task orientated", "Specialist", "Innovator", "Safety net"),
    r.name = c("Less effective", "People orientated", "General mgmt", "Consolidator", "No safety net"),
    scores = c(
      1, 1, 5, 3, 1,
      4, 5, 2, 1, 2,
      1, 3, 5, 1, 1,
      5, 1, 5, 5, 4,
      1, 1, 5, 3, 2
    ),
    min = 1, max = 5
  ))

  res <- focus(x, grid_only = FALSE)

  # --- Element difference sums (Table II) ---
  # Verify a few entries from the paper's Table II
  R <- getRatingLayer(x, names = FALSE)
  sc <- getScale(x)
  em_raw <- OpenRepGrid:::.focus_element_matching_scores(R, sc["min"], sc["max"])

  # Paper formula: % matching score = [(-100 * DS) / (c * (n-1))] + 100
  # where c = 5 constructs, n = 5 (max rating), so c*(n-1) = 20
  # JH-RD: DS=7, ms = (-100*7)/20 + 100 = 65
  expect_equal(em_raw[1, 2], 65)
  # JH-GB: DS=4, ms = (-100*4)/20 + 100 = 80
  expect_equal(em_raw[1, 5], 80)
  # EA-GB: DS=5, ms = (-100*5)/20 + 100 = 75
  expect_equal(em_raw[4, 5], 75)
  # PP-GB: DS=12, ms = (-100*12)/20 + 100 = 40
  expect_equal(em_raw[3, 5], 40)

  # --- Element chain order (from paper) ---
  # Paper result: RD-(7)-JH-(4)-GB-(5)-EA-(9)-PP
  # i.e. element order: RD=2, JH=1, GB=5, EA=4, PP=3
  expect_equal(unname(res$element_chain_order), c(2L, 1L, 5L, 4L, 3L))
})


test_that("focus() reproduces Jankowicz & Thomas (1982) construct results", {
  x <- makeRepgrid(list(
    name = c("JH", "RD", "PP", "EA", "GB"),
    l.name = c("Effective admin", "Task orientated", "Specialist", "Innovator", "Safety net"),
    r.name = c("Less effective", "People orientated", "General mgmt", "Consolidator", "No safety net"),
    scores = c(
      1, 1, 5, 3, 1,
      4, 5, 2, 1, 2,
      1, 3, 5, 1, 1,
      5, 1, 5, 5, 4,
      1, 1, 5, 3, 2
    ),
    min = 1, max = 5
  ))

  res <- focus(x, grid_only = FALSE)

  # --- Construct matching scores ---
  # Paper formula: ms = (-200*DS)/(e*(n-1)) + 100
  # where e = 5 elements, n = 5 (max rating), so e*(n-1) = 20
  cm <- res$construct_matching

  # C1 vs C5: UU DS=1, ms = (-200*1)/20+100 = 90; UR DS=19, ms=-90. Best=90
  expect_equal(cm[1, 5], 90)

  # C1 vs C3: UU DS=4, ms = (-200*4)/20+100 = 60; UR DS=16, ms=-60. Best=60
  expect_equal(cm[1, 3], 60)

  # C5 vs C2: UU DS=12, ms=-20; UR DS=6, ms = (-200*6)/20+100 = 40. Best=40
  expect_equal(cm[2, 5], 40)

  # C2 vs C4: UU DS=14, ms=-40; UR DS=4, ms = (-200*4)/20+100 = 60. Best=60
  expect_equal(cm[2, 4], 60)

  # --- Construct chain order (from paper) ---
  # Paper result: C3-(60%UR)-C1-(90%UR)-C5-(40%R)-C2-(60%R)-C4
  expect_equal(unname(res$construct_chain_order), c(3L, 1L, 5L, 2L, 4L))

  # --- Reversed constructs ---
  # Paper Table XIV version A: C3=UR, C1=UR, C5=UR, C2=R, C4=UR
  # So only C2 is reversed
  expect_equal(unname(res$reversed_constructs), 2L)
})


test_that("avg_adjacent scores show improvement", {
  res <- focus(bell2010, grid_only = FALSE)
  aa <- res$avg_adjacent

  # Focused ordering should be >= original ordering
  expect_gte(aa$constructs_focused, aa$constructs_original)
  expect_gte(aa$elements_focused, aa$elements_original)

  # All values should be numeric scalars
  expect_length(aa$constructs_focused, 1)
  expect_length(aa$elements_focused, 1)
})


test_that("print.focus() runs without error", {
  res <- focus(bell2010, grid_only = FALSE)
  expect_output(print(res), "FOCUS")
  expect_output(print(res), "Average adjacent matching scores")
})
