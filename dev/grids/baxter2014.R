# CITATION: Baxter, D. I., Goffin, K., & Szwejczewski, M. (2014). The repertory grid technique
#   as a customer insight method. Research-Technology Management, 57(4), 35-42.
#   DOI: 10.5437/08956308X5704229
#
# CONTEXT: This grid was elicited from a clinician working in intensive care as part of
#   a new product development study to uncover customers' hidden needs regarding medical
#   monitoring devices. The six elements (A-F) are anonymized patient monitoring devices
#   rated on a 1 (emergent pole) to 5 (contrast pole) scale across six constructs.

args <- list(
  name = c("A", "B", "C", "D", "E", "F"),
  l.name = c("Easy to use",
             "Clear display",
             "Quick set-up (while with patient)",
             "Many parameters",
             "Easy staff training",
             "Easy to clean"),
  r.name = c("Difficult",
             "Poor display",
             "Slow set-up",
             "Few parameters",
             "Hard staff training",
             "Hard to clean"),
  scores = c(1, 3, 3, 4, 2, 1,
             1, 4, 5, 3, 4, 4,
             2, 5, 3, 1, 1, 1,
             4, 2, 1, 2, 1, 1,
             3, 3, 5, 4, 5, 5,
             5, 5, 5, 4, 5, 5)
)
grid <- makeRepgrid(args)
grid <- setScale(grid, 1, 5)
