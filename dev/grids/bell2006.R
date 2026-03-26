# CITATION: Bell, R. C. (2006). A note on the correlation of elements in repertory grids:
#   How to and why. Journal of Constructivist Psychology, 19(3), 273-279.
#   DOI: 10.1080/10720530600523595
#
# CONTEXT: This illustrative grid (Figure 2) was constructed to demonstrate statistical
#   methods for correlating elements in repertory grids, showing two participants (A and B)
#   each with a self and ideal-self element rated on six bipolar constructs using a 1-7 scale.

args <- list(
  name = c("A-self", "A-ideal", "B-self", "B-ideal"),
  l.name = c("Soft", "Warm", "Weak", "Bold", "Headstrong", "Fast"),
  r.name = c("Hard", "Cool", "Strong", "Timid", "Cautious", "Slow"),
  scores = c(1, 5, 3, 7,
             1, 3, 1, 5,
             3, 5, 2, 6,
             2, 2, 7, 3,
             1, 7, 6, 2,
             1, 7, 5, 1)
)
grid <- makeRepgrid(args)
grid <- setScale(grid, 1, 7)
