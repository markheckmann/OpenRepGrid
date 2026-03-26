# CITATION: Bringmann, M. W. (1990). Computer-based methods for the analysis and
#   interpretation of personal construct systems. In G. Neimeyer & R. Neimeyer (Eds.),
#   Advances in Personal Construct Psychology (Vol. 1, pp. 57-90). JAI Press.
#   Grid data originally from Bell (1987) G-PACK program example.
#
# CONTEXT: This example grid labelled "Friends and Others" was used to illustrate
#   computer-based grid analysis methods; six anonymized persons (S1-S6) are rated
#   on eight bipolar constructs on a 1-7 scale.

args <- list(
  name = c("S1", "S2", "S3", "S4", "S5", "S6"),
  l.name = c("Willingness to listen",
             "Professional outlook",
             "Stubborn minded",
             "Ability to twist facts",
             "Willpower",
             "Friendly",
             "Unhealthy lifestyle",
             "Competitive"),
  r.name = c("Unwilling to listen",
             "Slapdash approach",
             "Relaxed attitude",
             "Honest",
             "Commitment difficulty",
             "Cold to others",
             "Conscious of health",
             "Not competitive"),
  scores = c(7, 6, 3, 4, 6, 6,
             6, 6, 3, 4, 5, 6,
             6, 6, 6, 3, 4, 4,
             3, 2, 5, 6, 2, 2,
             3, 6, 2, 5, 5, 5,
             5, 4, 4, 5, 5, 5,
             2, 4, 4, 2, 5, 2,
             6, 5, 3, 6, 5, 6)
)
grid <- makeRepgrid(args)
grid <- setScale(grid, 1, 7)
