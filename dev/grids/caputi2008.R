# CITATION: Caputi, P., & Hennessy, D. (2008). Using formal concept analysis to analyse
#   repertory grid data. In F. Fransella (Ed.), International Handbook of Personal Construct
#   Psychology (2nd ed., pp. 162-172). Wiley.
#
# CONTEXT: This grid was elicited from a 57-year-old man with approximately ten tattoos
#   as part of a study demonstrating formal concept analysis applied to repertory grid data;
#   twelve elements related to self and others (tattooed/non-tattooed, conforming/non-conforming)
#   were rated on twelve bipolar constructs using a 1-5 scale.

args <- list(
  name = c("Self",
           "Ideal self",
           "Future self",
           "Self as I'd like others to see me",
           "Self as seen by others",
           "Self before I had a tattoo",
           "Self now no tattoo",
           "Someone without a tattoo",
           "Someone with a tattoo",
           "Someone who conforms",
           "Someone who pushes boundaries",
           "Someone I don't like the look of"),
  l.name = c("Happy",
             "Patient",
             "Caring",
             "Kind",
             "Sense of humour",
             "Sociable",
             "Friendly",
             "Active",
             "Confident",
             "Organised",
             "Hard working",
             "Honest"),
  r.name = c("Miserable",
             "Noxious",
             "Neutral",
             "Callous",
             "Dour",
             "Hermit-like",
             "Aloof",
             "Dead beats",
             "Weak",
             "Shambles",
             "Lazy",
             "Slimy"),
  scores = c(2, 2, 2, 2, 2, 1, 3, 2, 1, 5, 1, 5,
             2, 1, 2, 2, 2, 3, 2, 3, 4, 5, 2, 5,
             5, 2, 3, 1, 2, 5, 1, 1, 2, 5, 1, 4,
             2, 2, 2, 1, 2, 2, 2, 1, 2, 5, 2, 3,
             1, 1, 1, 1, 1, 3, 2, 2, 2, 5, 2, 5,
             5, 3, 5, 5, 5, 4, 5, 1, 1, 2, 1, 4,
             2, 2, 2, 2, 5, 2, 2, 2, 1, 4, 2, 4,
             1, 1, 2, 1, 1, 1, 1, 1, 2, 1, 1, 4,
             1, 1, 1, 1, 1, 4, 2, 2, 2, 3, 1, 5,
             1, 1, 1, 2, 3, 3, 2, 5, 4, 2, 1, 4,
             2, 2, 3, 1, 2, 2, 2, 1, 2, 1, 1, 4,
             1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 2, 4)
)
grid <- makeRepgrid(args)
grid <- setScale(grid, 1, 5)
