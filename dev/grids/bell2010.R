# CITATION: Bell, R. C. (2010). A note on aligning constructs. Personal Construct Theory
#   & Practice, 7, 42-48. Grid data originally from: Haritos, A., Gindidis, A., Doan, C.,
#   & Bell, R. C. (2004). The effect of element role titles on construct dimensionality.
#   Journal of Constructivist Psychology, 17, 109-124.
#
# CONTEXT: This grid (Figure 3 in Bell, 2010) was used to illustrate construct alignment
#   methods; it comprises ratings by one participant on 10 role-based elements (self, friend,
#   mother, teachers, etc.) across 9 bipolar constructs on a 1-7 scale.

args <- list(
  name = c("self",
           "closest friend same sex",
           "unhappiest person you know",
           "opp. sex - don't get along",
           "opp. sex - like more than dislike",
           "mother",
           "teacher you respected",
           "person you work well with",
           "teacher you did not respect",
           "most confident person you know"),
  l.name = c("relaxed",
             "not so smart (academically)",
             "dislikes sport",
             "not interactive",
             "not transparent",
             "insensitive",
             "fearful & timid",
             "rough",
             "accept as it is"),
  r.name = c("worried & tense",
             "smart (academically)",
             "loves sports",
             "loves people",
             "transparent",
             "sensitive",
             "fearless",
             "gentle",
             "loves to argue"),
  scores = c(4, 4, 6, 5, 3, 6, 5, 2, 2, 6,
             6, 3, 7, 6, 4, 6, 7, 4, 7, 3,
             6, 7, 6, 4, 4, 2, 6, 5, 6, 3,
             6, 7, 5, 6, 6, 5, 6, 7, 7, 4,
             6, 4, 5, 7, 3, 7, 6, 5, 3, 3,
             4, 6, 3, 4, 6, 5, 3, 2, 4, 5,
             5, 4, 4, 3, 5, 3, 5, 6, 3, 5,
             5, 6, 6, 4, 5, 7, 7, 3, 5, 6,
             5, 5, 6, 7, 4, 4, 6, 7, 5, 5)
)
grid <- makeRepgrid(args)
grid <- setScale(grid, 1, 7)
