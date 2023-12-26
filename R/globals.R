# Avoid 'no visible binding for global variable' warnings in R CMD CHECK This code comes from a
# contributor. We might need to have a look to avoid these warnings all togther. This is a quick fix
# for now.
utils::globalVariables(c(
  "Self", "Ideal", "Construct", "Difference", "id_c", "id_d", "Dilemmatic",
  "Polarization", "name", "Classification"
))
