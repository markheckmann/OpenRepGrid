id <- indexDilemma(boeker, self = 1, ideal = 2)
id

# adjust minimal correlation
indexDilemma(boeker, self = 1, ideal = 2, r.min = .5)

# adjust congruence and discrepance ranges
indexDilemma(boeker, self = 1, ideal = 2, diff.congruent = 0, diff.discrepant = 4)

# print options (see ?print.indexDilemma for help)
print(id, output = "D")   # dilemmas only
print(id, output = "OD")  # overview and dilemmas

# plot dilemmas as network graph (see ?plot.indexDilemma for help)
# set a seed for reproducibility
plot(id, layout = "rows")
plot(id, layout = "circle")
plot(id, layout = "star")

