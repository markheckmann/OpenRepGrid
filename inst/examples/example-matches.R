m <- matches(boeker)

# several output options
print(m, index = FALSE, names = FALSE, upper = FALSE)
print(m, output = "C")  # construct matches
print(m, output = "E")  # element matches

# extract the matrices
m$constructs
m$elements

