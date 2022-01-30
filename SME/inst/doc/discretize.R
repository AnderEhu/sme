## -----------------------------------------------------------------------------
library(SME)
library(knitr)

## -----------------------------------------------------------------------------
v.example.1 <- c(11.5, 10.2, 1.2, 0.5, 5.3, 20.5, 8.4)
v.example.2 <- c(0,4,12,16,16,18,24,26,28)
df.example.3 <- data.frame(c(0,4,12), c(16,16,18), c(24,26,28))
matrix.example.4 <- matrix(c(0,4,12,16,16,18,24,26,28), 3, 3, byrow = FALSE )

## -----------------------------------------------------------------------------
ew.discretize.example1 <- discretize(v.example.1, method="interval", 4)
print(ew.discretize.example1)

ew.discretize.example2 <- discretize(v.example.2, method="interval", 3)
print(ew.discretize.example2)

ew.discretize.example3 <- discretize(df.example.3, method="interval", 3)
kable(ew.discretize.example3)

ew.discretize.example4 <- discretize(matrix.example.4, method="interval", 2)
kable(ew.discretize.example4)

ef.discretize.example1 <- discretize(v.example.1, method="frequency", 4)
print(ef.discretize.example1)

ef.discretize.example2 <- discretize(v.example.2, method="frequency", 3)
print(ef.discretize.example2)

ef.discretize.example3 <- discretize(df.example.3, method="frequency", 3)
kable(ef.discretize.example3)

ef.discretize.example4 <- discretize(matrix.example.4, method="frequency", 2)
kable(ef.discretize.example4)


clustering.discretize.example1 <- discretize(v.example.1, method="clustering", 4)
print(clustering.discretize.example1)

clustering.discretize.example2 <- discretize(v.example.2, method="clustering", 3)
print(clustering.discretize.example2)

clustering.discretize.example3 <- discretize(df.example.3, method="clustering", 3) 
kable(clustering.discretize.example3)

clustering.discretize.example4 <- discretize(matrix.example.4, method="clustering", 2)   
kable(clustering.discretize.example4)

