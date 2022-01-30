## -----------------------------------------------------------------------------
library(SME)
library(knitr)

## -----------------------------------------------------------------------------
df = data.frame("V1" = 1:5, "V2" = c("a", "b", "c", "d", "e"), "V3" = c("alto", "bajo", "bajo", "alto", "alto"))

## -----------------------------------------------------------------------------
write.df(df, "bench1.txt")
r <- read.df(file.path = "bench1.txt")
kable(r)

## -----------------------------------------------------------------------------
write.df(df, "bench2.txt", header = TRUE)
r <- read.df(file.path = "bench2.txt", header = TRUE)
kable(r)

## -----------------------------------------------------------------------------
write.df(df, "bench2.txt", append = TRUE)
r <- read.df(file.path = "bench2.txt", header = FALSE)
kable(r)

## -----------------------------------------------------------------------------
write.df(df, "bench2.txt", sep = ";", header = FALSE, append = TRUE)
r <- read.df(file.path = "bench2.txt", sep = ";", header = TRUE,  max.n.factor = 3, row.names = c('X1', 'X2', 'X3', 'X4', 'X5'), col.names = c('A', 'B', 'C'), n.rows = 5, transpose = FALSE)
kable(r)

