## -----------------------------------------------------------------------------
library(SME)
library(knitr)

## -----------------------------------------------------------------------------
example1 <- data.frame("V1" = rep(1, 100), "V2" = 100:1,  "V4" = sample.int(3000,100,replace=FALSE))
example2 <- rep(1, 100)

## -----------------------------------------------------------------------------
v <- variance(example1)
kable(v)
v <- variance(example1, margin = 1L)
print(v)
v <- variance(example2)
kable(v)

## -----------------------------------------------------------------------------
col1 <-  c('False', 'False', 'True', 'True', 'True', 'True', 'False', 'True', 'False', 'False')
col2 <- c('True', 'False', 'False', 'False', 'True', 'True', 'False', 'False', 'False', 'False')
col3 <- c('a', 'b', 'c', 'a', 'a', 'a', 'l', 'a', 'l', 'a')
df <- data.frame("V1" = col1, "V2" = col2, "V3" = col3)

## -----------------------------------------------------------------------------
e <- entropy(df)
kable(e)

## -----------------------------------------------------------------------------
df <- data.frame("V1" = sample.int(2,5,replace=TRUE), "V2" =sample.int(2,5,replace=TRUE) , "V3" = sample.int(2,5,replace=TRUE), "V4" = sample.int(2,5,replace=TRUE),  "V5" = sample.int(2,5,replace=TRUE), "V6" = sample.int(2,5,replace=TRUE), "V7" = sample.int(2,5,replace=TRUE), "V8" = sample.int(2,5,replace=TRUE))
x <- sample.int(2, 5,replace=TRUE)
y <- sample.int(2,5,replace=TRUE)

## -----------------------------------------------------------------------------
c <- conditional.entropy(df, file ="conditionalEntropy")
kable(c)

## -----------------------------------------------------------------------------
c <- conditional.entropy(x = x, y = y)
kable(c)

## -----------------------------------------------------------------------------
df <- data.frame("V1" = sample.int(2,5,replace=TRUE), "V2" =sample.int(2,5,replace=TRUE) , "V3" = sample.int(2,5,replace=TRUE), "V4" = sample.int(2,5,replace=TRUE),  "V5" = sample.int(2,5,replace=TRUE), "V6" = sample.int(2,5,replace=TRUE), "V7" = sample.int(2,5,replace=TRUE), "V8" = sample.int(2,5,replace=TRUE))
x <- sample.int(2, 5,replace=TRUE)
y <- sample.int(2, 5,replace=TRUE)

## -----------------------------------------------------------------------------
j <- joint.entropy(df, file ="jointEntropy")
kable(j)

## -----------------------------------------------------------------------------
j <- joint.entropy(x = x, y = y)
kable(j)

## -----------------------------------------------------------------------------
df <- data.frame("V1" = sample.int(2,5,replace=TRUE), "V2" =sample.int(2,5,replace=TRUE) , "V3" = sample.int(2,5,replace=TRUE), "V4" = sample.int(2,5,replace=TRUE),  "V5" = sample.int(2,5,replace=TRUE), "V6" = sample.int(2,5,replace=TRUE), "V7" = sample.int(2,5,replace=TRUE), "V8" = sample.int(2,5,replace=TRUE))
x <- sample.int(2, 5,replace=TRUE)
y <- sample.int(2, 5,replace=TRUE)

## -----------------------------------------------------------------------------
m <- mutual.information(df, file ="mutualInformation")
kable(m)

## -----------------------------------------------------------------------------
m <- mutual.information(x = x, y = y)
kable(m)

## -----------------------------------------------------------------------------
df <- data.frame("V1" = sample.int(3000,100,replace=FALSE), "V2" =sample.int(3000,100,replace=FALSE) , "V3" = sample.int(3000,100,replace=FALSE), "V4" = sample.int(3000,100,replace=FALSE),  "V5" = sample.int(3000,100,replace=FALSE), "V6" = sample.int(3000,100,replace=FALSE), "V7" = sample.int(3000,100,replace=FALSE), "V8" = sample.int(3000,100,replace=FALSE))
x <- c(35,23,47, 17, 10, 43, 9, 6, 28)
y <- c(30, 33, 45, 23, 8, 49, 12, 4, 31)

## -----------------------------------------------------------------------------
correlation(x = x, y = y, method = "Pearson")
correlation(x = x, y = y, method = "Spearman")
correlation(x = df, method = "Pearson")
correlation(x = df, method = "Spearman")

## -----------------------------------------------------------------------------
correlation(x = df, method = "Pearson", file = "pearsonCor")

## -----------------------------------------------------------------------------
correlation(x = df, method = "Spearman", file = "spearmanCor")

## -----------------------------------------------------------------------------
random.x <- sample.int(3000,1000,replace=FALSE)
class <- c(rep(TRUE, 500), rep(FALSE, 500))
pos <- sample.int(1000, 1000)
random.y <- class[order(pos)]
perfect.x <- 1:3000
perfect.y <- c(rep(TRUE, 1500), rep(FALSE, 1500))

worst.x <- 1:3000
worst.y <- c(rep(FALSE, 1500), rep(TRUE, 1500))

## -----------------------------------------------------------------------------
randomRocCurve <- roc.curve(random.x, random.y, file= "randomRocCurve")

## -----------------------------------------------------------------------------
perfectRocCurve <- roc.curve(perfect.x, perfect.y, file= "perfectRocCurve")

## -----------------------------------------------------------------------------
worstRocCurve <- roc.curve(worst.x, worst.y, file= "worstRocCurve")

## -----------------------------------------------------------------------------
class <- c(rep(TRUE, 5), rep(FALSE, 5))
pos <- sample.int(10, 10)
random.y <- class[order(pos)]
df.numeric <- data.frame("V1" = sample.int(3,10,replace=TRUE), "V2" = sample.int(3,10,replace=TRUE), "V3" = sample.int(3,10,replace=TRUE), "Class" =  random.y)
df.cat <- data.frame("V1" = as.factor(sample.int(3,10,replace=TRUE)), "V2" =  as.factor(sample.int(3,10,replace=TRUE)),  "V4" =  as.factor(sample.int(3,10,replace=TRUE)), "Class" =  random.y)

## -----------------------------------------------------------------------------
filter(df.numeric, class = "Class", by = "AUC", uplimit = 1, lowlimit = 0.5)
filter(df.numeric, class = "Class", by = "Variance", uplimit = 100, lowlimit = 0)
filter(df.cat, class = "Class", by = "Entropy", uplimit = 2, lowlimit = 0)
filter(df.cat, class = "Class", by = "Entropy")

## -----------------------------------------------------------------------------
df <- data.frame("V1" = sample.int(2,5,replace=TRUE), "V2" =sample.int(2,5,replace=TRUE) , "V3" = sample.int(2,5,replace=TRUE), "V4" = sample.int(2,5,replace=TRUE),  "V5" = sample.int(2,5,replace=TRUE), "V6" = sample.int(2,5,replace=TRUE), "V7" = sample.int(2,5,replace=TRUE), "V8" = sample.int(2,5,replace=TRUE))
ce <- conditional.entropy(df)
je <- joint.entropy(df)
mi <- mutual.information(df)

## -----------------------------------------------------------------------------
p <- visualize.infotheo.matrix(ce, type = "Conditional Entropy", colors = c("#692be2", "White", "#ddcbff"))
print(p)

p <- visualize.infotheo.matrix(je, type = "Joint Entropy", colors = c("#9999ff", "#6b7db3", "#6bb3b3"))
print(p)

p <- visualize.infotheo.matrix(je, type = "Mutual Information", colors = c("#ffd699", "White", "#b3966b"))
print(p)

