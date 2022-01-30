col.variance <- function(x){
  x.mean <- mean(x)
  sum((x-x.mean)**2) / (length(x)-1)
}



#' Variance
#' 
#' @param x a numeric vector, matrix, or data.frame.
#' @param margin In case of matrix or dataframe apply variance by columns margin = 2L or by rows margin = 1L. Default 2.
#' 
#' 
#' 
#' @return
#' the sample variance of the input.
#' 
#' @examples
#' example1 <- data.frame("V1" = rep(1, 100), "V2" = 100:1,  "V4" = sample.int(3000,100,replace=FALSE))
#' example2 <- rep(1, 100)
#' 
#' variance(example1)
#' variance(example1, margin = 1L)
#' variance(example2)


variance <- function(x, margin = 2L){
  if (is.data.frame(x) | is.matrix(x)){
      apply(x, margin,col.variance)
  }else if (is.vector(x) && is.atomic(x)) {
    col.variance(x)
  }else{
    stop("Not supported data type")
  }
}





col.entropy <- function(x) {
  x.p <- table(x) / length(x)
  x.entropy <- -sum(x.p * log2(x.p))
  return(x.entropy)
}
#' Entropy
#' 
#' @param x a numeric vector, matrix, or data.frame.
#' @param margin In case of matrix or dataframe apply entropy by columns margin = 2L or by rows margin = 1L. Default 2.
#' 
#' 
#' 
#' @return
#' the sample entropy of the input.
#' 
#' @examples
#' col1 <-  c('False', 'False', 'True', 'True', 'True', 'True', 'False', 'True', 'False', 'False')
#' col2 <- c('True', 'False', 'False', 'False', 'True', 'True', 'False', 'False', 'False', 'False')
#' col3 <- c('a', 'b', 'c', 'a', 'a', 'a', 'l', 'a', 'l', 'a')
#' df <- data.frame("V1" = col1, "V2" = col2, "V3" = col3)
#' entropy(df)


entropy <- function(x, margin = 2L){
  if (is.data.frame(x) | is.matrix(x)){
      apply(x, margin,col.entropy)
  }else if (is.vector(x) && is.atomic(x)) {
    col.entropy(x)
  }else{
    stop("Not supported data type")
  }

}


two.var.joint.entropy <- function(x, y){
  x.vals <- names(table(x))
  y.vals <- names(table(y))
  t <- cbind(x, y)
  res <- 0
  for(x.i in x.vals){
    for(y.i in y.vals){
      p_xi_yi <- length(t[t[,1] == x.i & t[,2] == y.i,] ) / 2 / nrow(t)
      if(!is.null(p_xi_yi) && length(p_xi_yi) > 0 && p_xi_yi > 0){
        res <- res - p_xi_yi * log2(p_xi_yi)
      }
      
    }
  }
  return(res) 
  
}


two.var.conditional.entropy <- function(x, y) {
  two.var.joint.entropy(x, y) - col.entropy(y)
}

two.var.mutual.information <- function(x, y) {
  col.entropy(x) - two.var.conditional.entropy(x, y)
}


df.mutual.information <- function(df, file) {
  ncols <- ncol(df)
  coln <- colnames(df)
  mi.df <- data.frame(matrix(ncol=ncols,nrow=ncols, dimnames=list(coln, coln)))
  for (i in 1:ncols){
    for (j in 1:i){
        x <- df[, i]
        y <- df[, j]
        c <- two.var.mutual.information(as.vector(x), as.vector(y))
        mi.df[i, j] <- c
        mi.df[j, i] <- c
    }
  }
  if (file != "None"){
    visualize.infotheo.matrix(mi.df, type = "Mutual Information", file = file)
  }
  mi.df
}


df.joint.entropy <- function(df, file) {
  ncols <- ncol(df)
  coln <- colnames(df)
  mi.df <- data.frame(matrix(ncol=ncols,nrow=ncols, dimnames=list(coln, coln)))
  for (i in 1:ncols){
    for (j in 1:ncols){
        x <- df[, i]
        y <- df[, j]
        c <- two.var.joint.entropy(as.vector(x), as.vector(y))
        mi.df[i, j] <- c
    }
  }
  if (file != "None"){
    visualize.infotheo.matrix(mi.df, type = "Joint Entropy", file = file)
  }
  mi.df
}

df.conditional.entropy <- function(df, file) {
  ncols <- ncol(df)
  coln <- colnames(df)
  mi.df <- data.frame(matrix(ncol=ncols,nrow=ncols, dimnames=list(coln, coln)))
  for (i in 1:ncols){
    for (j in 1:ncols){
        x <- df[, i]
        y <- df[, j]
        c <- two.var.conditional.entropy(as.vector(x), as.vector(y))
        mi.df[i, j] <- c
    }
  }
  if (file != "None"){
    visualize.infotheo.matrix(mi.df, type = "Conditional Entropy", file = file)
  }
  mi.df
}

#' Conditional Entropy
#' 
#' @param x a vector, matrix, or data.frame.
#' @param y a vector. y must be NULL if x is a matrix or a data.frame
#' @param file In case of x been a data.frame or matrix, represent the name of output file for saving conditional entropy plot
#' 
#' 
#' @return
#' In case of x been a data.frame or matrix, conditional entropy of all variables/columns in x. Otherwise, conditional entropy of x and y
#' 
#' @examples
#' df <- data.frame("V1" = sample.int(2,5,replace=TRUE), "V2" =sample.int(2,5,replace=TRUE) , "V3" = sample.int(2,5,replace=TRUE), "V4" = sample.int(2,5,replace=TRUE),  "V5" = sample.int(2,5,replace=TRUE), "V6" = sample.int(2,5,replace=TRUE), "V7" = sample.int(2,5,replace=TRUE), "V8" = sample.int(2,5,replace=TRUE))
#' x <- sample.int(2, 5,replace=TRUE)
#' y <- sample.int(2,5,replace=TRUE)
#' conditional.entropy(df, file ="conditionalEntropy")
#' conditional.entropy(x = x, y = y)
#' 


conditional.entropy <- function(x, y = NULL, file = "None"){
  if (is.data.frame(x) | is.matrix(x)){
    df.conditional.entropy(x, file)
  }else if (is.vector(x) && is.atomic(x) && !is.null(y)) {
    two.var.conditional.entropy(x, y)
  }else{
    stop("Not supported data type")
  }

}

#' Joint Entropy
#' 
#' @param x a vector, matrix, or data.frame.
#' @param y a vector. y must be NULL if x is a matrix or a data.frame.
#' @param file In case of x been a data.frame or matrix, represent the name of output file for saving joint entropy plot.
#' 
#' 
#' @return
#' In case of x been a data.frame or matrix, joint entropy of all variables/columns in x. Otherwise, joint entropy of x and y.
#' 
#' @examples
#' df <- data.frame("V1" = sample.int(2,5,replace=TRUE), "V2" =sample.int(2,5,replace=TRUE) , "V3" = sample.int(2,5,replace=TRUE), "V4" = sample.int(2,5,replace=TRUE),  "V5" = sample.int(2,5,replace=TRUE), "V6" = sample.int(2,5,replace=TRUE), "V7" = sample.int(2,5,replace=TRUE), "V8" = sample.int(2,5,replace=TRUE))
#' x <- sample.int(2, 5,replace=TRUE)
#' y <- sample.int(2, 5,replace=TRUE)
#' joint.entropy(df, file ="jointEntropy")
#' joint.entropy(x = x, y = y)
#' 
joint.entropy <- function(x, y = NULL, file = "None"){
  if (is.data.frame(x) | is.matrix(x)){
    df.joint.entropy(x, file)
  }else if (is.vector(x) && is.atomic(x) && !is.null(y)) {
    two.var.joint.entropy(x, y)
  }else{
    stop("Not supported data type")
  }

}

#' Mutual information
#' 
#' @param x a vector, matrix, or data.frame.
#' @param y a vector. y must be NULL if x is a matrix or a data.frame.
#' @param file In case of x been a data.frame or matrix, represent the name of output file for saving mutual information plot.
#' 
#' 
#' @return
#' In case of x been a data.frame or matrix, mutual information of all variables/columns in x. Otherwise, mutual information of x and y.
#' 
#' @examples
#' df <- data.frame("V1" = sample.int(2,5,replace=TRUE), "V2" =sample.int(2,5,replace=TRUE) , "V3" = sample.int(2,5,replace=TRUE), "V4" = sample.int(2,5,replace=TRUE),  "V5" = sample.int(2,5,replace=TRUE), "V6" = sample.int(2,5,replace=TRUE), "V7" = sample.int(2,5,replace=TRUE), "V8" = sample.int(2,5,replace=TRUE))
#' x <- sample.int(2, 5,replace=TRUE)
#' y <- sample.int(2, 5,replace=TRUE)
#' mutual.information(df, file ="mutualInformation")
#' mutual.information(x = x, y = y)

mutual.information <- function(x, y = NULL, file = "None"){
  if (is.data.frame(x) | is.matrix(x)){
    df.mutual.information(x, file)
  }else if (is.vector(x) && is.atomic(x) && !is.null(y)) {
    two.var.mutual.information(x, y)
  }else{
    stop("Not supported data type")
  }

}


#' Heatmap plot of theory information metrics
#' 
#' @param df Conditional Entropy, Joint Entropy or Mutual Information data.frame or matrix.
#' @param type Type of theory information metric. Character
#' @param file  In case you want to save the plot, file represents output file name.
#' @param colors Three colors for the gradient. Default c("#DEB841", "white", "#267278").
#' 
#' 
#' @return
#' Plot theory information metrics heatmap
#' 
#' @examples
#' df <- data.frame("V1" = sample.int(2,5,replace=TRUE), "V2" =sample.int(2,5,replace=TRUE) , "V3" = sample.int(2,5,replace=TRUE), "V4" = sample.int(2,5,replace=TRUE),  "V5" = sample.int(2,5,replace=TRUE), "V6" = sample.int(2,5,replace=TRUE), "V7" = sample.int(2,5,replace=TRUE), "V8" = sample.int(2,5,replace=TRUE))
#' ce <- conditional.entropy(df)
#' je <- joint.entropy(df)
#' mi <- mutual.information(df)
#' 
#' visualize.infotheo.matrix(ce, type = "Conditional Entropy")
#' visualize.infotheo.matrix(ce, type = "Conditional Entropy", file = "conditionalEntropy")
#' visualize.infotheo.matrix(ce, type = "Conditional Entropy", file = "conditionalEntropy", colors = c("Blue", "White", "Green"))
#' 
#' 
#' visualize.infotheo.matrix(je, type = "Joint Entropy")
#' visualize.infotheo.matrix(je, type = "Joint Entropy", file = "jointEntropy")
#' visualize.infotheo.matrix(je, type = "Joint Entropy", file = "jointEntropy", colors = c("Blue", "White", "Green"))
#' 
#' 
#' visualize.infotheo.matrix(je, type = "Mutual Information")
#' visualize.infotheo.matrix(je, type = "Mutual Information", file = "mutualInformation")
#' visualize.infotheo.matrix(je, type = "Mutual Information", file = "mutualInformation", colors = c("Blue", "White", "Green"))


visualize.infotheo.matrix <- function(df, type, file = "None", colors = c("#DEB841", "white", "#267278")) {
  if (!requireNamespace("ggplot2") | !requireNamespace("reshape2")){
    stop("visualize.infotheo.matrix requires the installation of ggplot2 and reshape2 packages")
  }
  df.melt <- reshape2::melt(as.matrix(df))
  gg <- ggplot2::ggplot(df.melt, ggplot2::aes(x = Var2, y = Var1)) + 
  ggplot2::geom_tile(ggplot2::aes(fill = value))+
  ggplot2::geom_text(ggplot2::aes(label = round(value, 2))) +
  ggplot2::scale_fill_gradientn(colors = colors, limits =  c(min(df),max(df))) + 
  ggplot2::labs(title=type) +
  ggplot2::theme_bw() + ggplot2::theme(axis.text.x=ggplot2::element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=ggplot2::element_text(size=9),
                     axis.title.x=ggplot2::element_blank(),
                     axis.title.y=ggplot2::element_blank(),
                     plot.title=ggplot2::element_text(size=11))
  if(file != "None"){
    path = paste(file, "jpg", sep = ".")
    ggplot2::ggsave(filename=path, plot= gg, height=15, width=15, units=c("cm"), dpi=200)
  }
  gg
}




two.var.correlation <- function(x, y, method) {
  
  if(method == "Pearson"){
    if(all(x == y)){
      return(1)
    }
    x.y.cov <- cov(x, y)
    if (x.y.cov != 0){
      mul.sd.x.y <- sd(x) * sd(y)
      if (mul.sd.x.y != 0){
        return(x.y.cov/mul.sd.x.y)
      }
    } 
    return(0)
  }else if(method == "Spearman"){
    n <- length(x)
    rx <- range.spearman(x)
    ry <- range.spearman(y)
    d2 <-(rx-ry) ^ 2 
    Sd2 <- sum(d2)
    corr <- 1 - (6 * Sd2) / (n * (n^2 - 1)) 
  }else{
    stop("Not supported discretization method")
  }
  corr
}

range.spearman <- function(x) {
  x.sort <- x[order(x)]
  x.values <- 1:length(x)
  x.ranges <- x.values
  x.sort.values <- data.frame("x" = x.sort, "values" = x.values)
  for (i in 1:length(x)){
    x.sort.i <- x.sort[i]
    x_i <- data.frame(x.sort.values[x.sort.values$x == x.sort.i , ])
    x.values[i] <- mean(x_i$values)
    
    
  }
  df.values <- data.frame("x" = x.sort, "v" = x.values)
  res <- c()
  for (elem in x){
    val <- head(df.values[df.values$x == elem, 2], 1)
    res <- c(res, val)
  }
  res
  
  
}



df.correlation <- function(df, method, file) {
  ncols <- ncol(df)
  coln <- colnames(df)
  cor.df <- data.frame(matrix(ncol=ncols,nrow=ncols, dimnames=list(coln, coln)))
  for (i in 1:ncols){
    for (j in 1:i){
      if (i != j){
        x <- df[, i]
        y <- df[, j]
        c <- two.var.correlation(x, y, method)
        cor.df[j, i] <- c
      }
    }
  }
  cor.df <- cor.df[-nrow(cor.df), -1]
  if (file != "None"){
    visualize.correlation.matrix(cor.df, file)
  }
  cor.df
  
}

#' Correlation
#' 
#' @param x a numeric vector, matrix, or data.frame.
#' @param y a numeric vector. y must be NULL if x is a matrix or a data.frame.
#' @param method Pearson or Spearman method for calculate correlation. Default Pearson.
#' @param file In case of x been a data.frame or matrix, represent the name of output file for saving mutual information plot.
#' 
#' 
#' @return
#' In case of x been a data.frame or matrix, correlation of all variables/columns in x. Otherwise, correlation of x and y.
#' 
#' @examples
#' df <- data.frame("V1" = sample.int(3000,100,replace=FALSE), "V2" =sample.int(3000,100,replace=FALSE) , "V3" = sample.int(3000,100,replace=FALSE), "V4" = sample.int(3000,100,replace=FALSE),  "V5" = sample.int(3000,100,replace=FALSE), "V6" = sample.int(3000,100,replace=FALSE), "V7" = sample.int(3000,100,replace=FALSE), "V8" = sample.int(3000,100,replace=FALSE))
#' x <- c(35,23,47, 17, 10, 43, 9, 6, 28)
#' y <- c(30, 33, 45, 23, 8, 49, 12, 4, 31)
#' 
#' correlation(x = x, y = y, method = "Pearson")
#' correlation(x = x, y = y, method = "Spearman")
#' correlation(x = df, method = "Pearson")
#' correlation(x = df, method = "Spearman")
#' correlation(x = df, method = "Pearson", file = "pearsonCor")
#' correlation(x = df, method = "Spearman", file = "spearmanCor")

correlation <- function(x, y = NULL, method = "Pearson", file = "None"){
 if (is.data.frame(x) | is.matrix(x)){
    df.correlation(x, method, file)
  }else if (is.vector(x) && is.atomic(x) && !is.null(y)) {
    two.var.correlation(x, y, method)
  }else{
    stop("Not supported data type")
  }
}

#' Plot correlation matrix
#' 
#' @param cor.df a data.frame representing correlation matrix
#' @param file In case of x been a data.frame or matrix, represent the name of output file for saving mutual information plot.
#' @param colors gradiente colors
#' 
#' @return
#' Plot correlation matrix
#' 
#' @examples
#' df <- data.frame("V1" = sample.int(3000,100,replace=FALSE), "V2" =sample.int(3000,100,replace=FALSE) , "V3" = sample.int(3000,100,replace=FALSE), "V4" = sample.int(3000,100,replace=FALSE),  "V5" = sample.int(3000,100,replace=FALSE), "V6" = sample.int(3000,100,replace=FALSE), "V7" = sample.int(3000,100,replace=FALSE), "V8" = sample.int(3000,100,replace=FALSE))
#' pearson.cor <- correlation(x = df, method = "Pearson")
#' visualize.correlation.matrix(cor.df = pearson.cor)
#' visualize.correlation.matrix(cor.df = pearson.cor, file = "pearsonCor")
#' visualize.correlation.matrix(cor.df = pearson.cor, file = "pearsonCor", colors = c("Blue", "White", "Green"))

visualize.correlation.matrix <- function(cor.df, file = "None", colors = c("#DEB841", "white", "#267278")) {
  if (!requireNamespace("ggplot2") | !requireNamespace("reshape2")){
    stop("visualize.correlation.matrix requires the installation of ggplot2 and reshape2 packages")
  }
  df <- reshape2::melt(as.matrix(cor.df))
  gg <- ggplot2::ggplot(df, ggplot2::aes(x = Var2, y = Var1)) + 
  ggplot2::geom_tile(ggplot2::aes(fill = value))+
  ggplot2::geom_text(ggplot2::aes(label = ifelse(is.na(value), "", round(value, 2)))) +
  ggplot2::scale_fill_gradientn(colors = colors, limits =  c(-1,1), na.value = "white", breaks = c(-1,0,1),  labels = format(c(-1,0,1))) + 
  ggplot2::labs(title="Correlation Matrix")+ 
  ggplot2::theme_bw() + ggplot2::theme(axis.text.x=ggplot2::element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=ggplot2::element_text(size=9),
                     axis.title.x=ggplot2::element_blank(),
                     axis.title.y=ggplot2::element_blank(),
                     plot.title=ggplot2::element_text(size=11))
  if(file != "None"){
    path = paste(file, "jpg", sep = ".")
    ggplot2::ggsave(filename=path, plot= gg, height=15, width=15, units=c("cm"), dpi=200)
  }
  gg
}




confusion.matrix <- function(actual, predited){
  TP <- 0
  FP <- 0
  TN <- 0
  FN <- 0
  n <- length(actual)
  for (i in 1:n){
    if (actual[i]  && predited[i]){
      TP <- TP + 1
    }else if (actual[i]  && !predited[i]){
      FN <- FN + 1
    }else if (!actual[i] && predited[i] ){
      FP <- FP + 1
    }else{
      TN <- TN + 1
    }
  }
  c.matrix <- matrix(c(TP, FN, FP, TN), nrow = 2, byrow = TRUE)

  rownames(c.matrix) <- c("Real Positive", "Real Negative")
  colnames(c.matrix) <- c("Predicted Positive", "Predicted Negative")
  
  return(c.matrix)
}

confusion.matrix.TPR <- function(m){
  TP <- m[1,1]
  FN <- m[1,2]
  if (TP > 0){
    TPR <- TP / (TP + FN)
  }else{
    TPR <- 0
  }
}

confusion.matrix.FPR <- function(m){
  FP <- m[2,1]
  TN <- m[2,2]
  if (FP > 0){
    FPR <- FP / (FP + TN)
  }else{
    FPR <- 0
  }
}

#' auc
#' 
#' @param x if x is a dataframe represent FPR and TPR values but if it is a vector represent FPR value.
#' @param y if x is a vector, y represent TPR values
#' 
#' @return
#' auc value
#' 
#' @examples
#' random.x <- sample.int(3000,1000,replace=FALSE)
#' class <- c(rep(TRUE, 500), rep(FALSE, 500))
#' pos <- sample.int(1000, 1000)
#' random.y <- class[order(pos)]
#' roc.curve <- roc.curve(random.x, random.y)
#' auc <- auc(roc.curve$FPR , roc.curve$TPR )
auc <- function(x, y = NULL) {
  if (is.data.frame(x) | is.matrix(x)){
      y <- x$TPR
      x <- x$FPR
      sum(-diff(x) * rowMeans(cbind(y[-1], y[-length(y)])))
  }else if (is.vector(x) && is.atomic(x) && !is.null(y)) {
      sum(-diff(x) * rowMeans(cbind(y[-1], y[-length(y)])))
  }else{
    stop("Not supported data type")
  }
}



#' Roc curve
#' 
#' @param x a numerical vector
#' @param y represent logical class TRUE/FALSE.
#' @param file name of output file for saving roc curve plot.
#' @param AUC return auc value AUC = TRUE
#' 
#' 
#' @return
#' Return roc curve values: TPR, FPR or AUC
#' 
#' @examples
#' random.x <- sample.int(3000,1000,replace=FALSE)
#' class <- c(rep(TRUE, 500), rep(FALSE, 500))
#'  pos <- sample.int(1000, 1000)
#'  random.y <- class[order(pos)]
#' 
#' perfect.x <- 1:3000
#' perfect.y <- c(rep(TRUE, 1500), rep(FALSE, 1500))
#' 
#' worst.x <- 1:3000
#' worst.y <- c(rep(FALSE, 1500), rep(TRUE, 1500))
#' 
#' roc.curve(random.x, random.y, file= "randomRocCurve")
#' roc.curve(perfect.x, perfect.y, file= "perfectRocCurve")
#' roc.curve(worst.x, worst.y, file= "worstRocCurve") 
roc.curve <- function(x, y, file = "None", AUC = FALSE){
  
  x.order <- x[order(x)]
  x.length <- length(x)
  
  actual.labels <- y[order(x)]
  roc.TPR <- c()
  roc.FPR <- c()
  
  for (i in 0:x.length){
    n.true <- x.length - i
    n.false <- x.length - n.true
    predicted.labels <- c(rep(TRUE, n.true), rep(FALSE, n.false))
    cMatrix <-  confusion.matrix(actual.labels, predicted.labels)
    
    TPR <- confusion.matrix.TPR(cMatrix)
    
    FPR <- confusion.matrix.FPR(cMatrix)
    
    roc.FPR <- append(roc.FPR, FPR)
    roc.TPR <- append(roc.TPR, TPR)
    
    
  }

  auc.val <- auc(roc.FPR, roc.TPR)

  if (file != "None"){
    visualize.roc.curve(roc.FPR, roc.TPR, auc.val, file)
  }
  if (AUC){
    auc.val
  }else{
    data.frame("FPR" = roc.FPR, "TPR" = roc.TPR)
  }
    
  
}


#' Plot Roc curve
#' 
#' @param roc.FPR a numerical vector representing false positive rates
#' @param roc.TPR a numerical vector representing true positive rates
#' @param auc auc value. Use auc() function.
#' @param file  name of output file for saving roc curve plot.
#' 
#' @return
#' Plot roc curve
#' 
#' @examples
#' random.x <- sample.int(3000,1000,replace=FALSE)
#' class <- c(rep(TRUE, 500), rep(FALSE, 500))
#' pos <- sample.int(1000, 1000)
#' random.y <- class[order(pos)]
#' roc.curve <- roc.curve(random.x, random.y)
#' auc <- auc(roc.curve$FPR , roc.curve$TPR )
#' visualize.roc.curve(roc.curve$FPR , roc.curve$TPR, auc)
#' visualize.roc.curve(roc.curve$FPR , roc.curve$TPR, auc, file = "randomRocCurve")
visualize.roc.curve <- function(roc.FPR, roc.TPR, auc, file = "None"){
  if (!requireNamespace("ggplot2")){
    stop("visualize.roc.curve requires the installation of ggplot2 and reshape2 packages")
  }
  data <- data.frame(FPR = rev(roc.FPR), TPR = rev(roc.TPR))
  auc.val <- as.character(round(auc, 3))
  
  gg <- ggplot2::ggplot(data, ggplot2::aes(x = FPR, y = TPR), limit = c(1,1)) + ggplot2::xlab("FPR (1-specificity)") + ggplot2::ylab("TPR (sensitivity)")
  gg <- gg + ggplot2::geom_area(position = "identity", fill="#69b3a2", alpha=0.4) 
  gg <- gg + ggplot2::geom_line(color="#9D9D9D", size=2)
  gg <- gg + ggplot2::geom_point(size=2, color="#267278")
  gg <- gg + ggplot2::geom_abline(size=1, color="#ec7063", linetype = "dashed")
  gg <- gg + ggplot2::annotate("text",  x = 0.8, y = 0.1, label = paste("AUC: ", auc.val))
  gg <- gg + ggplot2::annotate("rect", xmin = 0.65, xmax = 0.95, ymin = 0.05, ymax = 0.15, alpha = .3, fill="#424949" )
  gg <- gg + ggplot2::annotate(geom = "text", x = 0.5, y = 0.45, label = "RANDOM CLASSIFIER", color = "#ec7063", angle = 45)
  gg <- gg + ggplot2::ggtitle("ROC Curve") + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  
  if(file != "None"){
    path = paste(file, "jpg", sep = ".")
    ggplot2::ggsave(filename=path, plot= gg, height=15, width=15, units=c("cm"), dpi=200)
  }

  gg

}

#' Metric filter
#' 
#' @param df data.frame. Each colum represent a variable
#' @param class name of the class variable
#' @param by metric to apply the filter. AUC, Variance and Entropy are available.
#' @param uplimit metric uplimit value
#' @param lowlimit  metric uplimit value. Default
#' 
#'  
#' 
#' @return
#' Filtered data.frame
#' 
#' @examples
#' class <- c(rep(TRUE, 50), rep(FALSE, 50))
#' pos <- sample.int(100, 100)
#' random.y <- class[order(pos)]
#' df.numeric <- data.frame("V1" = sample.int(10,100,replace=TRUE), "V2" = sample.int(10,100,replace=TRUE), "V3" = sample.int(100,100,replace=TRUE), "Class" =  random.y)
#' df.cat <- data.frame("V1" = as.factor(sample.int(3,100,replace=TRUE)), "V2" =  as.factor(sample.int(3,100,replace=TRUE)),  "V4" =  as.factor(sample.int(3,100,replace=TRUE)), "Class" =  random.y)
#' filter(df.numeric, class = "Class", by = "AUC", uplimit = 1, lowlimit = 0.5)
#' filter(df.numeric, class = "Class", by = "Variance", uplimit = 100, lowlimit = 0)
#' filter(df.cat, class = "Class", by = "Entropy", uplimit = 2, lowlimit = 0)
#' filter(df.cat, class = "Class", by = "Entropy")
filter <- function(df, class, by = "AUC",  uplimit = .Machine$integer.max, lowlimit = -.Machine$integer.max) {
  y <- df[, c(class)]
  if(!is.logical(y)){
    stop("Class must be logical type")
  }
  vars <- df[, !names(df) %in% c(class)]
  if(by == "AUC"){
    if(FALSE %in% apply(vars, 2, is.numeric)){
      stop("For AUC filter, variables must be numerical")
    }
    df.filter <- filter.auc(df, vars, y, class, uplimit, lowlimit)
  }else if(by == "Variance"){
    if(FALSE %in% apply(vars, 2, is.numeric)){
      stop("For Variance filter, variables must be numerical")
    }
    df.filter <- filter.variance(df, vars, y, class, uplimit, lowlimit)
  }else if(by == "Entropy"){
    if(FALSE %in% apply(vars, 2, is.integer) && FALSE %in% apply(vars, 2, is.character)){
      stop("For Entropy filter, variables must be numerical")
    }
    df.filter <- filter.entropy(df, vars, y, class, uplimit, lowlimit)
  }else{
    stop("Not supported filter method")
  }
  df.filter
  
}

filter.auc <- function(df, vars, y, class, uplimit, lowlimit) {
  
  vars.auc <- apply(vars, 2, roc.curve, y = y, AUC = TRUE)
  print(vars.auc)
  df.filtered <- df[ , c(names(subset(vars.auc, vars.auc >= lowlimit & vars.auc <= uplimit)), class)]
  if (is.logical(df.filtered)){
    df.filtered <- data.frame(class = df.filtered)
  }

  df.filtered
}

filter.variance <- function(df, vars, y, class, uplimit, lowlimit) {
  vars.variance <- variance(vars)
  df.filtered <- df[ , c(names(subset(vars.variance,vars.variance >= lowlimit & vars.variance <= uplimit)), class)]
  if (is.logical(df.filtered)){
    df.filtered <- data.frame(class = df.filtered)
  }

  df.filtered
  
}


filter.entropy <- function(df, vars, y, class, uplimit, lowlimit) {
  vars.entropy <- entropy(vars)
  df.filtered <- df[ , c(names(subset(vars.entropy,vars.entropy >= lowlimit & vars.entropy <= uplimit)), class)]
  if (is.logical(df.filtered)){
    df.filtered <- data.frame(class = df.filtered)
  }

  df.filtered
  
}



