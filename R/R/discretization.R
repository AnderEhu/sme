elem.discretize <- function(elem, cut.points) {
  num.bins <- length(cut.points) + 1
  discretize.values <- paste("I", seq(1:num.bins), sep="")
  if(elem <= cut.points[1]){
    elem.interval <- paste("(-Inf, ",  cut.points[1], "]", sep = "")
    elem.discretize <- discretize.values[1]
    
  } else if(elem >  cut.points[num.bins-1]) {
    elem.interval <- paste("(", cut.points[num.bins-1], ", Inf)", sep = "")
    elem.discretize <- discretize.values[num.bins]
  } else{
    for (i in 2:(num.bins-1)){
      if(elem > cut.points[i-1] & elem <= cut.points[i]){
        elem.interval <- paste("(",cut.points[i-1], ", ", cut.points[i], "]", sep = "")
        elem.discretize <- discretize.values[i]
      }
    }
  }
  
  names(elem.discretize) <- elem.interval
  return(elem.discretize)
  
} 

cutpoints.discretize <- function (x, cut.points) {
  x.order <- order(x)
  num.bins <- length(cut.points) + 1
  x.discretized <- sapply(x, elem.discretize, cut.points = cut.points)
  
}


fdiscretizeEW <- function (x, num.bins) {

  if (num.bins < 2){
    return(x)
  }
  
  x.min <- min(x)
  x.max <- max(x)
  #Calculamos la longitud de cada intervalos
  w <- (x.max - x.min) / num.bins
  
  #Calculamos los puntos de corte con longitud de intervalo w
  cut.points <- round(seq(x.min + w, x.max - 1L, w), 3)
  
  cutpoints.discretize(x, cut.points)
  
}

discretizeClustering <- function (x, num.bins) {

  x.length <- length(x)
  x.ordered <- x[order(x)]
  n.cut.points <- num.bins - 1

  #Aplicamos kmeans
  kmeans.res = kmeans(x, centers = num.bins, iter.max = 1000, nstart = 10)
  
  #Ordenamos el vector
  ordered.rep.num.bins <- kmeans.res$cluster[order(kmeans.res$cluster)]

  #Calculamos los puntos de corte
  cut.points.index <- split(x.ordered,  ordered.rep.num.bins)
  x.divided <- sapply(cut.points.index, function(x) tail(x, 1))
  cut.points <- round(head(x.divided, -1), 2)

  #Discretizamos en base a los puntos de corte
  cutpoints.discretize(x, cut.points)
  
}



  
discretizeEF <- function (x, num.bins) {

  x.length <- length(x)
  x.ordered <- x[order(x)]
  n.cut.points <- num.bins - 1

  #Creamos un vector de longitud igual a x repitiendo el vector de 1 hasta num.bins
  rep.num.bins <- rep_len(seq(1:num.bins), x.length)

  #Ordenamos el vector
  ordered.rep.num.bins <- rep.num.bins[order(rep.num.bins)]

  #Calculamos los puntos de corte
  cut.points.index <- split(x.ordered,  ordered.rep.num.bins)
  x.divided <- sapply(cut.points.index, function(x) tail(x, 1))
  cut.points <- round(head(x.divided, -1), 2)

  #Discretizamos en base a los puntos de corte
  cutpoints.discretize(x, cut.points)
  
}

discretizeDF <- function(df, method, num.bins){
  df.ncol <- ncol(df)
  if(method == "frequency"){
    df.discretize.vector <- discretizeEF(as.vector(t(df)), num.bins)
  }else if (method == "interval"){
    df.discretize.vector <- fdiscretizeEW(as.vector(t(df)), num.bins)
  }else if (method == "clustering"){
    df.discretize.vector <- discretizeClustering(as.vector(t(df)), num.bins)
  }else{
    stop("Not supported discretization method")
  }
  df.discretize <- as.data.frame(matrix(df.discretize.vector, ncol = df.ncol, byrow=TRUE ))
}

discretizeVector <- function(v, method, num.bins){

  if(method == "frequency"){
    discretize.vector <- discretizeEF(v, num.bins)
  }else if (method == "clustering"){
    discretize.vector <- discretizeClustering(v, num.bins)
  }else if (method == "interval"){
    discretize.vector <- fdiscretizeEW(v, num.bins)
  }else{
    stop("Not supported discretization method")
  }
  

}



#' Discretize
#' 
#' @param x a numeric vector, matrix or dataframe (continuous variables)-
#' @param method discretization method. Available are: "interval" (equal interval width), "frequency" (equal frequency), "cluster" (k-means clustering).
#' @param num.bins number of intervals.
#' 
#' @return
#' A discretized vector or dataframe
#' 
#' @examples
#' v.example.1 <- c(11.5, 10.2, 1.2, 0.5, 5.3, 20.5, 8.4)
#' v.example.2 <- c(0,4,12,16,16,18,24,26,28)
#' df.example.3 <- data.frame(c(0,4,12), c(16,16,18), c(24,26,28))
#' matrix.example.4 <- matrix(c(0,4,12,16,16,18,24,26,28), 3, 3, byrow = FALSE )
#' 
#' ew.discretize.example1 <- discretize(v.example.1, method="interval", 4)
#' ew.discretize.example2 <- discretize(v.example.2, method="interval", 3)
#' ew.discretize.example3 <- discretize(df.example.3, method="interval", 3)
#' ew.discretize.example4 <- discretize(matrix.example.4, method="interval", 2)
#' 
#' 
#' ef.discretize.example1 <- discretize(v.example.1, method="frequency", 4)
#' ef.discretize.example2 <- discretize(v.example.2, method="frequency", 3)
#' ef.discretize.example3 <- discretize(df.example.3, method="frequency", 3)
#' ef.discretize.example4 <- discretize(matrix.example.4, method="frequency", 2)
#' 
#' 
#' 
#' clustering.discretize.example1 <- discretize(v.example.1, method="clustering", 4)
#' clustering.discretize.example2 <- discretize(v.example.2, method="clustering", 3)
#' clustering.discretize.example3 <- discretize(df.example.3, method="clustering", 3) 
#' clustering.discretize.example3 <- discretize(matrix.example.4, method="clustering", 2)   

discretize <- function(x, method = "frequency", num.bins){

  if (method != "frequency" && method != "interval" && method != "clustering"){
    stop(paste(method,  "is not a valid method"))
  }
  if (is.data.frame(x) | is.matrix(x)){
    discretizeDF(x, method, num.bins)
  }else if (is.vector(x) && is.atomic(x)) {
    discretizeVector(x, method, num.bins)
  }else{
    stop("Not supported data type")
  }
  
}