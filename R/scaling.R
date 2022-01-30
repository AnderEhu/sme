v.normalize <- function(v){
  if(is.numeric(v)){
    v.min.min = v-min(v)
    if (length(v.min.min )> 0)
      v.norm <- v.min.min / (max(v) - min(v))
    else{
      v.norm <- 0
    }
  }
}

v.standardize <- function(v){
  if(is.numeric(v)){
    v.min.mean = (v-mean(v))
    if (length(v.min.mean) > 0)
      v.norm <- v.min.mean / sd(v)
    else{
      v.norm <- 0
    }
  }
}


#' Normalize
#'
#' Currently implemented for numeric vectors, numeric matrices and data.frame normalization.
#'
#' @param x numeric, matrix or dataframe input vector.
#' @param margin 1 - normalize by rows and 2 - normalize by columns. Default is 1.
#'
#' @return A normalized numeric or data.frame.
#' @export
#'
#' @examples
#' df <- data.frame("V1" = 1:3, "V2" = 3:5, "V3" = 5:7)
#' normalize(df, margin = 1L)
#' normalize(df, margin = 2L)
#'
#' m <- matrix(c(1,2,3,3,4,5,5,6,7), ncol = 3, byrow = FALSE)
#' normalize(m, margin = 1L)
# normalize(m, margin = 2L)
#'
#' v <- c(1, 3, 5)
#' normalize(v)

normalize <- function(x, margin = 1L) {
  if (is.data.frame(x)| is.matrix(x)){
    if(margin == 1L){
      x.normalized <- data.frame(t(apply(x, margin, v.normalize)))
    }else{
      x.normalized <- data.frame(apply(x, margin, v.normalize))
    }
    rownames(x.normalized) <- rownames(x)
    colnames(x.normalized) <- colnames(x)


  }else if (is.vector(x) && is.atomic(x)) {
    x.normalized <- v.normalize(x)
  }else{
    stop("Not supported data type")
  }
  x.normalized
}


#' Standardize
#'
#' Currently implemented for numeric vectors, numeric matrices and data.frame standardization.
#'
#' @param x numeric, matrix or dataframe input vector.
#' @param margin 1 - standardize by rows and 2 - standardize by columns. Default is 1.
#'
#' @return A standardize numeric or data.frame.
#' 
#'
#' @examples
#' df <- data.frame("V1" = 1:3, "V2" = 3:5, "V3" = 5:7)
#' standardize(df, margin = 1L)
#' standardize(df, margin = 2L)
#'
#' m <- matrix(c(1,2,3,3,4,5,5,6,7), ncol = 3, byrow = FALSE)
#' standardize(m, margin = 1L)
#' standardize(m, margin = 2L)
#'
#' v <- c(1, 3, 5)
#' standardize(v)
#'
#'
standardize <- function(x, margin = 1L) {
  if (is.data.frame(x)| is.matrix(x)){
    if(margin == 1L){
      x.standardized <- data.frame(t(apply(x, margin, v.standardize)))
    }else{
      x.standardized <- data.frame(apply(x, margin, v.standardize))
    }
    rownames(x.standardized) <- rownames(x)
    colnames(x.standardized) <- colnames(x)

  }else if (is.vector(x) && is.atomic(x)) {
    x.standardized <- v.standardize(x)
  }else{
    stop("Not supported data type")
  }
  x.standardized
}

