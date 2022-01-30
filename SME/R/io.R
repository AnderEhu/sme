
split.line <- function(line, sep){
  unlist(strsplit(line, sep))
}


#' Read a csv file
#' 
#' @param file.path Name of input file
#' @param sep Field separator. Default ;
#' @param max.n.factor maximum number of column characters to be a factor. Default 3.
#' @param header a logical value indicating whether the file contains the names of the variables as its first line. Default FALSE.
#' @param row.names a vector of row names. Default NULL.
#' @param col.names a vector of col names. Default NULL.
#' @param n.rows integer: the maximum number of rows to read in. In case of Header = TRUE, it does not count for the number of read lines.
#' @param transpose If TRUE, transpose the result. Default FALSE.
#' 
#' @return A data frame containing a representation of the data in the file.
#' @export
#'
#' @examples
#' df = data.frame("V1" = 1:5, "V2" = c("a", "b", "c", "d", "e"), "V3" = c("alto", "bajo", "bajo", "alto", "alto"))
#' write.df(df, "bench1.txt")
#' read.df(file.path = "bench1.txt")
#' 
#' write.df(df, "bench2.txt", header = TRUE)
#' read.df(file.path = "bench2.txt", header = TRUE)
#' 
#' write.df(df, "bench2.txt", append = TRUE)
#' read.df(file.path = "bench2.txt", header = FALSE)
#' 
#' write.df(df, "bench2.txt", sep = ";", header = FALSE, append = TRUE)
#' read.df(file.path = "bench2.txt", sep = ";", header = TRUE,  max.n.factor = 3, row.names = c('X1', 'X2', 'X3', 'X4', 'X5'), col.names = c('A', 'B', 'C'), n.rows = 5, transpose = FALSE)
#'
#'
read.df <- function(file.path, sep = ";", max.n.factor = 3, header=FALSE, row.names = NULL, col.names = NULL, n.rows = -1L, transpose = FALSE) {
  if (n.rows >= 0){
    n.rows <- n.rows + 1
  }
  con <- file(description=file.path, open="r")
  lines <- readLines(con=con, n = n.rows)
  close(con)
  
  splited.lines <- lapply(lines, split.line, sep)
  
  n.col <- length(splited.lines[[1]])
  
  if (header) { 
    n.lines <- length(splited.lines) - 1
    names <- splited.lines[[1]]
    splited.lines[[1]] <- NULL
  }else{
    n.lines <- length(splited.lines)
    names <- paste("X", rep(1:n.col), sep="")
  }
  
  
  m.lines <- matrix(unlist(splited.lines), ncol = n.col, byrow = TRUE)

  col.with.type <- lapply(asplit(m.lines,2), col.type, max.n.factor)
  df <- as.data.frame(col.with.type)
  if (is.null(col.names)){
     colnames(df) <- names
  }else{
    colnames(df) <- col.names
  }

  if(!is.null(row.names)){
    rownames(df) <- row.names
  }
  if (transpose){
    return(t(df))
  }else{
    return(df)
  }
  

}

col.type <-  function(col, max.n.factor){
  
  if (!any(is.na(suppressWarnings(strtoi(col))))){
    return(as.integer(col))
    
  }else if (!any(is.na(suppressWarnings(as.numeric(col))))){
    return(as.numeric(col))
    
  }else{
    col.as.factor <- as.factor(col)
    if (max.n.factor < nlevels(col.as.factor) ){
      return(col)
    }else{
      return(col.as.factor)
    }

  }
}


#' Write a csv file
#' 
#' @param df the object to be written, preferably a matrix or data frame.
#' @param file.path Name of output file.
#' @param sep the field separator string. Values within each row of x are separated by this string. Default ";" .
#' @param append If TRUE, the output is appended to the file. If FALSE, any existing file of the name is destroyed. Defatul FALSE.
#' @param header if TRUE, name of the columns are written in the first line. Default FALSE.
#' 
#' 
#' 
#' @export
#'
#' @examples
#' df = data.frame("V1" = 1:5, "V2" = c("a", "b", "c", "d", "e"), "V3" = c("alto", "bajo", "bajo", "alto", "alto"))
#' write.df(df, "bench1.txt")
#' write.df(df, "bench2.txt", header = TRUE)
#' write.df(df, "bench2.txt", append = TRUE)
#' write.df(df, "bench2.txt", sep = ";", header = FALSE, append = TRUE)
#'
#'

write.df <- function(df, file.path, sep = ";", append=FALSE, header = FALSE){
  if (append) {
    con <- file(description = file.path, open="a")
  }else{
    con <- file(description = file.path, open="w")
  }
  
  if (header){
    col.names <- paste(colnames(df), collapse = sep)
    writeLines(col.names, con = con)
  }
  
  write.df <- apply(df, MARGIN=1,
        FUN=function(row) {
          return(paste(row, collapse=sep))
        })
  
  writeLines(write.df, con=con)
  close(con)
  
}
