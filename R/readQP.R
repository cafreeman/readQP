# readQP.R

#' @name readQP
#' @title Utility for reading model files for parsing
#' @description Read a CPLEX model file into memory as a character string
#' @param filePath The path to the model file
#' @return A character string containing the contents of the model file.
#' @export
readQP <- function(filePath) {
  if (!isLPFile(filePath)) {
    stop("You must use a CPLEX LP model file (ending in '.lp')")
  }
  file <- readChar(normalizePath(filePath), file.info(filePath)$size)
  return(file)
}
