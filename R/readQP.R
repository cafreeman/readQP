# readQP.R

#' @export
readQP <- function(filePath) {
  if (!isLPFile(filePath)) {
    stop("You must use a CPLEX LP model file (ending in '.lp')")
  }
  file <- readChar(normalizePath(filePath), file.info(filePath)$size)
  return(file)
}
