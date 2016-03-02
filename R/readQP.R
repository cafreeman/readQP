# readQP.R

#' @export
readQP <- function(filePath) {
  file <- readChar(normalizePath(filePath), file.info(filePath)$size)
  return(file)
}
