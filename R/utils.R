#' @importFrom stringr str_detect

isLPFile <- function(filePath) {
  tools::file_ext(filePath) == "lp"
}

listSampleData <- function() {
  path <- file.path(getwd(), "tests", "testthat", "test_data")
  list.files(path)
}

getSampleData <- function(name, isTest = FALSE) {
  if (nchar(tools::file_ext(name)) == 0) {
    name <- paste0(name, ".lp")
  }
  path <- ifelse(isTest,
                 file.path(getwd(), "test_data", name),
                 file.path(getwd(), "tests", "testthat", "test_data", name))
  if (!file.exists(path)) {
    msg <- paste(
      "The file you have requested does not exist.",
      "  Maybe you were looking for one these instead?",
      paste("\t", listSampleData(), collapse = "\n"),
      sep = "\n")
    stop(msg)
  }
  return(path)
}

# Helper function for destructuring lists/vectors. Helps when dealing with lists as results.
list <- structure(NA,class="result")
"[<-.result" <- function(x,...,value) {
  args <- as.list(match.call())
  args <- args[-c(1:2,length(args))]
  length(value) <- length(args)
  for(i in seq(along=args)) {
    a <- args[[i]]
    if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
  }
  x
}
