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
