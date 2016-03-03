#' @importFrom stringr str_detect

listSampleData <- function() {
  path <- file.path(getwd(), "tests", "testthat", "test_data")
  list.files(path)
}

getSampleData <- function(name, isTest = FALSE) {
  if (!str_detect(name, "^.+\\.lp$")) {
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
