context("read CPLEX QP files")

test_that("read a CPLEX file successfully", {
  test_path <- getSampleData("simple_qp", TRUE)
  file <- readModelFile(test_path)
  expect_equal(file, "Minimize\nobj: a + b + [ a^2 + 4 a * b + 7 b^2 ]/2\nSubject To\nc1: a + b >= 10\nEnd\n")
})

test_that("error when the user doesn't point to a CPLEX file", {
  test_path <- getSampleData("non_CPLEX.txt", TRUE)
  expect_error(readModelFile(test_path), "You must use a CPLEX LP model file \\(ending in '.lp'\\)")
})

test_that("readQP correctly short-circuits when it encounters a model without a quadratic piece", {
  test_path <- getSampleData("lp_example", TRUE)
  expect_message(readQP(test_path), "^No quadratic elements detected\\. .+$")
  result <- suppressMessages(readQP(test_path))
  expect_is(result, c("MP_data_from_file", "MILP"))
})
