context("read CPLEX QP files")

test_that("read a CPLEX file successfully", {
  test_path <- getSampleData("simple_qp", TRUE)
  # test_path <- normalizePath("./test_data/simple_qp.lp")
  file <- readQP(test_path)
  expect_equal(file, "Minimize\nobj: a + b + [ a^2 + 4 a * b + 7 b^2 ]/2\nSubject To\nc1: a + b >= 10\nEnd\n")
})
