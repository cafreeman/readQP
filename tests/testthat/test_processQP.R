context("Read a model file and return the parsed Q components")

expected <- data.frame(
  x1 = c("a", "b", "a", "b"),
  x2 = c("a", "a", "b", "b"),
  x3 = c("a^2", "b*a", "a*b", "b^2"),
  values = c("1", NA, "+4", "+7"),
  stringsAsFactors = FALSE
  )

test_that("processQP can read model a file and return the Q components", {
  test_path <- getSampleData("simple_qp", TRUE)
  # test_path <- normalizePath("./test_data/simple_qp.lp")
  result <- processQP(test_path)
  expect_is(result, "data.frame")
  expect_equal(length(result), 4)
  expect_equal(names(result), c("x1", "x2", "x3", "values"))
  for (i in names(result)) {
    expect_equal(result$i, expected$i)
  }
  expect_false(all(is.na(result)))
})
