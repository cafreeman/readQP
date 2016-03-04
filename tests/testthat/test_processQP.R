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
  modelFile <- readModelFile(test_path)
  list[model, vecMap] <- processQP(modelFile, "CPLEX_LP")
  expect_is(model, c("MP_data_from_file", "MILP"))
  expect_is(vecMap, "data.frame")
  expect_equal(length(vecMap), 4)
  expect_equal(names(vecMap), c("x1", "x2", "x3", "values"))
  for (i in names(vecMap)) {
    expect_equal(vecMap$i, expected$i)
  }
  expect_false(all(is.na(vecMap)))
})
