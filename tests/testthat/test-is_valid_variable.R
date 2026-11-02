test_that("is_valid_variable detects valid and invalid cases", {
  expect_true(MetricFishR:::is_valid_variable(
    var   = c(1, 2, 3, 4, 5, 6),
    group = c("A", "A", "A", "B", "B", "B")
  ))

  expect_false(MetricFishR:::is_valid_variable(
    var   = c(1, NA),
    group = c("A", "B")
  ))
  expect_false(MetricFishR:::is_valid_variable(
    var   = c(1, 2, 3),
    group = c("A", "A", "A")
  ))
  expect_false(MetricFishR:::is_valid_variable(
    var   = c(1, 1, 1),
    group = c("A", "A", "B")
  ))
})
