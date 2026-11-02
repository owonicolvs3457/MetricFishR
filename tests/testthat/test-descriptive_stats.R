test_that("descriptive_stats computes Min/Max/Mean/CV and exports an xlsx", {
  data <- data.frame(
    L1    = c(10, 12, 11, 13, 9),
    L2    = c(5, 5.5, 6, 5.2, 4.8),
    Group = c("A", "A", "B", "B", "B")
  )
  out_file <- tempfile(fileext = ".xlsx")

  res <- descriptive_stats(data, digits = 2, file_xlsx = out_file)

  expect_true(file.exists(out_file))
  expect_s3_class(res, "data.frame")
  expect_setequal(res$Variable, c("L1", "L2"))
  expect_equal(res$Min[res$Variable == "L1"], min(data$L1))
  expect_equal(res$Max[res$Variable == "L1"], max(data$L1))

  unlink(out_file)
})

test_that("descriptive_stats raises an error without a numeric column", {
  data <- data.frame(Group = c("A", "B", "C"))
  expect_error(descriptive_stats(data), "No numeric column")
})
