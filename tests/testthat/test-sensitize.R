test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("join_csv_files joins two files on 'run' column", {
  file1 <- system.file("extdata", "batch_param_map.txt", package = "sensitizeR")
  file2 <- system.file("extdata", "model_output.txt", package = "sensitizeR")
  df <- join_csv_files(file1, file2, by = "run")
  expect_true(is.data.frame(df))
  expect_true("run" %in% colnames(df))
  expect_gt(nrow(df), 0)
})

