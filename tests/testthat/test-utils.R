test_that("check_length actually checks length", {
  expect_silent(check_lengths(1:10, 1, 1))
  expect_error(check_lengths(1:10, 1, 1:5))
})
test_that("pad_arg actually pads args", {
  N <- 10
  expect_equal(length(pad_arg(1, N)), N)
  expect_equal(length(pad_arg(1:4, N)), 4)
})
