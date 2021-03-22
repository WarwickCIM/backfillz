test_that("can print one digit of pi", {
  op <- options(digits = 1)
  on.exit(options(op), add = TRUE, after = FALSE)
  expect_output(print(pi), "3")
})