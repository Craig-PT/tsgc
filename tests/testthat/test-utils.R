test_that("Test argmax", {
  x <- xts::xts(1:10, order.by = seq(as.Date("2021-01-01"),
                                     length.out = 10, by = 1))
  x[5,] <- 20
  expect_identical(str(index(argmax(x))), str(index(x)[5]))
  expect_identical(
    str(index(argmax(x, decreasing = FALSE))),
    str(index(x)[1])
  )
})


test_that("forecast.peak", {
  expect_error(forecast.peak(delta = -4, gamma = 0))
  expect_equal(3, round(forecast.peak(delta = -2, gamma = -0.1)))
})
