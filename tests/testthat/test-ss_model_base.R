test_that("Test SSModelBase() init works", {
  y <- gauteng[1:50]
  q <- 0.0005

  model <- SSModelBase$new(
    Y = y,
    q = q
  )

})

