test_that("Test SSModelDynamicGompertz().estimate() works", {
  data('gauteng')
  y <- gauteng[1:50]
  model <- tsgc::SSModelDynamicGompertz$new(Y = y)
  res.new <- model$estimate()
  expect_equal(class(res.new)[1], "FilterResults")
})


test_that("Test seas.type='trigonmetric'", {
  y <- gauteng[1:50]
  model <- SSModelDynamicGompertz$new(Y = y)
  res.new <- model$estimate()
  model <- res.new$output$model
  # 1. Has seasonal component
  model_seasonal <- attr(model$terms, "specials")$SSMseasonal
  if (is.na(model_seasonal)) stop('Should have seasonal component')

  # 2. Should have 2 params without q - Ht and Seasonal
  # TODO:

})


test_that("Test seas.type='none'", {
  y <- gauteng[1:50]
  model <- SSModelDynamicGompertz$new(Y = y)
  res.new <- model$estimate()
  # TODO

})


test_that("Check that filtered states from passing NA to forecast period
equivalent to current KF recurssions", {
  q <- 0.005
  y <- gauteng[1:50]
  estimation.date.start <- index(y)[1]
  model <- SSModelDynamicGompertz$new(Y = y, q = q)
  res <- model$estimate()

  # Check that filtered states from passing NA to forecast period equivalent to
  # current
  # KF recurssions
  y.new <- gauteng[1:64]
  y.new[51:64] <- NA
  model <- SSModelDynamicGompertz$new(Y=y.new, q = q)
  res.1 <- model$estimate()
  sig.slope <- res.1$output$Ptt[2,2,]

  # Check forecast gives same values.
  n.ahead <- 14
  model <- res$output
  filtered.out <- res$predict_all(n.ahead, return.all = TRUE)
  n <- attr(model$model, "n")
  dates <- seq(as.Date(estimation.date.start), by = 'day',
                length.out = (n + n.ahead))
  y.hat.me <- xts::xts(filtered.out$y, order.by = dates) %>%
    as.xts() %>%
    subset(index(.) > tail(index(y),1))
  sig.slope.me <- filtered.out$P.t.t[2,2,]
  expect_true(all(sig.slope == sig.slope.me))

})
