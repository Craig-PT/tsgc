test_that("Test plot_new_cases() works", {
  data(gauteng, package = "tsgc")
  model <- tsgc::SSModelDynamicGompertz$new(Y = gauteng[1:50], q = 0.005)
  res <- model$estimate()
  tsgc::plot_new_cases(
    res, Y=gauteng, n.ahead=14, confidence.level=0.68, date_format="%Y-%m-%d"
  )

})


test_that("Test plot_forecast() works", {
  data(gauteng, package = "tsgc")
  model <- tsgc::SSModelDynamicGompertz$new(Y = gauteng[1:50], q = 0.005)
  res <- model$estimate()
  tsgc::plot_forecast(
    res=res, y.eval = tsgc::df2ldl(gauteng[50:65]), n.ahead = 14,
    caption = 'hello', title = 'test'
  )
})


test_that("Test plot_gy_components() works", {
  data(gauteng, package = "tsgc")
  model <- tsgc::SSModelDynamicGompertz$new(Y = gauteng[1:50], q = 0.005)
  res <- model$estimate()
  # Plot filtered gy, g and gamma
  plot_gy_components(res)
})


test_that("Test plot_gy_ci() works", {
  data(gauteng, package = "tsgc")
  model <- tsgc::SSModelDynamicGompertz$new(Y = gauteng[1:50], q = 0.005)
  res <- model$estimate()
  plot_gy_ci(res)
})


test_that("Test plot_holdout() works", {
  data(gauteng, package = "tsgc")
  model <- tsgc::SSModelDynamicGompertz$new(Y = gauteng[1:50], q = 0.005)
  idx.est <- zoo::index(gauteng) <= as.Date("2020-07-20")
  idx.eval <- (zoo::index(gauteng) >= as.Date("2020-07-20")) &
    zoo::index(gauteng) <= as.Date("2020-07-27")
  model <- SSModelDynamicGompertz$new(Y = gauteng[idx.est], q = 0.005)
  res <- model$estimate()
  # Plot forecasts and outcomes over evaluation period
  plot_holdout(res = res, Y = gauteng[idx.est], Y.eval = gauteng[idx.eval])
})
