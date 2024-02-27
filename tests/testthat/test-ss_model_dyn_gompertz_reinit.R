test_that("Test SSModelDynGompertzReinit().estimate() works", {
  date.format <- "%Y-%m-%d"
  y <- gauteng
  estimation.date.start <- as.Date("2021-02-01")
  estimation.date.end <- as.Date("2021-06-01")
  reinit.date <- as.Date("2021-05-12", format = date.format)

  idx.est <- (zoo::index(y) >= estimation.date.start) & (zoo::index(y) <=
    estimation.date.end)
  y <- y[idx.est]

  model <- SSModelDynGompertzReinit$new(Y = y, reinit.date = reinit.date)
  res.new <- model$estimate()
  expect_equal(class(res.new)[1], "FilterResults")
})


test_that("Test estimation of (q) signal-noise works ", {
  y <- gauteng
  estimation.date.start <- as.Date("2021-02-01")
  estimation.date.end <- as.Date("2021-06-01")
  reinit.date <- as.Date("2021-05-12", format = "%Y-%m-%d")

  idx.est <- (zoo::index(y) >= estimation.date.start) & (zoo::index(y) <=
    estimation.date.end)
  y <- y[idx.est]

  # 2. Estimate model and use predict_all() for forecast.
  q <- 0.005
  model <- SSModelDynGompertzReinit$new(Y = y, q=q, reinit.date = reinit.date)
  res.with.q <- model$estimate()
  q.ssm <- res.with.q$output$model$Q[2,2,1] / res.with.q$output$model$H[,,1]
  expect_equal(q.ssm, q)
})


test_that("Test not using future information - e.g that q fixed at end of
reinit date", {
  y <- gauteng
  estimation.date.start <- as.Date("2021-02-01")
  estimation.date.end <- as.Date("2021-06-01")
  reinit.date <- "2021-05-24"

  idx.est <- (zoo::index(y) >= estimation.date.start) & (zoo::index(y) <=
    estimation.date.end)
  y <- y[idx.est]
  reinit.date

  # 2. Estimate model and use predict_all() for forecast.
  model <- SSModelDynamicGompertz$new(Y = y)
  res <- model$estimate()
  model <- SSModelDynGompertzReinit$new(Y = y, reinit.date=as.Date(
    reinit.date, "%Y-%m-%d"))
  res.reinit <- model$estimate()

  q.ssm <- res$output$model$Q[2,2,1] / res$output$model$H[,,1]
  q.reinit <- res.reinit$output$model$Q[2,2,1] / res.reinit$output$model$H[,,1]
  expect_false(abs(q.ssm - q.reinit)<0.00000001)
})


test_that("Test reinitialisation by model", {

  # 1. Params
  sea.type <- 'trigonometric'
  q <- 0.005
  # Estimation params
  estimation.date.start <- "2021-07-05"
  estimation.date.end <- "2021-11-25"
  estimation.date.reinitialisation <- as.Date("2021-11-12", format = "%Y-%m-%d")

  # Use first wave of Gauteng to test.
  y <- gauteng
  dt.ldl <- df2ldl(y)
  idx.est <- (index(y) >= estimation.date.start) & (index(y) <=
    estimation.date.end)
  dt.ldl <- dt.ldl[idx.est]
  y <- y[idx.est]

  # 1. Check that reinit with model passed is equivalent to default behaviour
  # when using pre-sample information. Thus, the results from the reinitialised
  # model with the apropriate model for initalisation should be the same.
  model <- SSModelDynGompertzReinit$new(
    Y = y, , q = q, reinit.date = estimation.date.reinitialisation,
    use.presample.info = TRUE
  )
  res <- model$estimate()

  idx.reinit <- (index(y) >= estimation.date.start) &
    (index(y) <= estimation.date.reinitialisation)
  model <- SSModelDynamicGompertz$new(Y = y[idx.reinit], q = q)
  res.full <- model$estimate()

  model <- SSModelDynGompertzReinit$new(
    Y = y, , q = q, reinit.date = estimation.date.reinitialisation,
    original.results=res.full
  )
  res.with.model.passed <- model$estimate()
  expect_equal(res.with.model.passed$output$att, res$output$att)

  # TODO: Fix or drop - should give same results if you do correction and
  #  timing correctly.
  # 2. Check reinitialising without using presample information equivalent to
  # starting a model a fresh from the reinitialisation point with a correction
  # for the level.
  model <- SSModelDynGompertzReinit$new(
    Y = y, , q = q, reinit.date = estimation.date.reinitialisation,
    use.presample.info = FALSE
  )
  res.exact.prior <- model$estimate()

  y.reinit <- reinitialise_dataframe(y, estimation.date.reinitialisation[1])
  model <- SSModelDynamicGompertz$new(Y = y.reinit, q = q)
  res.full <- model$estimate()
  # expect_equal(res.exact.prior$output$att, res.full$output$att)
})