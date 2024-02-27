test_that("Test FilterResults().predict_level() works", {
  data('gauteng')
  y <- gauteng[1:50]
  n.ahead <- 14
  model <- tsgc::SSModelDynamicGompertz$new(Y = y)
  res <- model$estimate()
  expect_true(inherits(res, "FilterResults"))
  out <- res$predict_level(
    y.cum = y,
    n.ahead = n.ahead,
    confidence_level = 0.68,
    return.diff = TRUE
  )
})


test_that("Test FilterResults().print_estimation_results() works", {
  data('gauteng')
  y <- gauteng[1:50]
  n.ahead <- 14
  model <- tsgc::SSModelDynamicGompertz$new(Y = y)
  res <- model$estimate()
  out <- res$print_estimation_results()
})


test_that("Test FilterResults().predict_all() works", {
  data('gauteng')
  y <- gauteng[1:50]
  n.ahead <- 14
  model <- tsgc::SSModelDynamicGompertz$new(Y = y)
  res <- model$estimate()
  y.hat.all <- res$predict_all(n.ahead, return.all = TRUE)
})


test_that("Test FilterResults().predict_level() works", {
  data('gauteng')
  y <- gauteng[1:50]
  n.ahead <- 14
  model <- tsgc::SSModelDynamicGompertz$new(Y = y)
  res <- model$estimate()
  y.hat.all <- res$predict_level(
    y.cum = y, n.ahead = n.ahead, confidence_level = 0.68, sea.on=TRUE,
    return.diff = TRUE)
  # TODO: Check that there is data in the forecast period.
})




test_that("Test FilterResults().get_growth_y() works", {
  data('gauteng')
  y <- gauteng[1:50]
  n.ahead <- 14
  model <- tsgc::SSModelDynamicGompertz$new(Y = y)
  res <- model$estimate()
  g.y.t.t <- res$get_growth_y(return.components = TRUE)
})


test_that("Test predict_all() gives same results as KFAS", {
  data('gauteng')
  q <- 0.005
  n.ahead <- 14
  y <- gauteng[1:50]
  estimation.date.start <- index(y)[1]

  y.new <- gauteng[1:64]
  y.new[51:64] <- NA
  model <- tsgc::SSModelDynamicGompertz$new(Y = y, q = q)
  res <- model$estimate()
  model <- tsgc::SSModelDynamicGompertz$new(Y=y.new, q = q)
  res.kfas <- model$estimate()
  filtered.out <- res$predict_all(n.ahead = 14, return.all = TRUE,
                                  sea.on = TRUE)

  expect_true(all(res.kfas$output$att == filtered.out$a.t.t))
  expect_equal(res.kfas$output$Ptt, filtered.out$P.t.t)

  # A solution - Extract model - extend y, change n ensure int then do the
  # KFS again.
  new.model <- res$output$model
  new.model$y <- rbind(
    new.model$y,
    matrix(NA, ncol = ncol(new.model$y), nrow = n.ahead) %>% as.ts()
  )
  attr(new.model, 'n') <- 64 %>% as.integer()
  model_output <- KFS(new.model)
  expect_equal(model_output$Ptt, filtered.out$P.t.t, tolerance = 1e-10)

  Zt <- drop(res.kfas$output$model$Z)
  Tt <- drop(res.kfas$output$model$T)
  y.t.t <- drop(Zt %*% t(res.kfas$output$att))
  expect_equal(y.t.t[51:64] %>% as.numeric(), filtered.out$y[51:64] %>%
    as.numeric())

  # Can use predict on non-extended y.
  y.hat.kfas <- predict(
    res$output$model, interval = c('prediction'), n.ahead = n.ahead,
    level = 0.68, states = c('all')
  )
  dates <- seq(tail(res.kfas$index,1) + 1, by = 'day', length.out = n.ahead)
  y.hat.kfas <- xts(y.hat.kfas[,1], order.by = dates)
  expect_equal(y.hat.kfas %>% as.numeric(), filtered.out$y[51:64] %>%
    as.numeric(), tolerance = 1e-10)

})


test_that("Test predict_all() yields same results as predict() with NA
in fcast period", {
  data('gauteng')
  q <- 0.005
  n.ahead <- 14
  y <- gauteng[1:50]
  estimation.date.start <- index(y)[1]

  # 2. Estimate model and use predict_all() for forecast.
  model <- tsgc::SSModelDynamicGompertz$new(Y = y, q = q)
  res <- model$estimate()
  y.hat.all <- res$predict_all(n.ahead, return.all = TRUE, sea.on = TRUE)

  y.hat <- y.hat.all$y.hat %>%
    subset(index(.) > tail(res$index, 1))
  level.t.t <- y.hat.all$level.t.t
  slope.t.t <- y.hat.all$slope.t.t

  # 3. Get filtered states from passing NA to forecast period equivalent to
  # current KF recurssions
  y.new <- gauteng[1:64]
  y.new[51:64] <- NA
  model <- tsgc::SSModelDynamicGompertz$new(Y=y.new, q = q)
  res.kfas <- model$estimate()
  level.t.t.kfas <- res.kfas$output$att[,1]
  slope.t.t.kfas <- res.kfas$output$att[,2]
  sig.slope.t.t.kfas <- res.kfas$output$Ptt[2,2,]

  # a. Check level and slope are the same from model with NA in forecast
  # observations and our KF recursions.
  expect_true(all(slope.t.t %>% as.numeric() == slope.t.t.kfas))
  expect_true(all(level.t.t == as.matrix(level.t.t.kfas)))

  # b. Check forecast y same as that obtained from kfas::predict()
  y.hat.kfas <- predict(
    res$output$model, interval = c('prediction'), n.ahead = n.ahead,
    level = 0.68, states = c('all')
  )
  dates <- seq(tail(res$index,1) + 1, by = 'day', length.out = n.ahead)
  y.hat.kfas <- xts(y.hat.kfas[,1], order.by = dates)
  expect_true(all(y.hat == y.hat.kfas))
})


