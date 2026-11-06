test_that("ONAM reversibly scales inputs and outputs by default", {
  skip_if_no_conda_env()
  n <- 150
  set.seed(1)
  # Badly scaled features and outcome
  x1 <- runif(n, -2, 2) * 1e4 + 5e5
  x2 <- runif(n, -2, 2) * 1e-3
  y <- (sin(x1 / 1e4) + (x2 * 1e3) ^ 2) * 1e6 + 3e7
  data_train <- cbind(x1, x2, y)
  f1 <- y ~ mod1(x1) + mod1(x2) + mod1(x1, x2)
  mod1 <- build_dnn(c(16, 8, 1))
  list_of_deep_models <- list(mod1 = mod1)
  mod <- onam(
    f1,
    list_of_deep_models,
    data_train,
    n_ensemble = 1,
    epochs = 20,
    progresstext = FALSE,
    verbose = 0
  )
  # Scaling parameters were estimated and stored
  expect_false(is.null(mod$feature_scaling))
  expect_false(is.null(mod$output_scaling))
  expect_setequal(names(mod$feature_scaling), c("x1", "x2"))
  # `data`, predictions, and feature effects are on the original scale
  expect_equal(mod$data[, "x1"], x1)
  expect_equal(mod$data[, "x2"], x2)
  expect_true(min(mod$predictions) > 1e6)
  expect_equal(rowSums(mod$feature_effects), mod$predictions,
               ignore_attr = TRUE)
  # Plots use the original (unscaled) data
  plt <- plot_main_effect(mod, "x1")
  expect_equal(range(plt$data$x), range(x1))
})

test_that("ONAM scaling can be disabled", {
  skip_if_no_conda_env()
  n <- 100
  x1 <- runif(n, -2, 2)
  x2 <- runif(n, -2, 2)
  y <- sin(x1) + x2 + x1 * x2
  data_train <- cbind(x1, x2, y)
  f1 <- y ~ mod1(x1) + mod1(x2) + mod1(x1, x2)
  mod1 <- build_dnn(c(8, 1))
  list_of_deep_models <- list(mod1 = mod1)
  mod <- onam(
    f1,
    list_of_deep_models,
    data_train,
    n_ensemble = 1,
    epochs = 5,
    progresstext = FALSE,
    verbose = 0,
    scale_inputs = FALSE,
    scale_outputs = FALSE
  )
  expect_null(mod$feature_scaling)
  expect_null(mod$output_scaling)
  expect_equal(rowSums(mod$feature_effects), mod$predictions,
               ignore_attr = TRUE)
})

test_that("predict.onam backtransforms new data consistently", {
  skip_if_no_conda_env()
  n <- 150
  set.seed(2)
  x1 <- runif(n, -2, 2) * 1e4 + 5e5
  x2 <- runif(n, -2, 2) * 1e-3
  y <- (sin(x1 / 1e4) + (x2 * 1e3) ^ 2) * 1e6 + 3e7
  data_train <- cbind(x1, x2, y)
  f1 <- y ~ mod1(x1) + mod1(x2) + mod1(x1, x2)
  mod1 <- build_dnn(c(16, 8, 1))
  list_of_deep_models <- list(mod1 = mod1)
  mod <- onam(
    f1,
    list_of_deep_models,
    data_train,
    n_ensemble = 1,
    epochs = 20,
    progresstext = FALSE,
    verbose = 0
  )
  newdat <- data.frame(x1 = runif(30, -2, 2) * 1e4 + 5e5,
                        x2 = runif(30, -2, 2) * 1e-3)
  out <- predict(mod, newdat)
  expect_equal(out$data, newdat)
  expect_true(min(out$predictions) > 1e6)
  expect_equal(rowSums(out$feature_effects), out$predictions,
               ignore_attr = TRUE)
})
