test_that("ONAM works", {
  skip_if_no_conda_env()
  n <- 100
  x1 <- runif(n,-2, 2)
  x2 <- runif(n,-2, 2)
  x3 <- runif(n,-2, 2)
  x4 <- runif(n,-2, 2)
  y <- sin(x1) + ifelse(x2 > 0, pweibull(x2, shape = 3),
                        pweibull(-x2, shape = 0.5)) +
    dt(x3, 1) * 4 +
    x1 * x2
  data_train <- cbind(x1, x2, x3, x4, y)
  # Define model
  f1 <- y ~ mod1(x1) + mod1(x2) + mod1(x3) +
    mod1(x1, x2) + mod1(x1, x2, x3)
  mod1 <- function(inputs) {
    outputs <- inputs %>%
      layer_dense(units = 16, activation = "relu") %>%
      layer_dense(units = 8,
                  activation = "linear",
                  use_bias = TRUE) %>%
      layer_dense(units = 1,
                  activation = "linear",
                  use_bias = TRUE)
    keras_model(inputs, outputs)
  }
  list_of_deep_models <- list(mod1 = mod1)
  # Fit model
  mod <- onam(
    f1,
    list_of_deep_models,
    data_train,
    n_ensemble = 1,
    epochs = 10,
    progresstext = FALSE,
    verbose = 0
  )
})
test_that("Input checks work", {
  skip_if_no_conda_env()
  n <- 100
  x1 <- runif(n,-2, 2)
  x2 <- runif(n,-2, 2)
  x3 <- runif(n,-2, 2)
  x4 <- runif(n,-2, 2)
  y <- sin(x1) + ifelse(x2 > 0, pweibull(x2, shape = 3),
                        pweibull(-x2, shape = 0.5)) +
    dt(x3, 1) * 4 +
    x1 * x2
  data_train <- cbind(x1, x2, x3, x4, y)
  # Define model
  f1 <- y ~ mod1(x1) + mod1(x2) + mod1(x3) +
    mod1(x1, x2) + mod1(x1, x2, x3)
  f2 <- y ~ mod1(x1) + mod1(x2) + mod1(x3) +
    mod1(x1, x2) + mod1(x1, x2, x3) + mod1(x5)
  f3 <- y ~ mod1(x1) + mod1(x2) + mod1(x3) +
    mod1(x1, x2) + mod1(x1, x2, x3) + mod4(x2)
  f4 <- y ~ mod1(x1) + mod1(x2) + mod1(x3) +
    mod1(x1, x2)
  mod1 <- function(inputs) {
    outputs <- inputs %>%
      layer_dense(units = 16, activation = "relu") %>%
      layer_dense(units = 8,
                  activation = "linear",
                  use_bias = TRUE) %>%
      layer_dense(units = 1,
                  activation = "linear",
                  use_bias = TRUE)
    keras_model(inputs, outputs)
  }
  list_of_deep_models <- list(mod1 = mod1)
  # Fit model
  expect_error(
    onam(
      f2,
      list_of_deep_models,
      data_train,
      n_ensemble = 1,
      epochs = 10,
      progresstext = FALSE,
      verbose = 0
    ),
    regexp = "Feature\\(s\\) x5 in formula, but not present in data. Make sure the features align with colnames\\(data\\).",
  )
  expect_error(
    onam(
      f3,
      list_of_deep_models,
      data_train,
      n_ensemble = 1,
      epochs = 10,
      progresstext = FALSE,
      verbose = 0
    ),
    regexp = "Model formula contains model mod4, but mod4 is not supplied in 'list_of_deep_models'.",
  )
  expect_error(
    onam(
      f1,
      list_of_deep_models,
      data_train,
      categorical_features = c("x5"),
      n_ensemble = 1,
      epochs = 10,
      progresstext = FALSE,
      verbose = 0
    ),
    regexp = "x5 provided in categorical_features, but not present in data. Make sure the features align with colnames\\(data\\).",
  )
  expect_warning(
    onam(
      f4,
      list_of_deep_models,
      data_train,
      n_ensemble = 1,
      epochs = 10,
      progresstext = FALSE,
      verbose = 0
    ),
    regexp = "Features in lower order effects do not appear in higher order effects. We recommend fitting a residual term that includes all lower order terms.",
  )
  expect_warning(
    onam(
      f1,
      list_of_deep_models,
      data_train,
      categorical_features = c("x4"),
      n_ensemble = 1,
      epochs = 10,
      progresstext = FALSE,
      verbose = 0
    ),
    regexp = "Feature\\(s\\) x4 stated as categorical, but not present in model formula.",
  )
  mod <- onam(
    f1,
    list_of_deep_models,
    data_train,
    n_ensemble = 1,
    epochs = 10,
    progresstext = FALSE,
    verbose = 0
  )
  expect_error(plot_main_effect(mod, "x4"),
               "x4 is not present in the fitted model effects.")
  expect_error(plot_inter_effect(mod, "x1", "x3"),
               "x3_x1 is not present in the fitted model effects.")
})
