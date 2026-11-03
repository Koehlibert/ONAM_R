test_that("ONAM works", {
  skip_if_no_conda_env()
  n <- 100
  x1 <- runif(n, -2, 2)
  x2 <- runif(n, -2, 2)
  x3 <- runif(n, -2, 2)
  x4 <- runif(n, -2, 2)
  y <- sin(x1) + ifelse(x2 > 0, pweibull(x2, shape = 3), pweibull(-x2, shape = 0.5)) +
    dt(x3, 1) * 4 +
    x1 * x2
  data_train <- cbind(x1, x2, x3, x4, y)
  # Define model
  f1 <- y ~ mod1(x1) + mod1(x2) + mod1(x3) +
    mod1(x1, x2) + mod1(x1, x2, x3)
  mod1 <- function(inputs) {
    combined <- if (is.list(inputs)) layer_concatenate(inputs) else inputs
    outputs <- combined %>%
      layer_dense(units = 16, activation = "gelu") %>%
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
      f1,
      list_of_deep_models,
      data_train,
      n_ensemble = 1,
      epochs = 10,
      progresstext = FALSE,
      verbose = 0
    ),
    regexp = NA
  )
})
test_that("ONAM predict works", {
  skip_if_no_conda_env()
  n <- 100
  x1 <- runif(n, -2, 2)
  x2 <- runif(n, -2, 2)
  x3 <- runif(n, -2, 2)
  x4 <- runif(n, -2, 2)
  y <- sin(x1) + ifelse(x2 > 0, pweibull(x2, shape = 3), pweibull(-x2, shape = 0.5)) +
    dt(x3, 1) * 4 +
    x1 * x2
  data_train <- cbind(x1, x2, x3, x4, y)
  # Define model
  f1 <- y ~ mod1(x1) + mod1(x2) + mod1(x3) +
    mod1(x1, x2) + mod1(x1, x2, x3)
  mod1 <- function(inputs) {
    combined <- if (is.list(inputs)) layer_concatenate(inputs) else inputs
    outputs <- combined %>%
      layer_dense(units = 16, activation = "gelu") %>%
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
  x1 <- runif(n, -2, 2)
  x2 <- runif(n, -2, 2)
  x3 <- runif(n, -2, 2)
  x4 <- runif(n, -2, 2)
  newdat = cbind(x1, x2, x3, x4)
  expect_false(isTRUE(all.equal((predict(mod, newdat))$predictions, mod$predictions
  )))
})
test_that("ONAM works with model", {
  skip_if_no_conda_env()
  n <- 100
  x1 <- runif(n, -2, 2)
  x2 <- runif(n, -2, 2)
  y <- 2 * x1 - 2 * x2
  data_train <- cbind(x1, x2, y)

  simple_mod <- lm(y ~ ., as.data.frame(data_train))

  # Define model
  f1 <- y ~ mod1(x1) + mod1(x2) + mod1(.)
  f2 <- outcome ~ mod1(x1) + mod1(.)
  mod1 <- function(inputs) {
    combined <- if (is.list(inputs)) layer_concatenate(inputs) else inputs
    outputs <- combined %>%
      layer_dense(units = 16, activation = "gelu") %>%
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
    mod <- onam(
      f1,
      list_of_deep_models,
      data_train,
      simple_mod,
      model_data = as.data.frame(data_train),
      n_ensemble = 1,
      epochs = 10,
      progresstext = FALSE,
      verbose = 0
    ),
    regexp = NA
  )
  expect_warning(
    onam(
      f2,
      list_of_deep_models,
      data_train,
      simple_mod,
      model_data = as.data.frame(data_train),
      n_ensemble = 1,
      epochs = 10,
      progresstext = FALSE,
      verbose = 0
    ),
    regexp = "Model formula includes term of type `deep_model\\(.\\)`. Data contains column y, which has a correlation of 1 with the outcome. If using a term like `deep_model\\(.\\)` and supplying a `model` to generate the response, make sure that the original outcome is not contained in `data`."
  )
})
test_that("Binary Outcome Works", {
  skip_if_no_conda_env()
  n <- 100
  x1 <- runif(n, -2, 2)
  x2 <- runif(n, -2, 2)
  x3 <- runif(n, -2, 2)
  eta = sin(x1) + ifelse(x2 > 0, pweibull(x2, shape = 3), pweibull(-x2, shape = 0.5)) +
    dt(x3, 1) * 4 +
    x1 * x2
  y <- runif(n) < 1 / (1 + exp(eta))
  data_train <- cbind(x1, x2, x3, y)
  # Define model
  f1 <- y ~ mod1(x1) + mod1(x2) + mod1(x3) +
    mod1(x1, x2) + mod1(.)
  mod1 <- function(inputs) {
    combined <- if (is.list(inputs)) layer_concatenate(inputs) else inputs
    outputs <- combined %>%
      layer_dense(units = 16, activation = "gelu") %>%
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
    mod <- onam(
      f1,
      list_of_deep_models,
      data_train,
      target = "binary",
      n_ensemble = 1,
      epochs = 10,
      progresstext = FALSE,
      verbose = 0
    ),
    regexp = NA
  )
  glm_dat <- as.data.frame(data_train)
  logmod <- glm(y ~ x1 + x2 + x3,
                data = glm_dat,
                family = binomial())
  f2 <- glm_out ~ mod1(x1) + mod1(x2) + mod1(x3) +
    mod1(x1, x2) + mod1(.)
  expect_error(
    mod_glm <- onam(
      f2,
      list_of_deep_models,
      glm_dat,
      logmod,
      target = "binary",
      n_ensemble = 1,
      epochs = 100,
      progresstext = FALSE,
      verbose = 0
    ),
    regexp = NA
  )
  expect_warning(
    summary(mod_glm),
    regexp = NA
  )
})
