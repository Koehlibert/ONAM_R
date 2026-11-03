test_that("Orthogonalization works", {
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
  eff_mat <- mod$feature_effects
  #Mean Centering works: every effect is centered, intercept carries the mean
  non_intercept <- setdiff(colnames(eff_mat), "Intercept")
  expect_all_equal(colMeans(eff_mat[, non_intercept]), 0)
  #Effects are orthogonal
  var_mat <- var(eff_mat)
  expect_all_equal(c(var_mat[-1, 1], var_mat[3:nrow(var_mat), 2]), 0)
})
test_that("Seed works", {
  skip_if_no_conda_env()
  n <- 100
  x1 <- runif(n, -2, 2)
  x2 <- runif(n, -2, 2)
  y <- 2 * x1 - 2 * x2
  data_train <- cbind(x1, x2, y)

  # Define model
  f1 <- y ~ mod1(x1) + mod1(x2) + mod1(.)
  mod1 <- function(inputs) {
    combined <- if (is.list(inputs)) layer_concatenate(inputs) else inputs
    outputs <- combined %>%
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
    mod <- onam(
      f1,
      list_of_deep_models,
      data_train,
      n_ensemble = 1,
      epochs = 10,
      progresstext = FALSE,
      seed = 1,
      verbose = 0
    ),
    regexp = NA
  )
  tmp_eff <- mod$feature_effects[, "x1"]
  mod2 <- onam(
    f1,
    list_of_deep_models,
    data_train,
    n_ensemble = 1,
    epochs = 10,
    progresstext = FALSE,
    seed = 1,
    verbose = 0
  )
  expect_equal(mod2$feature_effects[, "x1"], tmp_eff)
})
extract_dense_specs <- function(model) {
  dense_layers <- Filter(
    function(l) grepl("Dense", class(l)[1], ignore.case = TRUE),
    model$layers
  )
  lapply(dense_layers, function(l) {
    cfg <- l$get_config()
    list(
      units      = cfg$units,
      activation = cfg$activation,
      use_bias   = cfg$use_bias
    )
  })
}
test_that("build_dnn works", {
  skip_if_no_conda_env()
  inputs <- layer_input(shape = c(10))
  mod1 <- function(inputs) {
    outputs <- inputs %>%
      layer_dense(units = 16,
                  activation = "gelu",
                  use_bias = TRUE) %>%
      layer_dense(units = 8,
                  activation = "gelu",
                  use_bias = TRUE) %>%
      layer_dense(units = 1,
                  activation = "linear",
                  use_bias = TRUE)
    keras_model(inputs, outputs)
  }
  built_mod1 <- mod1(inputs)
  mod2 <- build_dnn(units = c(16, 8, 1))
  built_mod2 <- mod2(inputs)
  reference_specs <- extract_dense_specs(built_mod1)
  built_specs     <- extract_dense_specs(built_mod2)
  #Same number of layers
  expect_length(built_specs, length(reference_specs))
  # Same units / activation / use_bias, layer by layer
  expect_equal(built_specs, reference_specs)
  # Shapes and parameter counts
  expect_equal(built_mod2$output_shape, built_mod1$output_shape)
  expect_equal(built_mod2$count_params(), built_mod1$count_params())
  # DF parametrization
  mod3 <- function(inputs) {
    outputs <- inputs %>%
      layer_dense(units = 32,
                  activation = "gelu",
                  use_bias = FALSE) %>%
      layer_dense(units = 16,
                  activation = "gelu",
                  use_bias = FALSE) %>%
      layer_dense(units = 8,
                  activation = "gelu",
                  use_bias = FALSE) %>%
      layer_dense(units = 1,
                  activation = "linear",
                  use_bias = TRUE)
    keras_model(inputs, outputs)
  }
  built_mod3 <- mod3(inputs)
  mod4 <- build_dnn(layer_df = data.frame(
    units = c(32, 16, 8, 1),
    activation = c("gelu", "gelu", "gelu", "linear"),
    use_bias = c(FALSE, FALSE, FALSE, TRUE)
  ))
  built_mod4 <- mod4(inputs)
  reference_specs <- extract_dense_specs(built_mod3)
  built_specs     <- extract_dense_specs(built_mod4)
  #Same number of layers
  expect_length(built_specs, length(reference_specs))
  # Same units / activation / use_bias, layer by layer
  expect_equal(built_specs, reference_specs)
  # Shapes and parameter counts
  expect_equal(built_mod2$output_shape, built_mod1$output_shape)
  expect_equal(built_mod2$count_params(), built_mod1$count_params())
})
