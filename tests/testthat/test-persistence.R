test_that("Saving works", {
  skip_if_no_conda_env()
  tmp_dir <- tempfile("onam_persistence_")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
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
  mod1 <- build_dnn(c(16,8,1))
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
  save_onam(mod, tmp_dir)
  mod_loaded <- load_onam(tmp_dir)
  out <- predict(mod, data_train)
  out_loaded <- predict(mod_loaded)
  expect_equal(out$predictions, out_loaded$predictions)
  expect_equal(out$feature_effects, out_loaded$feature_effects)
})
