#Save each keras submodel in a nested `model_list` to its own `.keras` file
#inside `dir`. Returns file paths instead of model objects (result is sent
#back from a parallel worker because keras models cannot be serialized with
#base R serialization) and reloaded with `load_model_list()`.
save_model_list <- function(model_list, dir) {
  orders <- names(model_list)
  paths <- lapply(orders, function(order) {
    lapply(seq_along(model_list[[order]]), function(idx) {
      path <- file.path(dir, paste0(order, "_", idx, ".keras"))
      keras3::save_model(model_list[[order]][[idx]], path, overwrite = TRUE)
      path
    })
  })
  names(paths) <- orders
  paths
}
#Inverse of `save_model_list()`: reload the keras submodels referenced by
#nested list of file paths into same nested structure of model objects.
load_model_list <- function(paths) {
  orders <- names(paths)
  model_list <- lapply(orders, function(order) {
    lapply(paths[[order]], keras3::load_model)
  })
  names(model_list) <- orders
  model_list
}
#Build, fit and orthogonalize a single ensemble member. Used both directly
#and as the worker function dispatched to parallel workers by
#`fit_ensemble_parallel()`. When `tmp_dir` is supplied, the member's keras
#submodels are saved to disk and `model_list` in the result holds file paths
#instead of live model objects.
fit_ensemble_member <- function(i,
                                model_info,
                                list_of_deep_models,
                                categorical_features,
                                cat_counts,
                                target,
                                learning_rate,
                                data_fit,
                                y_fit,
                                epochs,
                                callback,
                                verbose,
                                seed = NULL,
                                tmp_dir = NULL) {
  if (!is.null(tmp_dir)) {
    require_keras()
  }
  if (!is.null(seed)) {
    keras3::set_random_seed(seed + i)
  }
  time_member_start <- Sys.time()
  model_object <-
    create_model(
      model_info,
      list_of_deep_models,
      categorical_features,
      cat_counts,
      target,
      learning_rate
    )
  model_whole <- model_object$model
  model_list <- model_object$model_list
  model_whole %>%
    keras3::fit(
      data_fit,
      y_fit,
      epochs = epochs,
      callbacks = callback,
      verbose = verbose
    )
  time_training <-
    as.numeric(difftime(Sys.time(), time_member_start, units = "secs"))
  time_ortho_start <- Sys.time()
  pho_result <- pho(model_list, model_info, data_fit)
  time_orthogonalization <-
    as.numeric(difftime(Sys.time(), time_ortho_start, units = "secs"))
  independent_columns <- pho_result$independent_columns
  pho_result$independent_columns <- NULL
  if (!is.null(tmp_dir)) {
    member_dir <- file.path(tmp_dir, paste0("member_", i))
    dir.create(member_dir, showWarnings = FALSE, recursive = TRUE)
    pho_result$model_list <- save_model_list(pho_result$model_list, member_dir)
  }
  list(
    pho_result = pho_result,
    independent_columns = independent_columns,
    time_training = time_training,
    time_orthogonalization = time_orthogonalization
  )
}
#Fit all `n_ensemble` members in parallel across `n_cores` worker processes
#using `parallel::makeCluster()`. Each model and its submodels are saved to a
#temporary directory, since keras model objects cannot be returned to the main
#process; the main process reloads them and cleans up the temporary files.
fit_ensemble_parallel <- function(n_ensemble,
                                  n_cores,
                                  model_info,
                                  list_of_deep_models,
                                  categorical_features,
                                  cat_counts,
                                  target,
                                  learning_rate,
                                  data_fit,
                                  y_fit,
                                  epochs,
                                  callback,
                                  verbose,
                                  seed,
                                  progresstext) {
  n_cores <- min(n_cores, n_ensemble)
  if (progresstext) {
    cat(
      paste0(
        "Fitting ",
        n_ensemble,
        " ensemble members using ",
        n_cores,
        " parallel workers...\n"
      )
    )
  }
  tmp_dir <- tempfile("onam_ensemble_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  cl <- parallel::makeCluster(n_cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  pkg_env <- environment(fit_ensemble_member)
  parallel::clusterExport(
    cl,
    varlist = ls(pkg_env, all.names = TRUE),
    envir = pkg_env
  )
  #`list_of_deep_models` architectures typically call 'keras3' layer
  #functions, so 'keras3' must be attached on each worker.
  parallel::clusterEvalQ(cl, {
    library(keras3)
    NULL
  })
  results <- parallel::parLapply(
    cl,
    seq_len(n_ensemble),
    fit_ensemble_member,
    model_info = model_info,
    list_of_deep_models = list_of_deep_models,
    categorical_features = categorical_features,
    cat_counts = cat_counts,
    target = target,
    learning_rate = learning_rate,
    data_fit = data_fit,
    y_fit = y_fit,
    epochs = epochs,
    callback = callback,
    verbose = verbose,
    seed = seed,
    tmp_dir = tmp_dir
  )
  ensemble <- lapply(results, function(res) {
    res$pho_result$model_list <- load_model_list(res$pho_result$model_list)
    res$pho_result
  })
  list(
    ensemble = ensemble,
    time_training = vapply(results, function(res)
      res$time_training, numeric(1)),
    time_orthogonalization_members = vapply(results, function(res)
      res$time_orthogonalization, numeric(1)),
    independent_columns_members = lapply(results, function(res)
      res$independent_columns)
  )
}
