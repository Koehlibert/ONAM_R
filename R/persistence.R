#' Save a fitted onam model to disk
#'
#' @description
#' An `onam` object holds one or more fitted 'keras' submodels per ensemble
#' member (in `object$ensemble[[i]]$model_list`), which cannot be
#' serialized with `saveRDS()` alone. This function saves each submodel to
#' its own `.keras` file and the remaining orthogonalization metadata
#' (weights, `model_info`, data, predictions, ...) to a single `.rds` file,
#' all inside `dir`. Use [load_onam] to restore the object.
#'
#' @param object Object of class `onam` as returned by [onam].
#' @param dir Directory the model is saved to. Created if it does not exist.
#' @param overwrite Should an existing directory of the same name be
#' overwritten?
#' @returns No return value, called for side effects.
#' @seealso [load_onam]
#' @export
save_onam <- function(object, dir, overwrite = FALSE) {
  stopifnot(inherits(object, "onam"))
  if (dir.exists(dir)) {
    if (!overwrite) {
      stop("`dir` already exists. Set `overwrite = TRUE` to overwrite.",
           call. = FALSE)
    }
  } else {
    dir.create(dir, recursive = TRUE)
  }
  submodel_dir <- file.path(dir, "submodels")
  dir.create(submodel_dir, showWarnings = FALSE)
  n_ensemble <- length(object$ensemble)
  for (i in seq_len(n_ensemble)) {
    orders <- names(object$ensemble[[i]]$model_list)
    for (order in orders) {
      model_indices <- seq_along(object$ensemble[[i]]$model_list[[order]])
      for (model_idx in model_indices)
      {
        keras3::save_model(
          object$ensemble[[i]]$model_list[[order]][[model_idx]],
          file.path(
            submodel_dir,
            paste0("ensemble", i, "_", order, "_", model_idx, ".keras")
          ),
          overwrite = TRUE
        )
      }
    }
    # Keras models can't round-trip through saveRDS; keep the order names
    # and how many models each order holds so load_onam() knows which
    # files to reload and where they go.
    model_counts <- vapply(orders, function(order) {
      length(object$ensemble[[i]]$model_list[[order]])
    }, integer(1))
    names(model_counts) <- orders
    object$ensemble[[i]]$model_list <- model_counts
  }
  saveRDS(object, file.path(dir, "onam_meta.rds"))
  invisible(NULL)
}
#' Load a fitted onam model from disk
#'
#' @param dir Directory as created by [save_onam].
#' @returns Returns the restored object of class `onam`.
#' @seealso [save_onam]
#' @export
load_onam <- function(dir) {
  if (!require_keras()) {
    invisible(return(NULL))
  }
  meta_path <- file.path(dir, "onam_meta.rds")
  if (!file.exists(meta_path)) {
    stop("No onam model found in `dir`.", call. = FALSE)
  }
  object <- readRDS(meta_path)
  submodel_dir <- file.path(dir, "submodels")
  n_ensemble <- length(object$ensemble)
  for (i in seq_len(n_ensemble)) {
    model_counts <- object$ensemble[[i]]$model_list
    orders <- names(model_counts)
    model_list <- lapply(orders, function(order) {
      lapply(seq_len(model_counts[[order]]), function(model_idx) {
        keras3::load_model(file.path(
          submodel_dir,
          paste0("ensemble", i, "_", order, "_", model_idx, ".keras")
        ))
      })
    })
    names(model_list) <- orders
    object$ensemble[[i]]$model_list <- model_list
  }
  object
}
