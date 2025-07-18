#check inputs for mismatch between formula and features/models
#' @keywords internal
check_inputs_formula <-
  function(parts_list,
           list_of_deep_models,
           feature_names,
           categorical_features,
           outcome_var) {
    features_ls <- lapply(parts_list, function(x) {
      if (!as.character(x[[1]]) %in% names(list_of_deep_models)) {
        stop(
          paste0(
            "Model formula contains model ",
            x[[1]],
            ", but ",
            x[[1]],
            " is not supplied in 'list_of_deep_models'."
          ),
          call. = FALSE
        )
      }
      return(as.character(x[-1]))
    })
    features <- unlist(features_ls)
    all_symbols <- c(outcome_var, features)
    missing_features <-
      all_symbols[which(!all_symbols %in% feature_names)]
    if (length(missing_features)) {
      stop(
        paste0(
          "Feature(s) ",
          paste(missing_features, collapse = ", "),
          " in formula, but not present in data. Make sure the features align with colnames(data)."
        ),
        call. = FALSE
      )
    }
    if (any(!categorical_features %in% feature_names)) {
      missing_features <-
        categorical_features[which(!categorical_features %in% feature_names)]
      stop(
        paste0(
          paste(missing_features, collapse = ", "),
          " provided in categorical_features, but not present in data. Make sure the features align with colnames(data)."
        ),
        call. = FALSE
      )
    }
    cat_not_in_formula <-
      categorical_features[which(!categorical_features %in% all_symbols)]
    if (length(cat_not_in_formula)) {
      warning(
        paste0(
          "Feature(s) ",
          paste(cat_not_in_formula, collapse = ", "),
          " stated as categorical, but not present in model formula."
        ),
        call. = FALSE
      )
    }
    highest_term_idx <- which.max(length(features_ls))
    highest_term_features <- features_ls[[highest_term_idx]]
    if (any(unlist(lapply(features_ls[-highest_term_idx], function(terms) {
      any(!terms %in% highest_term_features)
    })))) {
      warning(
        "Features in lower order effects do not appear in higher order effects. We recommend fitting a residual term that includes all lower order terms.",
        call. = FALSE
      )
    }
  }
#check inputs for proper object specification and if specified effects were fitted
#' @keywords internal
check_inputs_plot <- function(object, effect, interaction = 0) {
  if (inherits(object, "onam_prediction")) {
    names <- colnames(object$predictions_features)
  } else if (inherits(object, "onam")) {
    names <- colnames(object$outputs_post_ensemble)
  } else {
    stop(
      "Visualization functions can only be called for objects of type 'onam'
         or 'onam_prediction'."
    )
  }
  if (!effect %in% names) {
    if (interaction == 1) {
      return(interaction)
    } else {
      stop(paste(effect,
                 " is not present in the fitted model effects.",
                 sep = ""))
    }
  }
}
#check inputs of model fitting call
#' @keywords internal
check_inputs_onam <- function(inputs) {
  if (!is(formula, "formula")) {
    stop("Parameter `formula` must be a formula object of the kind `y~x`.",
         call. = FALSE)
  }
  if (!is.list(inputs$list_of_deep_models) |
      is.null(names(inputs$list_of_deep_models)) |
      any(lapply(inputs$list_of_deep_models, function(x) {
        !is(x, "function")
      }))) {
    stop("Parameter `list_of_deep_models` must be a named list of DNN architectures.",
         call. = FALSE)
  }
  if ((!is.data.frame(inputs$data)) && (!is.matrix(inputs$data))) {
    stop("Parameter `data` must be either of class `matrix` or of class ´data.frame´",
         call. = FALSE)
  }
  if (!inputs$target %in% c("continuous", "binary")) {
    stop("Parameter `target` must be either `continuous` or `binary`.",
         call. = FALSE)
  }
  if (!is.integer(inputs$n_ensemble) | inputs$n_ensemble < 0) {
    stop("Parameter `n_ensemble` must be a positive integer.")
  }
  if (!is.integer(inputs$epochs) | inputs$epochs < 0) {
    stop("Parameter `epochs` must be a positive integer.")
  }
  if(is.na(as.logical(inputs$progresstext)))
    stop("Parameter `progresstext` must be `TRUE` or `FALSE`.")
}
