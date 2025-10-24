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
    all_symbols <- c(features)
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
  if (inherits(object, "onam_prediction") |
      inherits(object, "onam")) {
    names <- colnames(object$feature_effects)
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
  if (!inherits(inputs$formula, "formula")) {
    stop("Parameter `formula` must be a formula object of the kind `y~x`.",
         call. = FALSE)
  }
  if (!is.list(inputs$list_of_deep_models) |
      is.null(names(inputs$list_of_deep_models)) |
      any(unlist(lapply(inputs$list_of_deep_models, function(x) {
        !inherits(x, "function")
      })))) {
    stop("Parameter `list_of_deep_models` must be a named list of DNN architectures.",
         call. = FALSE)
  }
  if ((!is.data.frame(inputs$data)) && (!is.matrix(inputs$data))) {
    stop("Parameter `data` must be either of class `matrix` or of class `data.frame`",
         call. = FALSE)
  }
  if (!inputs$target %in% c("continuous", "binary")) {
    stop("Parameter `target` must be either `continuous` or `binary`.",
         call. = FALSE)
  }
  if (!(inputs$n_ensemble %% 1 == 0) | inputs$n_ensemble < 0) {
    stop("Parameter `n_ensemble` must be a positive integer.",
         call. = FALSE)
  }
  if (!(inputs$epochs %% 1 == 0) | inputs$epochs < 0) {
    stop("Parameter `epochs` must be a positive integer.",
         call. = FALSE)
  }
  if (is.na(as.logical(inputs$progresstext)))
    stop("Parameter `progresstext` must be `TRUE` or `FALSE`.",
         call. = FALSE)
}
check_y_features <- function(data, y, model_info) {
  if (model_info$all_feature_indic) {
    data_no_outcome <-
      data[,!(colnames(data) %in% as.character(model_info$outcome))]
    f_y_cors <- apply(data_no_outcome, 2, function(x)
      stats::cor(x, y))
    cor_orders <- order(f_y_cors, decreasing = TRUE)
    if (f_y_cors[cor_orders[1]] > 0.99) {
      warning(
        paste0(
          "Model formula includes term of type `deep_model(.)`. Data contains column ",
          colnames(data_no_outcome)[cor_orders[1]],
          ", which has a correlation of ",
          round(f_y_cors[cor_orders[1]], 4),
          " with the outcome. If using a term like `deep_model(.)` and supplying a `model` to generate the response, make sure that the original outcome is not contained in `data`."
        ),
        call. = FALSE
      )
    }
  }
}
require_keras <- function() {
  if(!reticulate::py_available())
  {
    message("No Python Environemt available. Use install.keras() ",
            "to install recommended environment.")
    return(FALSE)
  }
  if(!reticulate::py_module_available("tensorflow"))
  {
    message("Tensorflow not available. Use install_keras().")
    return(FALSE)
  }
  TRUE
}
