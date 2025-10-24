#' Fit orthogonal neural additive model
#'
#' @description
#' Fits an interpretable neural additive model with post hoc orthogonalization
#' for a given network architecture and user-specified feature sets.
#'
#' @param formula Formula for model fitting. Specify deep parts with the same
#' name as `list_of_deep_models`.
#' @param list_of_deep_models List of named models used in `model_formula`.
#' @param data Data to be fitted
#' @param model Prediction model that is to be explained. Output of the model as
#' returned from `prediction_function(model)` will be used as model output. If
#' `NULL`(default), the outcome has to be present in `data`.
#' @param prediction_function Prediction function to be used to generate the
#' outcome. Only used if `model` is specified. If `NULL`(default), S3-method
#' based on the `model`argument is used.
#' @param model_data Data used for generating predictions of `model`. Necessary
#' for some models that require specific data formats, i.e. xgboost.
#' If `NULL`(default), `data` is used. Only used if `model` is specified.
#' @param categorical_features Vector of feature names of categorical features.
#' @param target Target of prediction task. Can be either "continuous" or
#' "binary". For "continuous"(default), an additive model for the prediction of
#' a continuous outcome is fitted. For "binary", a binary classification with
#' sigmoid activation in the last layer is fitted.
#' @param epochs Number of epochs to train the model. See
#' \code{\link[keras3]{fit}} for details.
#' @param n_ensemble Number of orthogonal neural additive model ensembles
#' @param callback Callback to be called during training. See
#' \code{\link[keras3]{fit}} for details.
#' @param progresstext Show model fitting progress. If `TRUE`, shows current
#' number of ensemble being fitted
#' @param verbose Verbose argument for internal model fitting. used for
#' debugging. See \code{\link[keras3]{fit}} for
#' details.
#' @importFrom keras3 fit
#' @returns Returns a model object of class \code{onam}, containing all ensemble
#' members, ensemble weights, and main and interaction effect outputs.
#' @examplesIf reticulate::py_module_available(tensorflow)
#' \donttest{
#' # Basic example for a simple ONAM-model
#' # Create training data
#' n <- 1000
#' x1 <- runif(n, -2, 2)
#' x2 <- runif(n, -2, 2)
#' y <- sin(x1) + ifelse(x2 > 0, pweibull(x2, shape = 3),
#'   pweibull(-x2, shape = 0.5)) +
#'   x1 * x2
#' data_train <- cbind(x1, x2, y)
#' # Define model
#' model_formula <- y ~ mod1(x1) + mod1(x2) +
#'   mod1(x1, x2)
#' list_of_deep_models <- list(mod1 = ONAM:::get_submodel)
#' # Fit model
#' mod <- onam(model_formula, list_of_deep_models,
#'                    data_train, n_ensemble = 2, epochs = 10)
#' summary(mod)
#' }
#' @export onam
onam <- function(formula,
                 list_of_deep_models,
                 data,
                 model = NULL,
                 prediction_function = NULL,
                 model_data = NULL,
                 categorical_features = NULL,
                 target = "continuous",
                 n_ensemble = 10,
                 epochs = 500,
                 callback = NULL,
                 progresstext = FALSE,
                 verbose = 0) {
  if(!require_keras()) {
    invisible(return(NULL))
  }
  inputs <- as.list(environment())
  check_inputs_onam(inputs)
  feature_names <- colnames(data)
  model_info <-
    get_theta(formula,
              list_of_deep_models,
              feature_names,
              categorical_features,
              target)
  cat_counts <- get_category_counts(categorical_features,
                                    data)
  y <-
    get_output(model,
               prediction_function,
               model_data,
               data,
               target,
               model_info)
  check_y_features(data, y, model_info)
  data_fit <-
    prepare_data(data, model_info, categorical_features)
  ensemble <- list()
  for (i in 1:n_ensemble) {
    if (progresstext) {
      show_progress(i, n_ensemble)
    }
    model_object <-
      create_model(model_info,
                   list_of_deep_models,
                   categorical_features,
                   cat_counts,
                   target)
    model_whole <- model_object$model
    model_list <- model_object$model_list
    #Fit model####
    # callback <-
    #   keras3::keras$callbacks$EarlyStopping(monitor = "loss",
    #                                        patience = 10)
    history <- model_whole %>%
      keras3::fit(
        data_fit,
        y,
        epochs = epochs,
        callbacks = callback,
        verbose = verbose
      )
    #Orthogonalize####
    ensemble[[i]] <-
      pho(model_list, model_info, data_fit)
  }
  model_list_pho <- list(
    ensemble = ensemble,
    model_info = model_info,
    data = data,
    categorical_features = categorical_features,
    input = model_whole$input
  )
  data_model_eval <-
    evaluate_onam_pre(model_list_pho)
  pho_ensemble_list <- pho_ensemble(data_model_eval, model_info)
  w_post_ensemble <- pho_ensemble_list[[1]]
  outputs_post_ensemble <- pho_ensemble_list[[2]]
  out <- c(
    model_list_pho,
    call = match.call(),
    w_post_ensemble = list(w_post_ensemble),
    feature_effects = list(outputs_post_ensemble),
    predictions = list(rowSums(outputs_post_ensemble)),
    y = list(y)
  )
  class(out) <- "onam"
  out
}
#' Get summary of an onam object
#'
#' @description
#' generates a summary of a fitted `onam` object including
#' information on ensembling strategy and performance metrics such as
#' correlation and degree of interpretabiltity
#'
#' @param object onam object of class `onam` as returned from
#' \code{\link{onam}} to be summarized
#' @param x object of class \code{\link{summary.onam}}.
#' @param ... further arguments passed to or from other methods.
#' @returns Gives summary of the `onam` object, including model inputs, number
#' of ensembles, correlation of model output and original outcome variable, and
#' interpretability metrics i_1 and i_2
#' @details
#' For examples see \code{\link[=onam]{example(onam)}}
#' @method summary onam
#' @export
summary.onam <- function(object, ...) {
  var_decomp <- decompose(object)$var_decomp
  convergence_metric <-
    if (object$model_info$target == "continuous") {
      stats::cor(object$predictions,
                 object$y)
    } else {
      pROC::auc(object$y,
                object$predictions)
      # stats::cor(object$predictions,
      #            object$y, method = "spearman")
    }
  res <- list(
    call = object$call,
    n_ensemble = length(object$ensemble),
    conv_metric = convergence_metric,
    i_1 = var_decomp["1"],
    i_2 = var_decomp["2"],
    degree_expl = sum(var_decomp[c("1", "2")])
  )
  class(res) <- "summary.onam"
  attr(res, "target") <- object$model_info$target
  res
}

#' @rdname summary.onam
#' @method print summary.onam
#' @export
print.summary.onam <- function(x, ...) {
  cat("\nCall:\n")
  print(x$call)
  # cat("\nInputs:")
  # input_string_1 <- as.character(x$input[[1]])
  # if (input_string_1 == "<pointer: 0x0>") {
  #   cat(
  #     "\nModel fitted in previous R session; Input information only available in
  #     session in which model was built.\n")
  # } else {
  #   lapply(x$input, function(input) cat("\n", as.character(input)))
  # }
  if (attr(x, "target") == "continuous") {
    cat("\nCorrelation of model prediction with outcome variable: ",
        round(x$conv_metric, 4),
        sep = "")
  } else {
    cat("\nPrediction AUC: ",
        round(x$conv_metric, 4),
        sep = "")
    # cat("\nCorrelation of onam probabilities with original model predicted probabilities: ",
    #     round(x$conv_metric, 4),
    #     sep = "")
  }
  cat("\nNumber of ensemble members: ", x$n_ensemble)
  cat("\nI_1: ",
      round(x$i_1, digits = 4),
      "; I_2:  ",
      round(x$i_2, digits = 4),
      sep = "")
  cat("\nDegree of interpretability: ",
      round(x$degree_expl, digits = 4),
      sep = "")
  invisible(x)
}
