#' @importFrom rlang .data
evaluate_onam_single <- function(model,
                                 data,
                                 model_info,
                                 categorical_features) {
  data_fit <- prepare_data(data, model_info,
                           categorical_features)
  model_idx_list <- get_model_idx_list(model_info)
  u_object <-
    get_u(model$model_list, model_idx_list, model_info, data_fit)
  predictions_submodel <-
    lapply(model$w_list,
           function(w)
             u_object$u %*% w)
  predictions_submodel_old <-
    lapply(model$w_list_old,
           function(w)
             u_object$u %*% w)
  model_names <- list()
  for (idx_order in seq_along(model_info$theta)) {
    for (idx_model in seq_along(model_info$theta[[idx_order]])) {
      tmp_name <-
        paste(unlist(model_info$theta[[idx_order]][[idx_model]]),
              collapse = "_")
      if (names(model_info$theta)[idx_order] == "Linear") {
        tmp_name <- paste(tmp_name, "_Linear", sep = "")
      }
      model_names <- c(model_names, tmp_name)
    }
  }
  names(predictions_submodel) <- model_names
  names(predictions_submodel_old) <- model_names
  list(predictions_submodel = predictions_submodel,
       predictions_submodel_old = predictions_submodel_old)
}
#' Evaluate orthogonal neural additive model
#' @param model model of class `onam` as returned from [fit_onam] to be
#' evaluated
#' @param data Data for which the model is to be evaluated. Default is the data
#' with which \code{model} was fitted.
#' @returns Returns a list containing data, model output for each observation in
#' `data` and main and interaction effects obtained by the model
#' @method predict onam
#' @export
predict.onam <- function(model,
                         data = model$data) {
  if (is.null(data))
    data <- model$data
  model_info <- model$model_info
  n <- nrow(data)
  n_ensemble <- length(model$ensemble)
  categorical_features <- model$categorical_features
  predictions_separate <-
    lapply(
      model$ensemble,
      evaluate_onam_single,
      data = data,
      model_info = model_info,
      categorical_features = categorical_features
    )
  effect_names <- names(predictions_separate[[1]][[1]])
  n_effects <- length(effect_names)
  data_predictions <-
    data.frame(
      y = unlist(predictions_separate),
      effect =
        rep(rep(effect_names,
                each = n),
            2 * n_ensemble),
      onam = rep(rep(c("after", "before"),
                     each = n * n_effects),
                 n_ensemble),
      observation = rep(1:n, n_effects * 2 * n_ensemble),
      model = rep(1:n_ensemble, each = n * 2 * n_effects)
    )
  predictions_features_ensemble <-
    lapply(effect_names,
           function(effect_name) {
             data_predictions %>%
               dplyr::filter(.data$onam == "after",
                             .data$effect == effect_name) %>%
               dplyr::group_by(.data$observation) %>%
               dplyr::summarise(totaleffect = mean(.data$y)) %>%
               dplyr::select(.data$totaleffect) %>% unlist()
           })
  names(predictions_features_ensemble) <-
    effect_names
  predictions_features <-
    unlist(predictions_features_ensemble) %>%
    matrix(nrow = n) %*%
    model$w_post_ensemble
  colnames(predictions_features) <-
    effect_names
  predictions_total <-
    data_predictions %>%
    dplyr::filter(.data$onam == "after") %>%
    dplyr::group_by(.data$observation) %>%
    dplyr::summarise(prediction = sum(.data$y) / n_ensemble) %>%
    dplyr::select(.data$prediction) %>% unlist()
  out <- list(
    data = data,
    predictions_total = predictions_total,
    predictions_features = predictions_features,
    model_info = model_info
  )
  class(out) <- "onam_prediction"
  out
}
evaluate_onam_pre <- function(model_list) {
  data <- model_list$data
  model_info <- model_list$model_info
  categorical_features <- model_list$categorical_features
  n <- nrow(data)
  n_ensemble <- length(model_list$ensemble)
  predictions_separate <-
    lapply(
      model_list$ensemble,
      evaluate_onam_single,
      data = data,
      model_info = model_info,
      categorical_features = categorical_features
    )
  effect_names <- names(predictions_separate[[1]][[1]])
  n_effects <- length(effect_names)
  data_predictions <-
    data.frame(
      y = unlist(predictions_separate),
      effect =
        rep(rep(effect_names,
                each = n),
            2 * n_ensemble),
      onam = rep(rep(c("after", "before"),
                     each = n * n_effects),
                 n_ensemble),
      observation = rep(1:n, n_effects * 2 * n_ensemble),
      model = rep(1:n_ensemble, each = n * 2 * n_effects)
    )
  predictions_features_ensemble <-
    lapply(effect_names,
           function(effect_name) {
             data_predictions %>%
               dplyr::filter(.data$onam == "after",
                             .data$effect == effect_name) %>%
               dplyr::group_by(.data$observation) %>%
               dplyr::summarise(totaleffect = mean(.data$y)) %>%
               dplyr::select(.data$totaleffect) %>% unlist()
           })
  names(predictions_features_ensemble) <-
    effect_names
  predictions_total <-
    data_predictions %>%
    dplyr::filter(.data$onam == "after") %>%
    dplyr::group_by(.data$observation) %>%
    dplyr::summarise(prediction = sum(.data$y) / n_ensemble) %>%
    dplyr::select(.data$prediction) %>% unlist()
  list(
    data = data,
    predictions_total = predictions_total,
    data_predictions = data_predictions,
    predictions_features_ensemble = predictions_features_ensemble
  )
}
#' Get variance decomposition of orthogonal neural additive model
#' @param object Either model of class `onam` as returned from [fit_onam] or
#' model evaluation outcome as returned from [evaluate_onam]
#' @param data Data for which the model is to be evaluated. If \code{NULL}
#' (DEFAULT), the data from model fitting is used.
#' with which \code{model} was fitted.
#' @returns Returns a named vector of percentage of variance explained by each
#' interaction order.
#' @examples
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
#'             data_train, n_ensemble = 2, epochs = 50,
#'             progresstext = TRUE, verbose = 1)
#' var_decomp_onam(mod)
#' }
#' @export var_decomp_onam
var_decomp_onam <- function(object, data = NULL) {
  if (is.null(data)) {
    if (class(object) == "onam") {
      effects <- object$outputs_post_ensemble
    } else {
      effects <- object$predictions_features
    }
  } else {
    if (class(object) == "onam") {
      effects <- evaluate_onam(object, data)$predictions_features
    } else {
      stop(
        "When calling \`var_decomp_onam\` with a non-default \`data\`
      argument, \`object\` has to be the \`onam\`-model."
      )
    }
  }
  theta_deep <-
    object$model_info$theta[setdiff(names(object$model_info$theta),
                                    "linear")]
  orders <- names(theta_deep)
  effect_order_matrix <- matrix(nrow = nrow(effects),
                                ncol = length(orders))
  sens_info = rep(0, length(orders))
  names(sens_info) <- orders
  for (idx_order in seq_along(orders)) {
    tmp_name <-
      lapply(theta_deep[[orders[idx_order]]],
             function(effect) {
               paste(effect, collapse = "_")
             }) %>%
      unlist()
    sens_info[idx_order] <- length(tmp_name)
    tmp_effects <- effects[, tmp_name]
    if (!is.null(dim(tmp_effects))) {
      tmp_effects <- rowSums(tmp_effects)
    }
    effect_order_matrix[, idx_order] <-
      tmp_effects
  }
  colnames(effect_order_matrix) <- orders
  effect_order_matrix <-
    effect_order_matrix[, ncol(effect_order_matrix):1]
  tmp_var <- stats::var(effect_order_matrix)
  sens_index <-
    colSums(stats::var(effects)) / sum(stats::var(effects))
  sens_index <-
    sens_index[length(sens_index):1]
  sens_info <- sens_info[length(sens_info):1]
  out <- list(var_decomp = diag(tmp_var) / sum(diag(tmp_var)),
              sens_index = sens_index,
              sens_info = sens_info)
}
