#' @importFrom rlang .data
evaluate_model_single <- function(model,
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
#' @param model_list model of class `onam` as returned from [fit_onam] to be
#' evaluated
#' @param data Data for which the model is to be evaluated
#' @returns Returns a list containing data, model output for each observation in
#' `data` and main and interaction effects obtained by the model
#' @export evaluate_model
evaluate_model <- function(model_list,
                           data = model_list$data) {
  if (is.null(data))
    data <- model_list$data
  model_info <- model_list$model_info
  n <- nrow(data)
  n_ensemble <- length(model_list$ensemble)
  categorical_features <- model_list$categorical_features
  predictions_separate <-
    lapply(
      model_list$ensemble,
      evaluate_model_single,
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
    model_list$w_post_ensemble
  colnames(predictions_features) <-
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
    predictions_features = predictions_features
  )
}
evaluate_model_pre <- function(model_list) {
  data <- model_list$data
  model_info <- model_list$model_info
  categorical_features <- model_list$categorical_features
  n <- nrow(data)
  n_ensemble <- length(model_list$ensemble)
  predictions_separate <-
    lapply(
      model_list$ensemble,
      evaluate_model_single,
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
