#' Get variance decomposition of orthogonal neural additive model
#' @param object Either model of class `onam` as returned from [onam] or
#' model evaluation outcome as returned from [predict.onam]
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
#' decompose(mod)
#' }
#' @export decompose
decompose <- function(object, data = NULL) {
  if (is.null(data)) {
    if (inherits(object, "onam")) {
      effects <- object$outputs_post_ensemble
    } else {
      effects <- object$predictions_features
    }
  } else {
    if (inherits(object, "onam")) {
      effects <- predict(object, data)$predictions_features
    } else {
      stop(
        "When calling \`decompose\` with a non-default \`data\`
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
  sens_info <- sens_info[length(sens_info):1]
  out <- list(var_decomp = diag(tmp_var) / sum(diag(tmp_var)))
  attr(out, "sens_info") <- sens_info
  class(out) <- "var_decomp"
  out
}
#' @method print var_decomp
#' @export
print.var_decomp <- function(x, ...) {
  cat("\nFraction of total variance explained per order:")
  for (i in seq_along(x$var_decomp)) {
    if (i == 1) {
      cat("\nMain effects:", x$var_decomp[i])
    } else {
      cat("\nInteractions of order ",
          names(x$var_decomp)[i],
          ": ",
          round(x$var_decomp[i], 4))
    }
  }
  # cat("\nGeneralized sensitivity indices for each effect:")
  # for (i in seq_along(x$sens_index)) {
  #   cat("\n", names(x$sens_index)[i],
  #       ": ", round(x$sens_index[i], 5))
  # }
  invisible(x)
}
