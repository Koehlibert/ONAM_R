#' Get variance decomposition of orthogonal neural additive model
#' @param object Either model of class `onam` as returned from [onam] or
#' model evaluation outcome as returned from [predict.onam]
#' @param data Data for which the model is to be evaluated. If \code{NULL}
#' (DEFAULT), the data from model fitting is used.
#' @returns Returns a named vector of percentage of variance explained by each
#' interaction order.
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
#' mod1 <- function(inputs) {
#'   outputs <- inputs %>%
#'     layer_dense(units = 16, activation = "relu") %>%
#'     layer_dense(units = 8, activation = "linear",
#'                 use_bias = TRUE) %>%
#'     layer_dense(units = 1, activation = "linear",
#'                 use_bias = TRUE)
#'   keras_model(inputs, outputs)
#' }
#' list_of_deep_models <- list(mod1 = mod1)
#' # Fit model
#' mod <- onam(model_formula, list_of_deep_models,
#'             data_train, n_ensemble = 1, epochs = 10)
#' decompose(mod)
#' }
#' @export decompose
decompose <- function(object, data = NULL) {
  if (!require_keras()) {
    invisible(return(NULL))
  }
  if (is.null(data)) {
    effects <- object$feature_effects
  } else {
    if (inherits(object, "onam")) {
      effects <- predict(object, data)$feature_effects
    } else {
      stop(
        "When calling \`decompose\` with a non-default \`data\`
      argument, \`object\` has to be the \`onam\`-model.",
        call. = FALSE
      )
    }
  }
  theta_deep <-
    object$model_info$theta[setdiff(names(object$model_info$theta), "linear")]
  max_order <- max(as.numeric(names(theta_deep)))
  orders <- sort(unique(c(1, 2, as.numeric(
    names(theta_deep)
  ))))
  effect_order_matrix <- matrix(0, nrow = nrow(effects), ncol = length(orders))
  sens_info = rep(0, length(orders))
  names(sens_info) <- orders
  for (idx_order in seq_along(orders)) {
    tmp_name <-
      get_effect_names_for_order(theta_deep[[as.character(orders[idx_order])]],
                                 max_order)
    sens_info[idx_order] <- length(tmp_name)
    tmp_effects <- effects[, tmp_name]
    if (!is.null(dim(tmp_effects))) {
      tmp_effects <- rowSums(tmp_effects)
    }
    effect_order_matrix[, idx_order] <-
      tmp_effects
  }
  # effect_order_matrix <-
  #   matrix(effect_order_matrix[, ncol(effect_order_matrix):1])
  colnames(effect_order_matrix) <- orders
  tmp_var <- stats::var(effect_order_matrix)
  sens_info <- sens_info[length(sens_info):1]
  out <- list(var_decomp = diag(tmp_var) / sum(diag(tmp_var)))
  attr(out, "sens_info") <- sens_info
  attr(out, "target") <- object$model_info$target
  class(out) <- "var_decomp"
  out
}
#' @method print var_decomp
#' @export
print.var_decomp <- function(x, ...) {
  out_txt <- "\nFraction of total variance explained per order"
  if (attr(x, "target") == "binary") {
    out_txt <- paste0(out_txt, " (on logit level)")
  }
  out_txt <- paste0(out_txt, ":")
  cat(out_txt)
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
#' Plot variance decomposition
#' @param x Object of class `var_decomp` as returned by [decompose]
#' @param ... further arguments, currently unused
#' @returns Returns a 'ggplot2' object showing the fraction of total
#' variance explained by each interaction order
#' @method plot var_decomp
#' @export
plot.var_decomp <- function(x, ...) {
  orders <- names(x$var_decomp)
  order_labels <- ifelse(orders == "1",
                         "Main effects",
                         paste0("Order ", orders, " interactions"))
  data_plot <- data.frame(
    order = factor(order_labels, levels = order_labels),
    variance = as.numeric(x$var_decomp)
  )
  ylab_val <- "Fraction of explained variance"
  if (attr(x, "target") == "binary") {
    ylab_val <- paste0(ylab_val, " (logit scale)")
  }
  ggplot2::ggplot(data_plot,
                  ggplot2::aes(x = .data$order, y = .data$variance)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(.data$variance, accuracy = 0.1)),
      vjust = -0.3
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent,
                                expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(ylab_val) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
}
#' Compute Generalized Sobol Indices for dependent features
#' @param object Either model of class `onam` as returned from [onam] or
#' evaluation outcome as returned from [predict.onam].
#' @param data Data for which the model is to be evaluated. If \code{NULL}
#' (DEFAULT), the data from model fitting is used.
#' @returns Returns a named vector of percentage of variance explained by each
#' interaction order.
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
#' mod1 <- function(inputs) {
#'   outputs <- inputs %>%
#'     layer_dense(units = 16, activation = "relu") %>%
#'     layer_dense(units = 8, activation = "linear",
#'                 use_bias = TRUE) %>%
#'     layer_dense(units = 1, activation = "linear",
#'                 use_bias = TRUE)
#'   keras_model(inputs, outputs)
#' }
#' list_of_deep_models <- list(mod1 = mod1)
#' # Fit model
#' mod <- onam(model_formula, list_of_deep_models,
#'             data_train, n_ensemble = 1, epochs = 10)
#' gen_sobol(mod)
#' }
#' @details For details on generalized sobol indices, see
#' Chastaing et al. (2012) \doi{10.1214/12-EJS749}.
#' @export gen_sobol
gen_sobol <- function(object, data = NULL) {
  if (!require_keras()) {
    invisible(return(NULL))
  }
  if (inherits(object, "onam") |
      inherits(object, "onam_prediction"))
  {
    tmp_var <- stats::var(object$feature_effects)
  } else
  {
    stop(
      "`gen_sobol`` can only be called on objects of class
       `onam` and `onam_prediction`.",
      call. = FALSE
    )
  }
  total_var <- sum(tmp_var)
  gen_sobol_index_1 <- diag(tmp_var)
  gen_sobol_index_2 <- (rowSums(tmp_var) - gen_sobol_index_1) / total_var
  gen_sobol_index_1 <- gen_sobol_index_1 / total_var
  total_sobol_index <- gen_sobol_index_1 + gen_sobol_index_2
  effect_orders <- sapply(seq_along(object$model_info$theta), function(i)
    rep(
      names(object$model_info$theta)[i],
      length(object$model_info$theta[[i]])
    )) %>% unlist() %>% as.numeric()
  out_df <-
    data.frame(
      effect_order = effect_orders,
      gen_sobol_index_1 = gen_sobol_index_1,
      gen_sobol_index_2 = gen_sobol_index_2,
      total_sobol_index = total_sobol_index
    )
  out <- list(
    out_df = out_df,
    gen_sobol_index_1 = gen_sobol_index_1,
    gen_sobol_index_2 = gen_sobol_index_2,
    total_sobol_index = total_sobol_index
  )
  attr(out, "target") <- object$model_info$target
  class(out) <- "gen_sobol"
  out
}
#' @method print gen_sobol
#' @export
print.gen_sobol <- function(x, ...) {
  out_txt <- "\nGeneralized Sobol Indices"
  if (attr(x, "target") == "binary") {
    out_txt <- paste0(out_txt, " (on logit level)")
  }
  out_txt <- paste0(out_txt, ":")
  cat(out_txt)
  tmp <- x$out_df %>% dplyr::arrange(.data$effect_order)
  for (i in unique(tmp$effect_order)) {
    sub_tmp <- tmp %>% dplyr::filter(.data$effect_order == i) %>% as.matrix()
    if (i == 1) {
      cat("\nMain effects:")
    } else {
      cat("\nInteractions of order ", i, ": ")
    }
    for (j in seq(nrow(sub_tmp)))
    {
      cat("\n ", rownames(sub_tmp)[j], sub_tmp[j, "total_sobol_index"])
    }
  }
  invisible(x)
}
#' Plot generalized Sobol indices
#' @param x Object of class `gen_sobol` as returned by [gen_sobol]
#' @param ... further arguments, currently unused
#' @returns Returns a 'ggplot2' object showing, for each fitted effect, the
#' generalized Sobol index split into the effect's own contribution
#' (`gen_sobol_index_1`) and its contribution through interactions with
#' other effects of the same order (`gen_sobol_index_2`)
#' @details For details on generalized sobol indices, see
#' Chastaing et al. (2012) \doi{10.1214/12-EJS749}.
#' @method plot gen_sobol
#' @export
plot.gen_sobol <- function(x, ...) {
  data_plot <- x$out_df
  data_plot$effect <- rownames(data_plot)
  data_plot <-
    data_plot[order(data_plot$effect_order, -data_plot$total_sobol_index), ]
  data_plot$effect <- factor(data_plot$effect, levels = data_plot$effect)
  data_plot_long <- data.frame(
    effect = rep(data_plot$effect, 2),
    component = rep(c("Effect contribution",
                      "Effect induced by other terms of the same order"),
                    each = nrow(data_plot)),
    value = c(data_plot$gen_sobol_index_1, data_plot$gen_sobol_index_2)
  )
  ylab_val <- "Generalized Sobol Index"
  if (attr(x, "target") == "binary") {
    ylab_val <- paste0(ylab_val, " (logit scale)")
  }
  ggplot2::ggplot(data_plot_long,
                  ggplot2::aes(x = .data$effect, y = .data$value,
                               fill = .data$component)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(ylab_val) +
    ggplot2::labs(fill = NULL) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "top"
    )
}
