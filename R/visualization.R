utils::globalVariables(c("x", "y", "prediction", "x1", "x2", "y1", "y2"))
#' Plot Main Effect
#' @param object Either model of class `onam` as returned from [onam] or
#' model evaluation outcome as returned from [predict.onam]
#' @param effect Effect to be plotted, must be present in the model formula.
#' For interaction terms, use plotInteractionEffect
#' @returns Returns a ggplot2 object of the specified effect
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
#' plot_main_effect(mod, "x1")
#' }
#' @export plot_main_effect
plot_main_effect <- function(object, effect) {
  check_inputs(object, effect)
  if (inherits(object, "onam_prediction")) {
    data_plot <-
      data.frame(x = object$data[, effect],
                 y = object$predictions_features[, effect])
  } else if (inherits(object, "onam")) {
    data_plot <-
      data.frame(x = object$data[, effect],
                 y = object$outputs_post_ensemble[, effect])
  }

  ggplot2::ggplot(data_plot, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point() + ggplot2::ylab("Effect") +
    ggplot2::xlab(effect)
}
#' Plot Interaction Effect
#' @param object Either model of class `onam` as returned from [onam] or
#' model evaluation outcome as returned from [predict.onam]
#' @param effect1 First effect to be plotted.
#' @param effect2 Second effect to be plotted.
#' @param interpolate If TRUE, values will be interpolated for a smooth plot.
#' If FALSE (default), only observations in the data will be plotted.
#' @param custom_colors color palette object for the interaction plot. Default
#' is "spectral", returning a color palette based on the spectral theme.
#' @param n_interpolate number of values per coordinate axis to interpolate.
#' Ignored if 'interpolate = FALSE'.
#' @returns Returns a 'ggplot2' object of the specified effect interaction
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
#' plot_inter_effect(mod, "x1", "x2", interpolate = TRUE)
#' }
#' @export plot_inter_effect
plot_inter_effect <- function(object,
                              effect1,
                              effect2,
                              interpolate = FALSE,
                              custom_colors = "spectral",
                              n_interpolate = 200) {
  inter <- paste(effect1, effect2, sep = "_")
  if (!is.null(check_inputs(object, inter, interaction = 1))) {
    inter <- paste(effect2, effect1, sep = "_")
    if (check_inputs(object, inter, interaction = 2) == 2) {
      stop(paste(
        "No interaction effect fitted for ",
        effect1,
        " and ",
        effect2,
        ".",
        sep = ""
      ))
    }
    tmp <- effect1
    effect1 <- effect2
    effect2 <- tmp
  }
  if (typeof(custom_colors) != "closure") {
    if (custom_colors == "spectral") {
      custom_colors <-
        grDevices::colorRampPalette(colors = (x = RColorBrewer::brewer.pal(n = 11, name = "Spectral")))
    }
  }
  if (inherits(object, "onam_prediction")) {
    eff <- object$predictions_features[, inter]
  } else if (inherits(object, "onam")) {
    eff <- object$outputs_post_ensemble[, inter]
  }
  if (interpolate) {
    tmp_interp <-
      akima::interp(
        x = object$data[, effect1],
        y = object$data[, effect2],
        z = eff,
        nx = n_interpolate,
        ny = n_interpolate,
        duplicate = "mean"
      )
    data_plot <-
      data.frame(
        x = rep(tmp_interp$x, length(tmp_interp$y)),
        y = rep(tmp_interp$y, each = length(tmp_interp$x)),
        prediction = tmp_interp$z %>% c()
      )
    data_plot <- data_plot[which(!is.na(data_plot$prediction)), ]
    aes_gradient <-
      ggplot2::scale_fill_gradientn(
        colors =
          custom_colors(n = 100),
        values =
          scales::rescale(c(
            min(data_plot$prediction),
            mean(data_plot$prediction),
            max(data_plot$prediction)
          )),
        guide = "colorbar",
        limits = c(min(data_plot$prediction),
                   max(data_plot$prediction)),
        name = "Effect"
      )
    geom_param <- ggplot2::geom_tile()
    aes_param <- ggplot2::aes(x = x, y = y, fill = prediction)
  } else {
    data_plot <-
      data.frame(x = object$data[, effect1],
                 y = object$data[, effect2],
                 prediction = eff)
    aes_gradient <-
      ggplot2::scale_color_gradientn(
        colors =
          custom_colors(n = 100),
        values =
          scales::rescale(c(
            min(data_plot$prediction),
            mean(data_plot$prediction),
            max(data_plot$prediction)
          )),
        guide = "colorbar",
        limits = c(min(data_plot$prediction),
                   max(data_plot$prediction)),
        name = "Effect"
      )
    geom_param <- ggplot2::geom_point()
    prediction <- NULL #remove cmd check note
    aes_param <- ggplot2::aes(x = x, y = y, color = prediction)
  }
  inter_theme <- ggplot2::theme(
    plot.title.position = "plot",
    plot.caption.position =  "plot",
    # plot.margin = grid::unit(c(0,0,0,0), "mm"),
    panel.grid = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    legend.position = "right"
  )
  ggplot2::ggplot(data_plot, aes_param) +
    # geom_point(size = 0.75) +
    geom_param +
    aes_gradient +
    inter_theme +
    ggplot2::ylab(effect1) + ggplot2::xlab(effect2)
}
#' Visualize variance decomposition
#' @param model Model of class `onam` as returned from [onam]
#' @returns Returns a 'ggplot2' object showing the variance decomposition into
#' effects of interaction orders along with generalized sensitivity indices
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
#' plot_decomp(mod)
#' }
#' @export plot_decomp
plot_decomp <- function(model) {
  tmp_var_decomp <- decompose(model)
  sens <- tmp_var_decomp$sens_index
  sens_info <- attr(tmp_var_decomp, "sens_info")
  plot_data_sens <-
    data.frame(x = 0:(length(sens) - 1),
               y1 = sens,
               y2 = cumsum(sens))
  plot_data_var_decomp <-
    data.frame(
      x1 = c(-0.5, cumsum(sens_info)[1:(length(sens_info) - 1)] - 0.5),
      x2 = c(cumsum(sens_info)[1:(length(sens_info) - 1)] - 0.5, sum(sens_info) - 0.5),
      y1 = rep(0, length(sens_info)),
      y2 = tmp_var_decomp$var_decomp,
      order = names(sens_info)
    )
  ggplot2::ggplot(plot_data_sens,
                  ggplot2::aes(x = x, y = y1)) +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(x = x, y = y2), color = "blue") +
    ggplot2::geom_rect(
      mapping = ggplot2::aes(
        xmin = x1,
        xmax = x2,
        ymin = y1,
        ymax = y2,
        fill = order
      ),
      data = plot_data_var_decomp,
      alpha = 0.5,
      inherit.aes = FALSE,
    ) +
    ggplot2::ylab("Generalized Sensitivity Index") +
    ggplot2::scale_fill_discrete(name = "Order") +
    ggplot2::scale_x_continuous(breaks = plot_data_sens$x,
                                labels = names(sens)) +
    ggplot2::xlab("") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ))
}
#' @keywords internal
check_inputs <- function(object, effect, interaction = 0) {
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
