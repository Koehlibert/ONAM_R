utils::globalVariables(c("x", "y", "prediction", "x1", "x2", "y1", "y2"))
#' Plot Main Effect
#' @param object Either model of class `onam` as returned from [onam] or
#' model evaluation outcome as returned from [predict.onam]
#' @param feature Feature for which the effect is to be plotted, must be present
#' in the model formula. For interaction terms, use plotInteractionEffect
#' @param reference_level Reference level to be used when plotting categorical
#' effects for better interpretability. Effect of the reference level will be
#' set to zero and subtracted from all over factor levels.
#' @param labs An optional named vector that can contain axis labels.
#' Expected vector names are 'xlab', 'ylab' and 'effect'. If both 'ylab' and
#' 'effect' are specified, 'effect' will be used as y-axis label.
#' @returns Returns a ggplot2 object of the specified effect
#' @examplesIf reticulate::py_module_available(tensorflow)
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
#' mod1 <-  build_dnn(units = c(16, 8, 1))
#' list_of_deep_models <- list(mod1 = mod1)
#' # Fit model
#' mod <- onam(model_formula, list_of_deep_models,
#'             data_train, n_ensemble = 1, epochs = 10)
#' plot_main_effect(mod, "x1")
#' @export plot_main_effect
plot_main_effect <-
  function(object, feature, reference_level = NULL, labs = NULL) {
    check_inputs_plot(object,
                      feature,
                      reference_level = reference_level)
    data_plot <-
      data.frame(x = object$data[, feature], y = object$feature_effects[, feature])
    if (feature %in% object$model_info$categorical_features) {
      plt <- plot_main_categorical(data_plot, reference_level)
    } else {
      plt <- ggplot2::ggplot(data_plot, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point()
    }
    # Extract optional labels
    xlab_val <- if (!is.null(labs) && "xlab" %in% names(labs)) {
      labs[["xlab"]]
    } else {
      feature
    }
    ylab_val <- if (!is.null(labs) && "effect" %in% names(labs)) {
      labs[["effect"]]
    } else if (!is.null(labs) && "ylab" %in% names(labs)) {
      labs[["ylab"]]
    } else {
      eff_label_helper(object$model_info$target)
    }

    plt + ggplot2::ylab(ylab_val) +
      ggplot2::xlab(xlab_val)
  }
#' Plot Interaction Effect
#' @param object Either model of class `onam` as returned from [onam] or
#' model evaluation outcome as returned from [predict.onam]
#' @param feature1,feature2 Effects to be plotted.
#' @param interpolate If TRUE, values will be interpolated for a smooth plot.
#' If FALSE (default), only observations in the data will be plotted.
#' @param labs An optional named vector that can contain axis labels and the
#' name of the effect. Expected vector names are 'xlab', 'ylab' and 'effect'.
#' @param custom_colors color palette object for the interaction plot. Default
#' is "spectral", returning a color palette based on the spectral theme.
#' @param n_interpolate number of values per coordinate axis to interpolate.
#' Ignored if 'interpolate = FALSE'.
#' @param include_main If TRUE, main effects for features feature1 and feature2
#' will be added to the interaction term to give combined effect of main and
#' interaction effects. Default is FALSE.
#' @returns Returns a 'ggplot2' object of the specified effect interaction
#' @examplesIf reticulate::py_module_available(tensorflow)
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
#' mod1 <-  build_dnn(units = c(16, 8, 1))
#' list_of_deep_models <- list(mod1 = mod1)
#' # Fit model
#' mod <- onam(model_formula, list_of_deep_models,
#'             data_train, n_ensemble = 1, epochs = 10)
#' plot_inter_effect(mod, "x1", "x2")
#' @export plot_inter_effect
plot_inter_effect <- function(object,
                              feature1,
                              feature2,
                              interpolate = FALSE,
                              labs = NULL,
                              custom_colors = "spectral",
                              n_interpolate = 200,
                              include_main = FALSE) {
  n_cat_effs <- sum(c(feature1, feature2) %in%
                      object$model_info$categorical_features)
  if ((n_cat_effs > 0) & interpolate) {
    warning("Interaction contains categorical feature. No Interpolation will be performed.")
  }
  inter <- paste(feature1, feature2, sep = "_")
  if (!is.null(check_inputs_plot(object, inter, interaction = 1))) {
    inter <- paste(feature2, feature1, sep = "_")
    if (!is.null(check_inputs_plot(object, inter, interaction = 2))) {
      stop(paste(
        "No interaction effect fitted for ",
        feature1,
        " and ",
        feature2,
        ".",
        sep = ""
      ))
    }
    tmp <- feature1
    feature1 <- feature2
    feature2 <- tmp
  }

  # Check for lab values, falling back to defaults
  xlab_val <- if (!is.null(labs) && "xlab" %in% names(labs)) labs[["xlab"]] else feature1
  ylab_val <- if (!is.null(labs) && "ylab" %in% names(labs)) labs[["ylab"]] else feature2
  effect_val <- if (!is.null(labs) && "effect" %in% names(labs)) {
    labs[["effect"]]
  } else {
    eff_label_helper(object$model_info$target)
  }

  legend_pos <- if (nchar(effect_val) > 10) "top" else "right"
  legend_title <- if (nchar(effect_val) > 10) ggplot2::element_text(hjust = 0.5)
    else ggplot2::element_text()
  colorbar_guide <- if (nchar(effect_val) > 10) {
    ggplot2::guide_colorbar(title.position = "top")
  } else {
    ggplot2::guide_colorbar(title.position = "top", title.vjust = 1)
  }

  tmp_xlab <- ggplot2::xlab(xlab_val)
  tmp_ylab <- ggplot2::ylab(ylab_val)
  if (typeof(custom_colors) != "closure") {
    if (custom_colors == "spectral") {
      custom_colors <-
        grDevices::colorRampPalette(colors = (x = RColorBrewer::brewer.pal(n = 11, name = "Spectral")))
    }
  }
  eff <- object$feature_effects[, inter]
  if (include_main)
  {
    f1_in <- feature1 %in% colnames(object$feature_effects)
    f2_in <- feature2 %in% colnames(object$feature_effects)
    if (!all(f1_in, f2_in))
    {
      warning(
        paste0(
          "`include_main` has been set to `true`, ",
          "but not all features were included as main effects."
        ),
        call. = FALSE
      )
    }
    if (f1_in)
    {
      eff <- eff + object$feature_effects[, feature1]
    }
    if (f2_in)
    {
      eff <- eff + object$feature_effects[, feature2]
    }
  }
  if (interpolate & (n_cat_effs == 0)) {
    tmp_interp <-
      akima::interp(
        x = object$data[, feature1],
        y = object$data[, feature2],
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
        guide = colorbar_guide,
        limits = c(min(data_plot$prediction), max(data_plot$prediction)),
        name = effect_val
      )
    geom_param <- ggplot2::geom_tile()
    aes_param <- ggplot2::aes(x = x, y = y, fill = prediction)
  } else {
    data_plot <-
      data.frame(x = object$data[, feature1],
                 y = object$data[, feature2],
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
        guide = colorbar_guide,
        limits = c(min(data_plot$prediction), max(data_plot$prediction)),
        name = effect_val
      )
    geom_param <- ggplot2::geom_point()
    prediction <- NULL #remove cmd check note
    if (n_cat_effs == 1) {
      data_plot <- data_plot %>%
        dplyr::group_by(x, y) %>%
        dplyr::summarise(prediction = mean(prediction), .groups = "drop")
      tmp_ylab <-
        ggplot2::ylab(effect_val)
      if (feature1 %in% object$model_info$categorical_features) {
        aes_gradient <- ggplot2::scale_color_discrete(name = feature1)
        data_plot$x <- as.factor(data_plot$x)
        aes_param <- ggplot2::aes(x = y,
                                  y = prediction,
                                  color = x)
      } else {
        aes_gradient <- ggplot2::scale_color_discrete(name = feature2)
        data_plot$y <- as.factor(data_plot$y)
        tmp_xlab <- ggplot2::xlab(feature1)
        aes_param <- ggplot2::aes(x = x,
                                  y = prediction,
                                  color = y)
      }
    } else {
      aes_param <- ggplot2::aes(x = x, y = y, color = prediction)
    }
  }
  inter_theme <- ggplot2::theme(
    plot.title.position = "plot",
    plot.caption.position =  "plot",
    # plot.margin = grid::unit(c(0,0,0,0), "mm"),
    panel.grid = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    legend.position = legend_pos,
    legend.title = legend_title,
    legend.box.spacing = ggplot2::unit(0.5, "pt")
  )
  ggplot2::ggplot(data_plot, aes_param) +
    # geom_point(size = 0.75) +
    geom_param +
    aes_gradient +
    inter_theme +
    tmp_xlab + tmp_ylab
}
eff_label_helper <- function(target) {
  if (target == "continuous") {
    "Effect"
  } else if (target == "binary") {
    "Effect on logit scale"
  }
}
plot_main_categorical <-
  function(data_plot, reference_level = NULL) {
    x_uq <- unique(data_plot$x)
    first_x <-
      sapply(x_uq, function(x_val)
        min(which(data_plot$x == x_val)))
    y_uq <- data_plot$y[first_x]
    data_plot_categ <-
      data.frame(x = x_uq, y = y_uq)
    if (!is.null(reference_level))
    {
      x_ref_idx <- which(data_plot_categ$x == reference_level)
      data_plot_categ$y <- data_plot_categ$y -
        data_plot_categ$y[x_ref_idx]
      data_plot_categ$x[x_ref_idx] <-
        paste0(reference_level, " (Reference)")
    }
    ggplot2::ggplot(data_plot_categ, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::geom_text(ggplot2::aes(
        y = 0,
        label = x,
        vjust = 0.5 + 0.75 * sign(y)
      )) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      )
  }
