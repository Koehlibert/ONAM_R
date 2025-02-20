utils::globalVariables(c("x", "y", "prediction")) #this is objectively bad and making my code more prone to errors and less easy to debug just to remove stupid check warnings/notes
#' Plot Main Effect
#' @param data_eval Model output as obtained from ONAM::evaluate_model
#' @param effect Effect to be plotted, must be present in the model formula. For interaction terms, use plotInteractionEffect
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
#' mod <- fit_onam(model_formula, list_of_deep_models,
#'                    data_train, n_ensemble = 2,
#'                    progresstext = TRUE, verbose = 1)
#' data_eval <- evaluate_model(mod)
#' plot_main_effect(data_eval, "x1")
#' }
#' @export plot_main_effect
plot_main_effect <- function(data_eval, effect)
{
  if(!effect %in% colnames(data_eval$predictions_features))
  {
    stop(paste(effect,
               " is not present in the fitted model effects.",
               sep = ""))
  }
  data_plot <-
    data.frame(x = data_eval$data[,effect],
               y = data_eval$predictions_features[,effect])
  out_plot <- ggplot2::ggplot(data_plot, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point() + ggplot2::ylab("Effect") +
    ggplot2::xlab(effect)
  return(out_plot)
}
#' Plot Interaction Effect
#' @param data_eval Model output as obtained from ONAM::evaluate_model
#' @param effect1 First effect to be plotted
#' @param effect2 Second effect to be plotted
#' @param interpolate If TRUE, values will be interpolated for a smooth plot. If FALSE (default), only observations in the data will be plotted.
#' @param custom_colors color palette object for the interaction plot. Default is "spectral", returning a color palette based on the spectral theme.
#' @param n_interpolate number of values per coordinate axis to interpolate. Ignored if 'interpolate = FALSE'.
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
#' mod <- fit_onam(model_formula, list_of_deep_models,
#'                    data_train, n_ensemble = 2,
#'                    progresstext = TRUE, verbose = 1)
#' data_eval <- evaluate_model(mod)
#' plot_inter_effect(data_eval, "x1", "x2", interpolate = TRUE)
#' }
#' @export plot_inter_effect
plot_inter_effect <- function(data_eval, effect1, effect2,
                            interpolate = FALSE,
                            custom_colors = "spectral",
                            n_interpolate = 200)
{
  inter <- paste(effect1, effect2, sep = "_")
  if(!inter %in% colnames(data_eval$predictions_features))
  {
    inter <- paste(effect2, effect1, sep = "_")
    tmp <- effect1
    effect1 <- effect2
    effect2 <- tmp
    if(!inter %in% colnames(data_eval$predictions_features))
    {
      stop(paste("No interaction effect fitted for ",
                 effect1, " and ", effect2,".", sep = ""))
    }
  }
  if(typeof(custom_colors) != "closure")
  {
    if(custom_colors == "spectral")
    {
      custom_colors <-
        grDevices::colorRampPalette(colors = (x = RColorBrewer::brewer.pal(n = 11, name = "Spectral")))
    }
  }
  if(interpolate)
  {
    tmp_interp <-
      akima::interp(x = data_eval$data[,effect1],
                    y = data_eval$data[,effect2],
                    z =
                      data_eval$predictions_features[,inter],
                    nx = n_interpolate, ny = n_interpolate,
                    duplicate = "mean")
    data_plot <-
      data.frame(x = rep(tmp_interp$x, length(tmp_interp$y)),
                 y = rep(tmp_interp$y, each = length(tmp_interp$x)),
                 prediction = tmp_interp$z %>% c())
    data_plot <- data_plot[which(!is.na(data_plot$prediction)),]
    aes_gradient <-
      ggplot2::scale_fill_gradientn(colors =
                                      custom_colors(n = 100),
                                    values =
                                      scales::rescale(c(min(data_plot$prediction),
                                                        mean(data_plot$prediction),
                                                        max(data_plot$prediction))),
                                    guide = "colorbar",
                                    limits = c(min(data_plot$prediction),
                                               max(data_plot$prediction)))
    geom_param <- ggplot2::geom_tile()
    aes_param <- ggplot2::aes(x = x, y = y, fill = prediction)
  }else
  {
    data_plot <-
      data.frame(x = data_eval$data[,effect1],
                 y = data_eval$data[,effect2],
                 prediction =
                   data_eval$predictions_features[,inter])
    aes_gradient <-
      ggplot2::scale_color_gradientn(colors =
                              custom_colors(n = 100),
                            values =
                              scales::rescale(c(min(data_plot$prediction),
                                                mean(data_plot$prediction),
                                                max(data_plot$prediction))),
                            guide = "colorbar",
                            limits = c(min(data_plot$prediction),
                                       max(data_plot$prediction)))
    geom_param <- ggplot2::geom_point()
    prediction <- NULL#this is objectively bad and making my code more prone to errors and less easy to debug just to remove stupid check warnings/notes
    aes_param <- ggplot2::aes(x = x, y = y, color = prediction)
  }
  inter_theme <- ggplot2::theme(plot.title.position = "plot",
                       plot.caption.position =  "plot",
                       # plot.margin = grid::unit(c(0,0,0,0), "mm"),
                       panel.grid = ggplot2::element_blank(),
                       panel.background = ggplot2::element_blank(),
                       legend.position = "right")
  out_plot <-
    ggplot2::ggplot(data_plot, aes_param) +
    # geom_point(size = 0.75) +
    geom_param +
    aes_gradient +
    inter_theme +
    ggplot2::ylab(effect1) + ggplot2::xlab(effect2)
  return(out_plot)
}
